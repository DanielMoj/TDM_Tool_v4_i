#' FHIR Connection Manager mit Circuit Breaker Pattern
#' Verhindert Überlastung des FHIR-Servers und handled Ausfälle intelligent
#'
#' @description
#' Implementiert das Circuit Breaker Pattern für resiliente FHIR-Kommunikation.
#' Der Circuit Breaker hat drei Zustände:
#' - closed: Normal operation, requests werden durchgelassen
#' - open: Zu viele Fehler, requests werden blockiert
#' - half_open: Testphase nach Wartezeit, limitierte requests

# Circuit Breaker State Management
.fhir_circuit <- new.env(parent = emptyenv())
.fhir_circuit$state <- "closed"  # closed, open, half_open
.fhir_circuit$failures <- 0
.fhir_circuit$last_failure_time <- NULL
.fhir_circuit$success_count <- 0
.fhir_circuit$config <- list(
  failure_threshold = 5,     # Failures bis Circuit öffnet
  timeout_seconds = 60,       # Wartezeit bis half_open
  success_threshold = 3,      # Erfolge in half_open bis closed
  request_timeout = 30        # Standard request timeout
)

#' Execute FHIR request with circuit breaker
#' 
#' @param request_fn Function that performs the actual HTTP request
#' @param ... Parameters for request_fn
#' @param max_retries Maximum retry attempts (default: 3)
#' @param timeout Request timeout in seconds (default: 30)
#' @param retry_on_401 Whether to retry on 401 with token refresh (default: TRUE)
#' @return Response object or NULL on failure
#' @export
fhir_request_with_circuit_breaker <- function(request_fn, ..., 
                                             max_retries = 3, 
                                             timeout = 30,
                                             retry_on_401 = TRUE) {
  
  # Validate inputs
  if (!is.function(request_fn)) {
    stop("request_fn must be a function")
  }
  
  # Check circuit breaker state
  circuit_status <- check_circuit_state()
  
  if (circuit_status == "blocked") {
    time_remaining <- .fhir_circuit$config$timeout_seconds - 
                     as.numeric(difftime(Sys.time(), .fhir_circuit$last_failure_time, units = "secs"))
    
    warning(sprintf("Circuit breaker OPEN: FHIR server unavailable. Retry in %d seconds.",
                   max(0, ceiling(time_remaining))))
    
    # Log circuit breaker block
    log_circuit_event("blocked", list(
      time_remaining = time_remaining,
      failures = .fhir_circuit$failures
    ))
    
    return(NULL)
  }
  
  # Execute request with retry logic
  result <- execute_with_retry(
    request_fn = request_fn,
    args = list(...),
    max_retries = max_retries,
    timeout = timeout,
    retry_on_401 = retry_on_401
  )
  
  # Update circuit breaker based on result
  update_circuit_state(result$success)
  
  return(result$response)
}

#' Check current circuit breaker state
#' @return "allowed", "blocked", or "testing"
check_circuit_state <- function() {
  if (.fhir_circuit$state == "open") {
    time_since_failure <- difftime(Sys.time(), .fhir_circuit$last_failure_time, units = "secs")
    
    if (time_since_failure > .fhir_circuit$config$timeout_seconds) {
      # Transition to half-open for testing
      .fhir_circuit$state <- "half_open"
      .fhir_circuit$success_count <- 0
      message("Circuit breaker: Attempting recovery (half-open)")
      return("testing")
    }
    
    return("blocked")
  }
  
  return("allowed")
}

#' Execute request with retry logic and exponential backoff
execute_with_retry <- function(request_fn, args, max_retries, timeout, retry_on_401) {
  attempt <- 1
  wait_time <- 1  # Initial wait time in seconds
  last_error <- NULL
  
  while (attempt <= max_retries) {
    # Log attempt if not first
    if (attempt > 1) {
      message(sprintf("FHIR request attempt %d/%d", attempt, max_retries))
    }
    
    # Execute request with timeout
    result <- tryCatch({
      response <- httr::with_config(
        httr::timeout(timeout),
        do.call(request_fn, args)
      )
      
      # Check for HTTP errors
      if (httr::http_error(response)) {
        handle_http_error(response, retry_on_401)
      } else {
        list(success = TRUE, response = response)
      }
      
    }, error = function(e) {
      last_error <<- e$message
      
      # Log network error
      log_network_error(e$message, attempt)
      
      # Determine if retry is appropriate
      if (should_retry_error(e$message)) {
        list(success = FALSE, retry = TRUE, error = e$message)
      } else {
        list(success = FALSE, retry = FALSE, error = e$message)
      }
    })
    
    # Handle successful response
    if (!is.null(result$success) && result$success) {
      return(result)
    }
    
    # Check if we should retry
    if (!is.null(result$retry) && !result$retry) {
      warning(sprintf("Non-retryable error: %s", result$error))
      return(list(success = FALSE, response = NULL))
    }
    
    # Handle specific retry scenarios
    if (!is.null(result$wait_time)) {
      wait_time <- result$wait_time
    } else if (attempt < max_retries) {
      # Exponential backoff with jitter
      wait_time <- min(wait_time * 2 + runif(1, 0, 1), 30)  # Cap at 30 seconds
    }
    
    # Wait before retry
    if (attempt < max_retries) {
      message(sprintf("Waiting %.1f seconds before retry...", wait_time))
      Sys.sleep(wait_time)
    }
    
    attempt <- attempt + 1
  }
  
  # All retries exhausted
  warning(sprintf("FHIR request failed after %d attempts. Last error: %s",
                 max_retries, last_error))
  
  # Audit log for failed request
  audit_event("fhir_request_failed", list(
    endpoint = deparse(substitute(request_fn)),
    attempts = max_retries,
    last_error = last_error,
    circuit_state = .fhir_circuit$state
  ))
  
  return(list(success = FALSE, response = NULL))
}

#' Handle HTTP error responses
handle_http_error <- function(response, retry_on_401) {
  status_code <- httr::status_code(response)
  
  if (status_code == 401 && retry_on_401) {
    # Token expired - attempt refresh
    message("FHIR: 401 Unauthorized - attempting token refresh")
    
    # Load auth functions if available
    if (exists("fhir_refresh_token") && is.function(fhir_refresh_token)) {
      if (fhir_refresh_token()) {
        return(list(success = FALSE, retry = TRUE))
      }
    }
    
    return(list(success = FALSE, retry = FALSE, error = "Token refresh failed"))
    
  } else if (status_code == 429) {
    # Rate limited - extract retry-after header
    retry_after <- httr::headers(response)$`retry-after`
    wait_time <- ifelse(is.null(retry_after), 5, as.numeric(retry_after))
    
    warning(sprintf("FHIR: Rate limited (429). Waiting %d seconds.", wait_time))
    
    return(list(success = FALSE, retry = TRUE, wait_time = wait_time))
    
  } else if (status_code >= 500) {
    # Server error - worth retrying
    error_msg <- sprintf("FHIR server error: %d %s", 
                        status_code, 
                        httr::http_status(response)$message)
    
    return(list(success = FALSE, retry = TRUE, error = error_msg))
    
  } else {
    # Client error - don't retry
    error_msg <- sprintf("FHIR client error: %d %s", 
                        status_code,
                        httr::http_status(response)$message)
    
    return(list(success = FALSE, retry = FALSE, error = error_msg))
  }
}

#' Update circuit breaker state based on request result
update_circuit_state <- function(success) {
  if (success) {
    handle_success()
  } else {
    handle_failure()
  }
}

#' Handle successful request
handle_success <- function() {
  if (.fhir_circuit$state == "half_open") {
    .fhir_circuit$success_count <- .fhir_circuit$success_count + 1
    
    if (.fhir_circuit$success_count >= .fhir_circuit$config$success_threshold) {
      # Recovery successful
      .fhir_circuit$state <- "closed"
      .fhir_circuit$failures <- 0
      .fhir_circuit$success_count <- 0
      message("Circuit breaker: Recovery successful (closed)")
      
      log_circuit_event("recovered", list(
        success_count = .fhir_circuit$config$success_threshold
      ))
    }
  } else if (.fhir_circuit$state == "closed") {
    # Reset failure count on success
    .fhir_circuit$failures <- 0
  }
}

#' Handle failed request
handle_failure <- function() {
  .fhir_circuit$failures <- .fhir_circuit$failures + 1
  .fhir_circuit$last_failure_time <- Sys.time()
  
  if (.fhir_circuit$state == "half_open") {
    # Failed during recovery - reopen immediately
    .fhir_circuit$state <- "open"
    .fhir_circuit$success_count <- 0
    warning("Circuit breaker: Recovery failed, reopening")
    
    log_circuit_event("recovery_failed", list(
      failures = .fhir_circuit$failures
    ))
    
  } else if (.fhir_circuit$failures >= .fhir_circuit$config$failure_threshold) {
    # Too many failures - open circuit
    .fhir_circuit$state <- "open"
    warning(sprintf("Circuit breaker: Opening after %d failures", .fhir_circuit$failures))
    
    log_circuit_event("opened", list(
      failures = .fhir_circuit$failures,
      threshold = .fhir_circuit$config$failure_threshold
    ))
  }
}

#' Check if an error should trigger retry
should_retry_error <- function(error_message) {
  retry_patterns <- c(
    "timeout",
    "connection refused",
    "could not resolve host",
    "network is unreachable",
    "connection reset",
    "temporary failure"
  )
  
  any(sapply(retry_patterns, function(pattern) {
    grepl(pattern, error_message, ignore.case = TRUE)
  }))
}

#' Get circuit breaker status
#' @return List with circuit breaker state information
#' @export
get_fhir_circuit_status <- function() {
  status <- list(
    state = .fhir_circuit$state,
    failures = .fhir_circuit$failures,
    last_failure = .fhir_circuit$last_failure_time,
    available = .fhir_circuit$state != "open",
    config = .fhir_circuit$config
  )
  
  # Add time until recovery if circuit is open
  if (.fhir_circuit$state == "open" && !is.null(.fhir_circuit$last_failure_time)) {
    time_since_failure <- difftime(Sys.time(), .fhir_circuit$last_failure_time, units = "secs")
    status$recovery_in <- max(0, .fhir_circuit$config$timeout_seconds - as.numeric(time_since_failure))
  }
  
  return(status)
}

#' Reset circuit breaker (for admin use)
#' @param reason Optional reason for reset
#' @export
reset_fhir_circuit <- function(reason = NULL) {
  .fhir_circuit$state <- "closed"
  .fhir_circuit$failures <- 0
  .fhir_circuit$last_failure_time <- NULL
  .fhir_circuit$success_count <- 0
  
  message("FHIR circuit breaker reset")
  
  # Log reset event
  log_circuit_event("manual_reset", list(
    reason = reason %||% "Manual reset by admin"
  ))
}

#' Configure circuit breaker parameters
#' @param failure_threshold Number of failures before opening
#' @param timeout_seconds Time to wait before half-open
#' @param success_threshold Successes needed to close from half-open
#' @param request_timeout Default request timeout
#' @export
configure_circuit_breaker <- function(failure_threshold = NULL,
                                     timeout_seconds = NULL,
                                     success_threshold = NULL,
                                     request_timeout = NULL) {
  
  if (!is.null(failure_threshold)) {
    .fhir_circuit$config$failure_threshold <- failure_threshold
  }
  
  if (!is.null(timeout_seconds)) {
    .fhir_circuit$config$timeout_seconds <- timeout_seconds
  }
  
  if (!is.null(success_threshold)) {
    .fhir_circuit$config$success_threshold <- success_threshold
  }
  
  if (!is.null(request_timeout)) {
    .fhir_circuit$config$request_timeout <- request_timeout
  }
  
  message("Circuit breaker configuration updated")
  return(.fhir_circuit$config)
}

# Logging functions (implementations depend on your logging setup)

#' Log circuit breaker events
log_circuit_event <- function(event_type, details = NULL) {
  # Implementation depends on your logging system
  # For now, just use message()
  if (getOption("fhir.verbose", FALSE)) {
    message(sprintf("[CIRCUIT] %s: %s", 
                   event_type, 
                   jsonlite::toJSON(details, auto_unbox = TRUE)))
  }
}

#' Log network errors
log_network_error <- function(error_message, attempt) {
  if (getOption("fhir.verbose", FALSE)) {
    message(sprintf("[NETWORK] Attempt %d failed: %s", attempt, error_message))
  }
}

#' Audit event logging
audit_event <- function(event_type, details) {
  # Implementation depends on your audit system
  # Could write to database, file, or external service
  if (getOption("fhir.audit", FALSE)) {
    audit_entry <- list(
      timestamp = Sys.time(),
      event = event_type,
      details = details,
      user = Sys.getenv("USER", "unknown")
    )
    
    # Write to audit log (implementation specific)
    # For now, just use message
    message(sprintf("[AUDIT] %s", jsonlite::toJSON(audit_entry, auto_unbox = TRUE)))
  }
}