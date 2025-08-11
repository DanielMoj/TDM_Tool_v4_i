#' Error Monitoring System for TDMx
#' 
#' Central error tracking, aggregation, and alerting system
#' 
#' @description
#' This module provides comprehensive error monitoring with:
#' - Real-time error tracking
#' - Error aggregation and classification
#' - Automatic alerting for critical errors
#' - Performance metrics tracking
#' - Error pattern detection

# Required libraries
library(DBI)
library(jsonlite)
library(digest)
library(lubridate)

# Configuration
.error_monitor_config <- new.env(parent = emptyenv())
.error_monitor_config$max_errors_in_memory <- 1000
.error_monitor_config$alert_threshold_critical <- 5
.error_monitor_config$alert_threshold_warning <- 20
.error_monitor_config$alert_window_minutes <- 5
.error_monitor_config$db_enabled <- FALSE
.error_monitor_config$email_alerts <- FALSE
.error_monitor_config$slack_webhook <- NULL

# Error storage
.error_store <- new.env(parent = emptyenv())
.error_store$errors <- list()
.error_store$metrics <- list()
.error_store$alerts_sent <- list()

#' Initialize error monitoring system
#' 
#' @param db_connection Optional database connection for persistence
#' @param config List with configuration overrides
#' @export
init_error_monitor <- function(db_connection = NULL, config = list()) {
  
  # Apply configuration
  for (name in names(config)) {
    if (name %in% ls(.error_monitor_config)) {
      .error_monitor_config[[name]] <- config[[name]]
    }
  }
  
  # Set up database if provided
  if (!is.null(db_connection)) {
    .error_monitor_config$db_connection <- db_connection
    .error_monitor_config$db_enabled <- TRUE
    
    # Create tables if they don't exist
    create_error_tables(db_connection)
  }
  
  # Initialize metrics
  .error_store$metrics <- list(
    total_errors = 0,
    errors_by_severity = list(CRITICAL = 0, ERROR = 0, WARNING = 0, INFO = 0),
    errors_by_module = list(),
    error_rate_per_minute = numeric(),
    last_error_time = NULL,
    uptime_start = Sys.time()
  )
  
  # Set up cleanup scheduler
  schedule_cleanup()
  
  message("Error monitoring system initialized")
  return(TRUE)
}

#' Log an error event
#' 
#' @param error_type Type/category of error
#' @param message Error message
#' @param severity One of: CRITICAL, ERROR, WARNING, INFO
#' @param module Module where error occurred
#' @param context Additional context (list)
#' @param stack_trace Optional stack trace
#' @export
log_error <- function(error_type, 
                     message, 
                     severity = "ERROR",
                     module = NA,
                     context = list(),
                     stack_trace = NULL) {
  
  # Create error record
  error_record <- list(
    id = generate_error_id(),
    timestamp = Sys.time(),
    error_type = error_type,
    message = message,
    severity = severity,
    module = module,
    context = context,
    stack_trace = stack_trace %||% capture_stack_trace(),
    session_id = get_session_id(),
    user = get_current_user(),
    environment = Sys.getenv("R_ENV", "production")
  )
  
  # Store in memory
  store_error_memory(error_record)
  
  # Store in database if enabled
  if (.error_monitor_config$db_enabled) {
    store_error_db(error_record)
  }
  
  # Update metrics
  update_error_metrics(error_record)
  
  # Check for alerts
  check_alert_conditions(error_record)
  
  # Log to file
  write_error_log(error_record)
  
  return(error_record$id)
}

#' Store error in memory
store_error_memory <- function(error_record) {
  .error_store$errors[[error_record$id]] <- error_record
  
  # Limit memory usage
  if (length(.error_store$errors) > .error_monitor_config$max_errors_in_memory) {
    # Remove oldest errors
    timestamps <- sapply(.error_store$errors, function(e) e$timestamp)
    oldest <- names(sort(timestamps)[1:100])
    .error_store$errors[oldest] <- NULL
  }
}

#' Store error in database
store_error_db <- function(error_record) {
  if (!.error_monitor_config$db_enabled) return(FALSE)
  
  tryCatch({
    conn <- .error_monitor_config$db_connection
    
    # Serialize complex fields
    context_json <- toJSON(error_record$context, auto_unbox = TRUE)
    stack_json <- toJSON(error_record$stack_trace, auto_unbox = TRUE)
    
    # Insert into database
    query <- "
      INSERT INTO error_log 
      (error_id, timestamp, error_type, message, severity, module, 
       context, stack_trace, session_id, user_id, environment)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
    "
    
    DBI::dbExecute(conn, query, params = list(
      error_record$id,
      error_record$timestamp,
      error_record$error_type,
      error_record$message,
      error_record$severity,
      error_record$module,
      context_json,
      stack_json,
      error_record$session_id,
      error_record$user,
      error_record$environment
    ))
    
    return(TRUE)
    
  }, error = function(e) {
    warning(sprintf("Failed to store error in database: %s", e$message))
    return(FALSE)
  })
}

#' Update error metrics
update_error_metrics <- function(error_record) {
  metrics <- .error_store$metrics
  
  # Update totals
  metrics$total_errors <- metrics$total_errors + 1
  metrics$errors_by_severity[[error_record$severity]] <- 
    (metrics$errors_by_severity[[error_record$severity]] %||% 0) + 1
  
  # Update by module
  if (!is.na(error_record$module)) {
    metrics$errors_by_module[[error_record$module]] <- 
      (metrics$errors_by_module[[error_record$module]] %||% 0) + 1
  }
  
  # Update error rate
  current_minute <- floor_date(Sys.time(), "minute")
  if (length(metrics$error_rate_per_minute) == 0 || 
      names(metrics$error_rate_per_minute)[length(metrics$error_rate_per_minute)] != as.character(current_minute)) {
    metrics$error_rate_per_minute[as.character(current_minute)] <- 1
  } else {
    metrics$error_rate_per_minute[as.character(current_minute)] <- 
      metrics$error_rate_per_minute[as.character(current_minute)] + 1
  }
  
  # Keep only last hour of rate data
  cutoff <- Sys.time() - hours(1)
  keep_times <- as.POSIXct(names(metrics$error_rate_per_minute)) >= cutoff
  metrics$error_rate_per_minute <- metrics$error_rate_per_minute[keep_times]
  
  metrics$last_error_time <- error_record$timestamp
  
  .error_store$metrics <- metrics
}

#' Check alert conditions and send alerts if needed
check_alert_conditions <- function(error_record) {
  
  # Check for critical errors
  if (error_record$severity == "CRITICAL") {
    send_alert(
      level = "CRITICAL",
      subject = sprintf("CRITICAL ERROR: %s", error_record$error_type),
      message = format_alert_message(error_record),
      error_id = error_record$id
    )
    return()
  }
  
  # Check error rate thresholds
  window_start <- Sys.time() - minutes(.error_monitor_config$alert_window_minutes)
  recent_errors <- Filter(function(e) e$timestamp >= window_start, .error_store$errors)
  
  error_count <- length(recent_errors)
  critical_count <- sum(sapply(recent_errors, function(e) e$severity == "CRITICAL"))
  
  # Alert on high error rate
  if (error_count >= .error_monitor_config$alert_threshold_warning) {
    if (!has_recent_alert("high_error_rate")) {
      send_alert(
        level = "WARNING",
        subject = sprintf("High error rate: %d errors in %d minutes", 
                         error_count, .error_monitor_config$alert_window_minutes),
        message = format_rate_alert(recent_errors)
      )
    }
  }
  
  # Alert on multiple critical errors
  if (critical_count >= .error_monitor_config$alert_threshold_critical) {
    if (!has_recent_alert("critical_errors")) {
      send_alert(
        level = "CRITICAL",
        subject = sprintf("Multiple critical errors: %d in %d minutes", 
                         critical_count, .error_monitor_config$alert_window_minutes),
        message = format_rate_alert(Filter(function(e) e$severity == "CRITICAL", recent_errors))
      )
    }
  }
  
  # Check for error patterns
  check_error_patterns(recent_errors)
}

#' Check for error patterns
check_error_patterns <- function(errors) {
  if (length(errors) < 5) return()
  
  # Check for repeated errors
  error_types <- sapply(errors, function(e) e$error_type)
  type_counts <- table(error_types)
  
  for (error_type in names(type_counts)) {
    if (type_counts[error_type] >= 5) {
      if (!has_recent_alert(paste0("repeated_", error_type))) {
        send_alert(
          level = "WARNING",
          subject = sprintf("Repeated error pattern: %s (%d times)", 
                           error_type, type_counts[error_type]),
          message = sprintf("Error '%s' has occurred %d times in the last %d minutes",
                           error_type, type_counts[error_type], 
                           .error_monitor_config$alert_window_minutes)
        )
      }
    }
  }
  
  # Check for module-specific issues
  modules <- sapply(errors, function(e) e$module)
  modules <- modules[!is.na(modules)]
  if (length(modules) > 0) {
    module_counts <- table(modules)
    
    for (module in names(module_counts)) {
      if (module_counts[module] >= 10) {
        if (!has_recent_alert(paste0("module_", module))) {
          send_alert(
            level = "WARNING",
            subject = sprintf("Module experiencing issues: %s", module),
            message = sprintf("Module '%s' has generated %d errors in the last %d minutes",
                             module, module_counts[module], 
                             .error_monitor_config$alert_window_minutes)
          )
        }
      }
    }
  }
}

#' Send alert
send_alert <- function(level, subject, message, error_id = NULL) {
  
  alert_record <- list(
    timestamp = Sys.time(),
    level = level,
    subject = subject,
    message = message,
    error_id = error_id
  )
  
  # Record alert
  alert_key <- paste0(level, "_", gsub("[^a-zA-Z0-9]", "_", subject))
  .error_store$alerts_sent[[alert_key]] <- alert_record
  
  # Send email if configured
  if (.error_monitor_config$email_alerts) {
    send_email_alert(alert_record)
  }
  
  # Send to Slack if configured
  if (!is.null(.error_monitor_config$slack_webhook)) {
    send_slack_alert(alert_record)
  }
  
  # Log alert
  message(sprintf("[ALERT] %s: %s", level, subject))
  
  # Store in database
  if (.error_monitor_config$db_enabled) {
    store_alert_db(alert_record)
  }
}

#' Check if alert was recently sent
has_recent_alert <- function(alert_type, window_minutes = 15) {
  key <- alert_type
  if (key %in% names(.error_store$alerts_sent)) {
    last_alert <- .error_store$alerts_sent[[key]]
    if (difftime(Sys.time(), last_alert$timestamp, units = "mins") < window_minutes) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Get error statistics
#' @export
get_error_stats <- function(time_window = NULL) {
  
  if (is.null(time_window)) {
    errors <- .error_store$errors
  } else {
    cutoff <- Sys.time() - time_window
    errors <- Filter(function(e) e$timestamp >= cutoff, .error_store$errors)
  }
  
  if (length(errors) == 0) {
    return(list(
      total = 0,
      by_severity = list(),
      by_module = list(),
      by_type = list(),
      error_rate = 0,
      uptime = difftime(Sys.time(), .error_store$metrics$uptime_start, units = "hours")
    ))
  }
  
  # Calculate statistics
  stats <- list(
    total = length(errors),
    by_severity = table(sapply(errors, function(e) e$severity)),
    by_module = table(sapply(errors, function(e) e$module)),
    by_type = table(sapply(errors, function(e) e$error_type)),
    error_rate = mean(.error_store$metrics$error_rate_per_minute),
    recent_errors = tail(errors, 10),
    uptime = difftime(Sys.time(), .error_store$metrics$uptime_start, units = "hours"),
    last_error = .error_store$metrics$last_error_time
  )
  
  return(stats)
}

#' Get error trends
#' @export
get_error_trends <- function(hours = 24) {
  cutoff <- Sys.time() - hours(hours)
  
  if (.error_monitor_config$db_enabled) {
    # Fetch from database
    conn <- .error_monitor_config$db_connection
    query <- "
      SELECT 
        date_trunc('hour', timestamp) as hour,
        severity,
        COUNT(*) as count
      FROM error_log
      WHERE timestamp >= $1
      GROUP BY hour, severity
      ORDER BY hour
    "
    
    trends <- DBI::dbGetQuery(conn, query, params = list(cutoff))
    
  } else {
    # Calculate from memory
    errors <- Filter(function(e) e$timestamp >= cutoff, .error_store$errors)
    
    if (length(errors) == 0) {
      return(data.frame(hour = character(), severity = character(), count = numeric()))
    }
    
    # Group by hour and severity
    error_df <- do.call(rbind, lapply(errors, function(e) {
      data.frame(
        hour = floor_date(e$timestamp, "hour"),
        severity = e$severity,
        stringsAsFactors = FALSE
      )
    }))
    
    trends <- aggregate(
      list(count = rep(1, nrow(error_df))),
      by = list(hour = error_df$hour, severity = error_df$severity),
      FUN = sum
    )
  }
  
  return(trends)
}

#' Clear error history
#' @export
clear_error_history <- function(older_than = NULL) {
  
  if (is.null(older_than)) {
    # Clear all
    .error_store$errors <- list()
    message("Cleared all error history")
    
  } else {
    # Clear errors older than specified time
    cutoff <- Sys.time() - older_than
    keep <- Filter(function(e) e$timestamp >= cutoff, .error_store$errors)
    removed <- length(.error_store$errors) - length(keep)
    .error_store$errors <- keep
    message(sprintf("Removed %d errors older than %s", removed, cutoff))
  }
  
  # Clear from database if enabled
  if (.error_monitor_config$db_enabled) {
    conn <- .error_monitor_config$db_connection
    
    if (is.null(older_than)) {
      DBI::dbExecute(conn, "DELETE FROM error_log")
    } else {
      DBI::dbExecute(conn, "DELETE FROM error_log WHERE timestamp < $1", 
                    params = list(cutoff))
    }
  }
}

# Helper functions

generate_error_id <- function() {
  paste0("ERR_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", 
         paste0(sample(c(0:9, letters[1:6]), 6, TRUE), collapse = ""))
}

capture_stack_trace <- function() {
  calls <- sys.calls()
  if (length(calls) > 2) {
    # Remove this function and log_error from trace
    calls <- calls[1:(length(calls) - 2)]
  }
  lapply(calls, deparse)
}

get_session_id <- function() {
  if (exists("session") && !is.null(session$token)) {
    return(session$token)
  }
  return(NA)
}

get_current_user <- function() {
  if (exists("session") && !is.null(session$user)) {
    return(session$user)
  }
  return(Sys.getenv("USER", "unknown"))
}

write_error_log <- function(error_record) {
  log_dir <- "log/errors"
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  
  log_file <- file.path(log_dir, sprintf("errors_%s.log", format(Sys.Date(), "%Y%m%d")))
  
  log_entry <- sprintf(
    "[%s] %s | %s | %s | %s | %s",
    format(error_record$timestamp, "%Y-%m-%d %H:%M:%S"),
    error_record$severity,
    error_record$error_type,
    error_record$module,
    error_record$message,
    error_record$user
  )
  
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

format_alert_message <- function(error_record) {
  sprintf(
    "Error Details:\n\nType: %s\nSeverity: %s\nModule: %s\nMessage: %s\nTime: %s\nUser: %s\nSession: %s\nEnvironment: %s\n\nContext: %s",
    error_record$error_type,
    error_record$severity,
    error_record$module %||% "Unknown",
    error_record$message,
    error_record$timestamp,
    error_record$user,
    error_record$session_id %||% "N/A",
    error_record$environment,
    toJSON(error_record$context, pretty = TRUE, auto_unbox = TRUE)
  )
}

format_rate_alert <- function(errors) {
  summary <- table(sapply(errors, function(e) e$error_type))
  
  paste0(
    "Error Summary:\n\n",
    "Total errors: ", length(errors), "\n",
    "Time window: Last ", .error_monitor_config$alert_window_minutes, " minutes\n\n",
    "Error types:\n",
    paste(sprintf("  - %s: %d", names(summary), summary), collapse = "\n"),
    "\n\nMost recent errors:\n",
    paste(sapply(tail(errors, 5), function(e) {
      sprintf("  [%s] %s: %s", 
              format(e$timestamp, "%H:%M:%S"), 
              e$error_type, 
              substr(e$message, 1, 50))
    }), collapse = "\n")
  )
}

schedule_cleanup <- function() {
  # Schedule periodic cleanup of old errors
  later::later(function() {
    clear_error_history(older_than = hours(24))
    schedule_cleanup()  # Reschedule
  }, delay = 3600)  # Every hour
}

create_error_tables <- function(conn) {
  # Create error log table
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS error_log (
      error_id VARCHAR(50) PRIMARY KEY,
      timestamp TIMESTAMP NOT NULL,
      error_type VARCHAR(100),
      message TEXT,
      severity VARCHAR(20),
      module VARCHAR(100),
      context JSONB,
      stack_trace JSONB,
      session_id VARCHAR(100),
      user_id VARCHAR(100),
      environment VARCHAR(50),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create indexes
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_error_timestamp ON error_log(timestamp)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_error_severity ON error_log(severity)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_error_module ON error_log(module)")
  
  # Create alerts table
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS error_alerts (
      alert_id SERIAL PRIMARY KEY,
      timestamp TIMESTAMP NOT NULL,
      level VARCHAR(20),
      subject VARCHAR(200),
      message TEXT,
      error_id VARCHAR(50),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
}

store_alert_db <- function(alert_record) {
  if (!.error_monitor_config$db_enabled) return()
  
  tryCatch({
    conn <- .error_monitor_config$db_connection
    
    DBI::dbExecute(conn, "
      INSERT INTO error_alerts (timestamp, level, subject, message, error_id)
      VALUES ($1, $2, $3, $4, $5)
    ", params = list(
      alert_record$timestamp,
      alert_record$level,
      alert_record$subject,
      alert_record$message,
      alert_record$error_id
    ))
    
  }, error = function(e) {
    warning(sprintf("Failed to store alert in database: %s", e$message))
  })
}

# Placeholder functions for external integrations
send_email_alert <- function(alert_record) {
  # Implement email sending logic
  message(sprintf("Would send email: %s", alert_record$subject))
}

send_slack_alert <- function(alert_record) {
  # Implement Slack webhook integration
  message(sprintf("Would send to Slack: %s", alert_record$subject))
}