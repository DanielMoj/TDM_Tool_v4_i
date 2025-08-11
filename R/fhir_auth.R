#' FHIR Authentication and Token Management
#' 
#' @description
#' Manages OAuth2 authentication with FHIR server including:
#' - Token acquisition and storage
#' - Automatic token refresh
#' - Secure credential handling
#' - Multi-tenant support

# Token storage environment (in production: use secure storage like keyring)
.fhir_tokens <- new.env(parent = emptyenv())
.fhir_auth_config <- new.env(parent = emptyenv())

# Default configuration
.fhir_auth_config$settings <- list(
  token_refresh_margin = 60,  # Refresh tokens 60 seconds before expiry
  max_auth_retries = 3,
  auth_timeout = 10,
  supported_grant_types = c("client_credentials", "refresh_token", "authorization_code")
)

#' Initialize FHIR authentication
#' 
#' @param auth_url OAuth2 token endpoint URL
#' @param client_id OAuth2 client ID
#' @param client_secret OAuth2 client secret (optional for public clients)
#' @param scope OAuth2 scope (default: "system/*.read")
#' @param tenant_id Optional tenant identifier for multi-tenant setups
#' @export
init_fhir_auth <- function(auth_url = NULL, 
                          client_id = NULL, 
                          client_secret = NULL,
                          scope = "system/*.read",
                          tenant_id = "default") {
  
  # Get from environment if not provided
  auth_url <- auth_url %||% Sys.getenv("FHIR_AUTH_URL")
  client_id <- client_id %||% Sys.getenv("FHIR_CLIENT_ID")
  client_secret <- client_secret %||% Sys.getenv("FHIR_CLIENT_SECRET")
  
  # Validate required parameters
  if (!nzchar(auth_url)) {
    stop("FHIR authentication URL is required (auth_url or FHIR_AUTH_URL env var)")
  }
  
  if (!nzchar(client_id)) {
    stop("FHIR client ID is required (client_id or FHIR_CLIENT_ID env var)")
  }
  
  # Store configuration
  .fhir_auth_config[[tenant_id]] <- list(
    auth_url = auth_url,
    client_id = client_id,
    client_secret = client_secret,
    scope = scope
  )
  
  message(sprintf("FHIR authentication initialized for tenant: %s", tenant_id))
  
  # Attempt initial authentication
  if (fhir_authenticate(tenant_id = tenant_id)) {
    message("Initial authentication successful")
    return(TRUE)
  } else {
    warning("Initial authentication failed - manual authentication required")
    return(FALSE)
  }
}

#' Get current FHIR access token
#' 
#' @param tenant_id Tenant identifier (default: "default")
#' @param force_refresh Force token refresh even if not expired
#' @return Access token string or stops with error
#' @export
get_fhir_token <- function(tenant_id = "default", force_refresh = FALSE) {
  
  # Check if tenant is configured
  if (!exists(tenant_id, envir = .fhir_auth_config)) {
    stop(sprintf("Tenant '%s' not configured. Run init_fhir_auth() first.", tenant_id))
  }
  
  token_key <- paste0("token_", tenant_id)
  
  # Check if token exists and is valid
  if (!force_refresh && exists(token_key, envir = .fhir_tokens)) {
    token_data <- .fhir_tokens[[token_key]]
    
    # Check expiry with margin
    if (token_data$expires_at > Sys.time()) {
      return(token_data$token)
    }
    
    message("FHIR token expired, refreshing...")
  }
  
  # Get new token
  if (fhir_authenticate(tenant_id = tenant_id)) {
    token_data <- .fhir_tokens[[token_key]]
    return(token_data$token)
  }
  
  stop(sprintf("Failed to obtain FHIR access token for tenant: %s", tenant_id))
}

#' Authenticate with FHIR server
#' 
#' @param tenant_id Tenant identifier
#' @param grant_type OAuth2 grant type
#' @return TRUE on success, FALSE on failure
#' @export
fhir_authenticate <- function(tenant_id = "default", 
                            grant_type = "client_credentials") {
  
  # Get configuration
  config <- .fhir_auth_config[[tenant_id]]
  if (is.null(config)) {
    warning(sprintf("No configuration found for tenant: %s", tenant_id))
    return(FALSE)
  }
  
  # Check for existing refresh token
  token_key <- paste0("token_", tenant_id)
  if (grant_type == "client_credentials" && exists(token_key, envir = .fhir_tokens)) {
    token_data <- .fhir_tokens[[token_key]]
    if (!is.null(token_data$refresh_token)) {
      # Try refresh first
      if (fhir_refresh_token(tenant_id = tenant_id)) {
        return(TRUE)
      }
    }
  }
  
  # Prepare authentication request
  auth_body <- list(
    grant_type = grant_type,
    client_id = config$client_id,
    scope = config$scope
  )
  
  # Add client secret if available (not required for public clients)
  if (!is.null(config$client_secret) && nzchar(config$client_secret)) {
    auth_body$client_secret <- config$client_secret
  }
  
  # Attempt authentication with retries
  attempt <- 1
  max_attempts <- .fhir_auth_config$settings$max_auth_retries
  
  while (attempt <= max_attempts) {
    result <- perform_auth_request(
      url = config$auth_url,
      body = auth_body,
      tenant_id = tenant_id,
      attempt = attempt
    )
    
    if (result$success) {
      return(TRUE)
    }
    
    if (attempt < max_attempts) {
      wait_time <- min(2^(attempt - 1), 10)  # Exponential backoff, max 10s
      message(sprintf("Authentication attempt %d failed, retrying in %d seconds...", 
                     attempt, wait_time))
      Sys.sleep(wait_time)
    }
    
    attempt <- attempt + 1
  }
  
  warning(sprintf("FHIR authentication failed after %d attempts for tenant: %s", 
                 max_attempts, tenant_id))
  return(FALSE)
}

#' Perform authentication request
perform_auth_request <- function(url, body, tenant_id, attempt = 1) {
  
  tryCatch({
    response <- httr::POST(
      url,
      body = body,
      encode = "form",
      httr::timeout(.fhir_auth_config$settings$auth_timeout),
      httr::user_agent("FHIR-R-Client/1.0")
    )
    
    # Check response status
    if (httr::http_error(response)) {
      error_msg <- extract_auth_error(response)
      warning(sprintf("Authentication failed (attempt %d): %s", attempt, error_msg))
      
      return(list(success = FALSE, error = error_msg))
    }
    
    # Parse response
    content <- httr::content(response, as = "parsed", type = "application/json")
    
    # Validate token response
    if (is.null(content$access_token)) {
      warning("Invalid token response: missing access_token")
      return(list(success = FALSE, error = "Invalid token response"))
    }
    
    # Calculate token expiry
    expires_in <- content$expires_in %||% 3600  # Default 1 hour
    expires_at <- Sys.time() + expires_in - .fhir_auth_config$settings$token_refresh_margin
    
    # Store token data
    token_key <- paste0("token_", tenant_id)
    .fhir_tokens[[token_key]] <- list(
      token = content$access_token,
      token_type = content$token_type %||% "Bearer",
      expires_at = expires_at,
      expires_in = expires_in,
      refresh_token = content$refresh_token,
      scope = content$scope %||% body$scope,
      obtained_at = Sys.time()
    )
    
    message(sprintf("FHIR authentication successful (tenant: %s, expires in: %ds)", 
                   tenant_id, expires_in))
    
    # Log successful authentication
    log_auth_event("authentication_success", list(
      tenant_id = tenant_id,
      grant_type = body$grant_type,
      scope = content$scope,
      expires_in = expires_in
    ))
    
    return(list(success = TRUE))
    
  }, error = function(e) {
    warning(sprintf("Authentication request failed: %s", e$message))
    
    log_auth_event("authentication_error", list(
      tenant_id = tenant_id,
      error = e$message,
      attempt = attempt
    ))
    
    return(list(success = FALSE, error = e$message))
  })
}

#' Refresh FHIR token
#' 
#' @param tenant_id Tenant identifier
#' @return TRUE on success, FALSE on failure
#' @export
fhir_refresh_token <- function(tenant_id = "default") {
  
  token_key <- paste0("token_", tenant_id)
  
  # Check if refresh token exists
  if (!exists(token_key, envir = .fhir_tokens)) {
    message("No existing token to refresh")
    return(fhir_authenticate(tenant_id = tenant_id))
  }
  
  token_data <- .fhir_tokens[[token_key]]
  
  if (is.null(token_data$refresh_token)) {
    message("No refresh token available, performing full authentication")
    return(fhir_authenticate(tenant_id = tenant_id))
  }
  
  # Get configuration
  config <- .fhir_auth_config[[tenant_id]]
  if (is.null(config)) {
    warning(sprintf("No configuration found for tenant: %s", tenant_id))
    return(FALSE)
  }
  
  # Prepare refresh request
  refresh_body <- list(
    grant_type = "refresh_token",
    refresh_token = token_data$refresh_token,
    client_id = config$client_id
  )
  
  # Add client secret if available
  if (!is.null(config$client_secret) && nzchar(config$client_secret)) {
    refresh_body$client_secret <- config$client_secret
  }
  
  tryCatch({
    response <- httr::POST(
      config$auth_url,
      body = refresh_body,
      encode = "form",
      httr::timeout(.fhir_auth_config$settings$auth_timeout)
    )
    
    if (httr::http_error(response)) {
      # Refresh failed, try full authentication
      warning(sprintf("Token refresh failed: %s", extract_auth_error(response)))
      return(fhir_authenticate(tenant_id = tenant_id))
    }
    
    # Parse response
    content <- httr::content(response, as = "parsed", type = "application/json")
    
    # Update token data
    expires_in <- content$expires_in %||% 3600
    .fhir_tokens[[token_key]] <- list(
      token = content$access_token,
      token_type = content$token_type %||% "Bearer",
      expires_at = Sys.time() + expires_in - .fhir_auth_config$settings$token_refresh_margin,
      expires_in = expires_in,
      refresh_token = content$refresh_token %||% token_data$refresh_token,  # Keep old if not provided
      scope = content$scope %||% token_data$scope,
      obtained_at = Sys.time()
    )
    
    message(sprintf("FHIR token refreshed successfully (tenant: %s)", tenant_id))
    
    log_auth_event("token_refresh_success", list(
      tenant_id = tenant_id,
      expires_in = expires_in
    ))
    
    return(TRUE)
    
  }, error = function(e) {
    warning(sprintf("Token refresh failed: %s", e$message))
    
    log_auth_event("token_refresh_error", list(
      tenant_id = tenant_id,
      error = e$message
    ))
    
    # Fall back to full authentication
    return(fhir_authenticate(tenant_id = tenant_id))
  })
}

#' Extract error message from OAuth response
extract_auth_error <- function(response) {
  # Try to parse error from response body
  tryCatch({
    content <- httr::content(response, as = "parsed", type = "application/json", 
                            encoding = "UTF-8")
    
    # OAuth2 standard error format
    if (!is.null(content$error)) {
      error_desc <- content$error_description %||% content$error
      return(sprintf("%s: %s", httr::status_code(response), error_desc))
    }
    
    # Fallback to status message
    return(sprintf("%d: %s", 
                  httr::status_code(response), 
                  httr::http_status(response)$message))
    
  }, error = function(e) {
    # If parsing fails, use basic status
    return(sprintf("%d: %s", 
                  httr::status_code(response), 
                  httr::http_status(response)$message))
  })
}

#' Get token information
#' 
#' @param tenant_id Tenant identifier
#' @return List with token information or NULL
#' @export
get_token_info <- function(tenant_id = "default") {
  token_key <- paste0("token_", tenant_id)
  
  if (!exists(token_key, envir = .fhir_tokens)) {
    return(NULL)
  }
  
  token_data <- .fhir_tokens[[token_key]]
  
  # Calculate time remaining
  time_remaining <- difftime(token_data$expires_at, Sys.time(), units = "secs")
  
  list(
    tenant_id = tenant_id,
    token_type = token_data$token_type,
    scope = token_data$scope,
    expires_at = token_data$expires_at,
    expires_in_seconds = max(0, as.numeric(time_remaining)),
    is_expired = time_remaining <= 0,
    has_refresh_token = !is.null(token_data$refresh_token),
    obtained_at = token_data$obtained_at
  )
}

#' Clear stored tokens
#' 
#' @param tenant_id Specific tenant or NULL for all
#' @export
clear_tokens <- function(tenant_id = NULL) {
  if (is.null(tenant_id)) {
    # Clear all tokens
    rm(list = ls(envir = .fhir_tokens), envir = .fhir_tokens)
    message("All FHIR tokens cleared")
  } else {
    # Clear specific tenant token
    token_key <- paste0("token_", tenant_id)
    if (exists(token_key, envir = .fhir_tokens)) {
      rm(list = token_key, envir = .fhir_tokens)
      message(sprintf("Token cleared for tenant: %s", tenant_id))
    }
  }
}

#' Build authorization header
#' 
#' @param tenant_id Tenant identifier
#' @return Named character vector for use with httr
#' @export
fhir_auth_header <- function(tenant_id = "default") {
  token <- get_fhir_token(tenant_id = tenant_id)
  token_info <- get_token_info(tenant_id = tenant_id)
  
  token_type <- token_info$token_type %||% "Bearer"
  
  c(Authorization = paste(token_type, token))
}

#' Check if authentication is configured
#' 
#' @param tenant_id Tenant identifier
#' @return TRUE if configured, FALSE otherwise
#' @export
is_fhir_auth_configured <- function(tenant_id = "default") {
  exists(tenant_id, envir = .fhir_auth_config)
}

#' Log authentication events
log_auth_event <- function(event_type, details = NULL) {
  if (getOption("fhir.auth.verbose", FALSE)) {
    message(sprintf("[AUTH] %s: %s", 
                   event_type, 
                   jsonlite::toJSON(details, auto_unbox = TRUE)))
  }
}

# Null-safe operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}