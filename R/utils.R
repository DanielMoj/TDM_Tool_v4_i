# R/utils.R
# Zentrale Utility-Funktionen mit Fehlerbehandlung

#' Null coalescing operator
#'
#' Returns y if x is NULL, otherwise returns x
#'
#' @param x Primary value
#' @param y Fallback value if x is NULL
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Parse numeric list from comma-separated string
parse_num_list <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(numeric(0))
  }
  
  # Split and clean
  parts <- trimws(unlist(strsplit(x, ",")))
  
  # Convert to numeric with warning for failures
  vals <- suppressWarnings(as.numeric(parts))
  
  # Check for conversion failures
  failed <- is.na(vals) & nzchar(parts)
  if (any(failed)) {
    warning(sprintf("Could not parse %d value(s): %s", 
                   sum(failed), 
                   paste(parts[failed], collapse = ", ")))
  }
  
  # Return valid values
  vals[!is.na(vals)]
}

# Stop with NA check
stop_na <- function(x, msg) {
  if (any(!is.finite(x))) {
    stop(msg, call. = FALSE)
  }
}

# Coalesce multiple values (returns first non-NULL)
coalesce <- function(...) {
  values <- list(...)
  for (val in values) {
    if (!is.null(val)) return(val)
  }
  return(NULL)
}

# Safe element extraction
safe_extract <- function(x, key, default = NULL) {
  if (is.null(x)) return(default)
  if (key %in% names(x)) {
    return(x[[key]])
  }
  return(default)
}

# Check if value is empty (NULL, NA, empty string, or length 0)
is_empty <- function(x) {
  is.null(x) || 
  length(x) == 0 || 
  all(is.na(x)) ||
  (is.character(x) && all(!nzchar(trimws(x))))
}

# Safe paste with NULL handling
safe_paste <- function(..., sep = " ", collapse = NULL) {
  args <- list(...)
  # Remove NULL values
  args <- args[!sapply(args, is.null)]
  if (length(args) == 0) return("")
  do.call(paste, c(args, list(sep = sep, collapse = collapse)))
}

# Format number with proper precision
format_number <- function(x, digits = 3, scientific = FALSE) {
  if (is.null(x) || is.na(x)) return("NA")
  format(x, digits = digits, scientific = scientific)
}

# Safe division (returns NA for division by zero)
safe_divide <- function(numerator, denominator, na_value = NA_real_) {
  if (is.null(numerator) || is.null(denominator)) return(na_value)
  result <- numerator / denominator
  result[!is.finite(result)] <- na_value
  return(result)
}

# Clamp value between min and max
clamp <- function(x, min_val = -Inf, max_val = Inf) {
  pmax(pmin(x, max_val), min_val)
}

# Check if running in test environment
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true") ||
  identical(Sys.getenv("R_TESTS"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true") ||
  identical(Sys.getenv("CI"), "true")
}

# Get environment variable with default
get_env <- function(var, default = NULL) {
  val <- Sys.getenv(var, unset = NA)
  if (is.na(val) || !nzchar(val)) {
    return(default)
  }
  return(val)
}

# Create directory if it doesn't exist
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  return(path)
}

# Generate unique ID
generate_id <- function(prefix = "id", length = 8) {
  random_part <- paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = "")
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", random_part)
}

# Retry function with exponential backoff
retry_with_backoff <- function(fn, max_attempts = 3, initial_wait = 1, multiplier = 2) {
  attempt <- 1
  wait_time <- initial_wait
  
  while (attempt <= max_attempts) {
    result <- tryCatch(
      {
        return(fn())
      },
      error = function(e) {
        if (attempt == max_attempts) {
          stop(e)
        }
        message(sprintf("Attempt %d failed: %s. Retrying in %g seconds...", 
                       attempt, e$message, wait_time))
        Sys.sleep(wait_time)
        wait_time <<- wait_time * multiplier
        attempt <<- attempt + 1
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
}

# Memoize function results
memoize <- function(fn) {
  cache <- new.env(parent = emptyenv())
  
  function(...) {
    key <- digest::digest(list(...))
    if (exists(key, envir = cache)) {
      return(cache[[key]])
    }
    result <- fn(...)
    cache[[key]] <- result
    return(result)
  }
}

# Time a function execution
time_it <- function(expr, label = "Execution") {
  start_time <- Sys.time()
  result <- force(expr)
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("%s took %.3f seconds", label, as.numeric(duration)))
  return(result)
}

# Validate email format
is_valid_email <- function(email) {
  if (is.null(email) || !is.character(email)) return(FALSE)
  grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)
}

# Normalize path (expand ~ and make absolute)
normalize_path <- function(path) {
  if (is.null(path)) return(NULL)
  normalizePath(path.expand(path), mustWork = FALSE)
}

# Export for use in other modules
# The %||% operator is now available globally when utils.R is sourced