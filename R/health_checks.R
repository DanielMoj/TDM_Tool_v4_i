# R/health_checks.R
#' System Health Checks for TDMx
#' 
#' Comprehensive health monitoring for all system components
#' 
#' @description
#' Provides health checks for:
#' - Database connectivity
#' - FHIR server availability
#' - File system access
#' - Memory usage
#' - API endpoints
#' - Authentication services
#' - Cache performance

# Required libraries
library(httr)
library(DBI)
library(jsonlite)

# Health check configuration
.health_config <- new.env(parent = emptyenv())
.health_config$checks_enabled <- TRUE
.health_config$check_interval_seconds <- 60
.health_config$timeout_seconds <- 5
.health_config$memory_threshold_mb <- 500
.health_config$disk_threshold_percent <- 90
.health_config$cache_hit_threshold <- 0.5

# Health status storage
.health_status <- new.env(parent = emptyenv())
.health_status$current <- list()
.health_status$history <- list()
.health_status$last_check <- NULL

#' Run all health checks
#' 
#' @param verbose Print detailed output
#' @return List with health status for all components
#' @export
run_health_checks <- function(verbose = FALSE) {
  
  if (verbose) message("Starting system health checks...")
  
  start_time <- Sys.time()
  
  # Initialize results
  results <- list(
    timestamp = start_time,
    overall_status = "healthy",
    checks = list()
  )
  
  # Database health
  results$checks$database <- check_database_health()
  
  # FHIR server health
  results$checks$fhir <- check_fhir_health()
  
  # File system health
  results$checks$filesystem <- check_filesystem_health()
  
  # Memory health
  results$checks$memory <- check_memory_health()
  
  # API health
  results$checks$api <- check_api_health()
  
  # Authentication health
  results$checks$auth <- check_auth_health()
  
  # Cache health
  results$checks$cache <- check_cache_health()
  
  # Audit system health
  results$checks$audit <- check_audit_health()
  
  # Model performance
  results$checks$models <- check_model_health()
  
  # Calculate overall status
  statuses <- sapply(results$checks, function(x) x$status)
  
  if (any(statuses == "critical")) {
    results$overall_status <- "critical"
  } else if (any(statuses == "degraded")) {
    results$overall_status <- "degraded"
  } else if (any(statuses == "warning")) {
    results$overall_status <- "warning"
  } else {
    results$overall_status <- "healthy"
  }
  
  # Calculate duration
  results$duration_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
  
  # Store results
  .health_status$current <- results
  .health_status$last_check <- start_time
  
  # Add to history (keep last 100)
  .health_status$history <- c(list(results), .health_status$history)
  if (length(.health_status$history) > 100) {
    .health_status$history <- .health_status$history[1:100]
  }
  
  if (verbose) {
    print_health_summary(results)
  }
  
  # Log critical issues
  if (results$overall_status == "critical") {
    log_critical_health_issues(results)
  }
  
  return(results)
}

#' Check database health
check_database_health <- function() {
  
  result <- list(
    component = "database",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  # Check if database is configured
  db_config <- tryCatch({
    # FIXED: Changed from db.R to db.R (consistent capitalization)
    source("R/db.R", local = TRUE)
    get_db_config()
  }, error = function(e) NULL)
  
  if (is.null(db_config)) {
    result$status <- "warning"
    result$message <- "Database not configured"
    return(result)
  }
  
  # Try to connect
  tryCatch({
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = db_config$host,
      port = db_config$port,
      dbname = db_config$database,
      user = db_config$user,
      password = db_config$password
    )
    
    # Run test query
    test_start <- Sys.time()
    test_result <- DBI::dbGetQuery(conn, "SELECT 1 as test")
    query_time <- as.numeric(difftime(Sys.time(), test_start, units = "secs")) * 1000
    
    # Check connection pool
    pool_info <- DBI::dbGetInfo(conn)
    
    # Get table sizes
    table_sizes <- DBI::dbGetQuery(conn, "
      SELECT 
        schemaname,
        tablename,
        pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
      FROM pg_tables 
      WHERE schemaname = 'public'
      ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
      LIMIT 5
    ")
    
    DBI::dbDisconnect(conn)
    
    result$status <- if (query_time < 100) "healthy" else if (query_time < 500) "warning" else "degraded"
    result$message <- sprintf("Database responsive (query time: %.0fms)", query_time)
    result$metrics <- list(
      query_time_ms = query_time,
      connection_count = pool_info$connections %||% NA,
      largest_tables = table_sizes
    )
    
  }, error = function(e) {
    result$status <- "critical"
    result$message <- sprintf("Database connection failed: %s", e$message)
  })
  
  return(result)
}

#' Check FHIR server health
check_fhir_health <- function() {
  
  result <- list(
    component = "fhir",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  # Check if FHIR is configured
  fhir_url <- Sys.getenv("FHIR_BASE_URL", "")
  
  if (nchar(fhir_url) == 0) {
    result$status <- "warning"
    result$message <- "FHIR server not configured"
    return(result)
  }
  
  # Try to connect
  tryCatch({
    start_time <- Sys.time()
    
    response <- httr::GET(
      paste0(fhir_url, "/metadata"),
      httr::timeout(.health_config$timeout_seconds),
      httr::add_headers(Accept = "application/fhir+json")
    )
    
    response_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
    
    if (httr::status_code(response) == 200) {
      metadata <- httr::content(response, as = "parsed")
      
      result$status <- if (response_time < 500) "healthy" else if (response_time < 2000) "warning" else "degraded"
      result$message <- sprintf("FHIR server responsive (%.0fms)", response_time)
      result$metrics <- list(
        response_time_ms = response_time,
        server_version = metadata$software$version %||% "unknown",
        resource_count = length(metadata$rest[[1]]$resource) %||% 0
      )
      
    } else {
      result$status <- "degraded"
      result$message <- sprintf("FHIR server returned status %d", httr::status_code(response))
      result$metrics$status_code <- httr::status_code(response)
    }
    
  }, error = function(e) {
    result$status <- "critical"
    result$message <- sprintf("FHIR connection failed: %s", e$message)
  })
  
  return(result)
}

#' Check file system health
check_filesystem_health <- function() {
  
  result <- list(
    component = "filesystem",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  critical_paths <- c(
    "config" = "config/",
    "audit" = "audit/",
    "cache" = "cache/",
    "priors" = "priors/",
    "log" = "log/"
  )
  
  issues <- character()
  metrics <- list()
  
  for (name in names(critical_paths)) {
    path <- critical_paths[[name]]
    
    # Check if path exists
    if (!dir.exists(path)) {
      issues <- c(issues, sprintf("%s directory missing", name))
      next
    }
    
    # Check write permissions
    test_file <- file.path(path, ".health_check_test")
    can_write <- tryCatch({
      writeLines("test", test_file)
      file.remove(test_file)
      TRUE
    }, error = function(e) FALSE)
    
    if (!can_write) {
      issues <- c(issues, sprintf("%s not writable", name))
    }
    
    # Get directory size
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    if (length(files) > 0) {
      size_mb <- sum(file.info(files)$size, na.rm = TRUE) / 1024^2
      metrics[[paste0(name, "_size_mb")]] <- round(size_mb, 2)
    }
  }
  
  # Check disk space
  if (Sys.info()["sysname"] != "Windows") {
    disk_info <- tryCatch({
      system("df -h .", intern = TRUE)[2]
    }, error = function(e) NULL)
    
    if (!is.null(disk_info)) {
      parts <- strsplit(disk_info, "\\s+")[[1]]
      if (length(parts) >= 5) {
        use_percent <- as.numeric(sub("%", "", parts[5]))
        metrics$disk_use_percent <- use_percent
        
        if (use_percent > .health_config$disk_threshold_percent) {
          issues <- c(issues, sprintf("Disk usage high: %d%%", use_percent))
        }
      }
    }
  }
  
  result$metrics <- metrics
  
  if (length(issues) == 0) {
    result$status <- "healthy"
    result$message <- "File system checks passed"
  } else if (length(issues) <= 2) {
    result$status <- "warning"
    result$message <- paste(issues, collapse = "; ")
  } else {
    result$status <- "degraded"
    result$message <- sprintf("%d file system issues detected", length(issues))
  }
  
  return(result)
}

#' Check memory health
check_memory_health <- function() {
  
  result <- list(
    component = "memory",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  # Get memory info
  gc_info <- gc()
  
  # Calculate memory usage
  used_mb <- sum(gc_info[, 2])
  max_mb <- sum(gc_info[, 6])
  
  result$metrics <- list(
    used_mb = round(used_mb, 2),
    max_mb = round(max_mb, 2),
    ncells = gc_info[1, 1],
    vcells = gc_info[2, 1]
  )
  
  if (used_mb < .health_config$memory_threshold_mb) {
    result$status <- "healthy"
    result$message <- sprintf("Memory usage normal (%.0f MB)", used_mb)
  } else if (used_mb < .health_config$memory_threshold_mb * 1.5) {
    result$status <- "warning"
    result$message <- sprintf("Memory usage elevated (%.0f MB)", used_mb)
  } else {
    result$status <- "degraded"
    result$message <- sprintf("Memory usage high (%.0f MB)", used_mb)
  }
  
  return(result)
}

#' Check API health
check_api_health <- function() {
  
  result <- list(
    component = "api",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  api_port <- as.integer(Sys.getenv("API_PORT", "8000"))
  api_url <- sprintf("http://localhost:%d/healthz", api_port)
  
  tryCatch({
    start_time <- Sys.time()
    
    response <- httr::GET(
      api_url,
      httr::timeout(.health_config$timeout_seconds)
    )
    
    response_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")
      
      result$status <- if (response_time < 100) "healthy" else if (response_time < 500) "warning" else "degraded"
      result$message <- sprintf("API responsive (%.0fms)", response_time)
      result$metrics <- list(
        response_time_ms = response_time,
        api_status = content$status %||% "unknown"
      )
    } else {
      result$status <- "degraded"
      result$message <- sprintf("API returned status %d", httr::status_code(response))
    }
    
  }, error = function(e) {
    result$status <- "warning"
    result$message <- "API not available (may not be running)"
  })
  
  return(result)
}

#' Check authentication health
check_auth_health <- function() {
  
  result <- list(
    component = "auth",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  users_file <- "config/users.yaml"
  
  if (!file.exists(users_file)) {
    result$status <- "warning"
    result$message <- "Users configuration not found"
    return(result)
  }
  
  tryCatch({
    users <- yaml::read_yaml(users_file)
    
    user_count <- length(users$users)
    has_admin <- any(sapply(users$users, function(u) u$role == "admin"))
    has_plain_passwords <- any(sapply(users$users, function(u) !is.null(u$password)))
    
    if (has_plain_passwords) {
      result$status <- "critical"
      result$message <- "Plain text passwords detected - security risk!"
    } else if (!has_admin) {
      result$status <- "warning"
      result$message <- "No admin user configured"
    } else {
      result$status <- "healthy"
      result$message <- sprintf("%d users configured", user_count)
    }
    
    result$metrics <- list(
      user_count = user_count,
      has_admin = has_admin,
      has_plain_passwords = has_plain_passwords
    )
    
  }, error = function(e) {
    result$status <- "critical"
    result$message <- sprintf("Failed to load users: %s", e$message)
  })
  
  return(result)
}

#' Check cache health
check_cache_health <- function() {
  
  result <- list(
    component = "cache",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  cache_dir <- "cache/"
  
  if (!dir.exists(cache_dir)) {
    result$status <- "warning"
    result$message <- "Cache directory missing"
    return(result)
  }
  
  # Get cache statistics
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    result$status <- "healthy"
    result$message <- "Cache empty"
    result$metrics <- list(entries = 0, size_mb = 0)
    return(result)
  }
  
  # Calculate cache metrics
  cache_info <- file.info(cache_files)
  total_size_mb <- sum(cache_info$size) / 1024^2
  
  # Check age of cache entries
  cache_ages <- difftime(Sys.time(), cache_info$mtime, units = "hours")
  old_entries <- sum(cache_ages > 24)
  
  result$metrics <- list(
    entries = length(cache_files),
    size_mb = round(total_size_mb, 2),
    oldest_hours = round(max(cache_ages), 1),
    old_entries = old_entries
  )
  
  if (old_entries > length(cache_files) * 0.5) {
    result$status <- "warning"
    result$message <- sprintf("Many stale cache entries (%d/%d > 24h old)", 
                              old_entries, length(cache_files))
  } else {
    result$status <- "healthy"
    result$message <- sprintf("Cache healthy (%d entries, %.1f MB)", 
                              length(cache_files), total_size_mb)
  }
  
  return(result)
}

#' Check audit system health
check_audit_health <- function() {
  
  result <- list(
    component = "audit",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  audit_file <- "audit/audit_log.csv"
  
  if (!file.exists(audit_file)) {
    result$status <- "warning"
    result$message <- "Audit log not initialized"
    return(result)
  }
  
  tryCatch({
    # Check file size
    file_size_mb <- file.info(audit_file)$size / 1024^2
    
    # Read last few entries
    audit_data <- utils::tail(read.csv(audit_file, stringsAsFactors = FALSE), 100)
    
    # Calculate metrics
    entries_today <- sum(as.Date(audit_data$timestamp) == Sys.Date())
    unique_users <- length(unique(audit_data$user))
    
    result$metrics <- list(
      file_size_mb = round(file_size_mb, 2),
      total_entries = nrow(audit_data),
      entries_today = entries_today,
      unique_users = unique_users
    )
    
    if (file_size_mb > 100) {
      result$status <- "warning"
      result$message <- sprintf("Audit log large (%.1f MB) - consider archiving", file_size_mb)
    } else {
      result$status <- "healthy"
      result$message <- sprintf("Audit system operational (%d entries today)", entries_today)
    }
    
  }, error = function(e) {
    result$status <- "degraded"
    result$message <- sprintf("Cannot read audit log: %s", e$message)
  })
  
  return(result)
}

#' Check model health
check_model_health <- function() {
  
  result <- list(
    component = "models",
    status = "unknown",
    message = "",
    metrics = list(),
    checked_at = Sys.time()
  )
  
  # Check for required model files
  required_models <- c("pk_1cpt", "pk_2cpt", "pk_3cpt")
  
  # Check Stan models
  stan_dir <- "models/stan/"
  stan_available <- dir.exists(stan_dir)
  
  # Check JAGS models
  jags_dir <- "models/jags/"
  jags_available <- dir.exists(jags_dir)
  
  if (!stan_available && !jags_available) {
    result$status <- "critical"
    result$message <- "No model directories found"
    return(result)
  }
  
  # Count available models
  missing <- character()
  
  for (model in required_models) {
    stan_file <- file.path(stan_dir, paste0(model, ".stan"))
    jags_file <- file.path(jags_dir, paste0(model, ".bug"))
    
    if (!file.exists(stan_file) && !file.exists(jags_file)) {
      missing <- c(missing, model)
    }
  }
  
  result$metrics <- list(
    models_found = length(required_models) - length(missing),
    models_missing = length(missing),
    stan_available = stan_available,
    jags_available = jags_available
  )
  
  if (length(missing) > 0) {
    result$status <- "degraded"
    result$message <- sprintf("Missing models: %s", paste(missing, collapse = ", "))
  } else if (!stan_available || !jags_available) {
    result$status <- "warning"
    result$message <- sprintf("Backend missing - Stan: %s, JAGS: %s", 
                              stan_available, jags_available)
  } else {
    result$status <- "healthy"
    result$message <- "All models and backends available"
  }
  
  return(result)
}

#' Get current health status
#' @export
get_health_status <- function() {
  if (is.null(.health_status$current)) {
    run_health_checks(verbose = FALSE)
  }
  .health_status$current
}

#' Get health history
#' @export
get_health_history <- function(limit = 10) {
  history <- .health_status$history
  
  if (length(history) == 0) {
    return(data.frame())
  }
  
  if (length(history) > limit) {
    history <- history[1:limit]
  }
  
  # Convert to data frame
  do.call(rbind, lapply(history, function(h) {
    data.frame(
      timestamp = h$timestamp,
      overall_status = h$overall_status,
      duration_ms = h$duration_ms,
      healthy = sum(sapply(h$checks, function(c) c$status == "healthy")),
      warning = sum(sapply(h$checks, function(c) c$status == "warning")),
      degraded = sum(sapply(h$checks, function(c) c$status == "degraded")),
      critical = sum(sapply(h$checks, function(c) c$status == "critical")),
      stringsAsFactors = FALSE
    )
  }))
}

#' Print health summary
print_health_summary <- function(results) {
  
  cat("\n========== SYSTEM HEALTH CHECK ==========\n")
  cat(sprintf("Timestamp: %s\n", format(results$timestamp, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Overall Status: %s\n", toupper(results$overall_status)))
  cat(sprintf("Duration: %.0f ms\n\n", results$duration_ms))
  
  cat("Component Status:\n")
  cat("-----------------\n")
  
  for (check_name in names(results$checks)) {
    check <- results$checks[[check_name]]
    
    status_symbol <- switch(
      check$status,
      healthy = "✓",
      warning = "⚠",
      degraded = "⚡",
      critical = "✗",
      unknown = "?"
    )
    
    cat(sprintf("  %s %-15s: %-8s - %s\n",
                status_symbol,
                check$component,
                toupper(check$status),
                check$message))
  }
  
  cat("\n==========================================\n")
}

#' Log critical health issues
log_critical_health_issues <- function(results) {
  
  critical_components <- names(Filter(function(x) x$status == "critical", 
                                      results$checks))
  
  if (length(critical_components) > 0) {
    # Log to error monitor if available
    if (exists("log_error")) {
      for (component in critical_components) {
        check <- results$checks[[component]]
        
        log_error(
          error_type = "HEALTH_CHECK_CRITICAL",
          message = check$message,
          severity = "CRITICAL",
          module = paste0("health_check_", component),
          context = list(
            component = component,
            metrics = check$metrics
          )
        )
      }
    }
    
    # Write to health log
    log_file <- sprintf("log/health_critical_%s.log", format(Sys.Date(), "%Y%m%d"))
    
    log_entry <- sprintf(
      "[%s] CRITICAL: %s - Components: %s\n",
      format(results$timestamp, "%Y-%m-%d %H:%M:%S"),
      results$overall_status,
      paste(critical_components, collapse = ", ")
    )
    
    cat(log_entry, file = log_file, append = TRUE)
  }
}

#' Schedule automatic health checks
#' @export
schedule_health_checks <- function(interval_seconds = 60) {
  
  .health_config$check_interval_seconds <- interval_seconds
  
  run_scheduled_check <- function() {
    tryCatch({
      run_health_checks(verbose = FALSE)
    }, error = function(e) {
      warning(sprintf("Scheduled health check failed: %s", e$message))
    })
    
    # Reschedule
    later::later(run_scheduled_check, delay = interval_seconds)
  }
  
  # Start scheduling
  later::later(run_scheduled_check, delay = interval_seconds)
  
  message(sprintf("Health checks scheduled every %d seconds", interval_seconds))
}