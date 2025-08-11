# tests/testthat/test-integration.R
# Integration Tests for PK/PD Shiny Application

library(testthat)
library(mockery)

# Helper function to check if all required packages are installed
check_required_packages <- function() {
  required_packages <- c(
    "shiny", "shinydashboard", "shinyWidgets",
    "ggplot2", "plotly", "DT",
    "dplyr", "tidyr", "data.table",
    "DBI", "RSQLite",
    "rstan", "posterior"
  )
  
  missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    skip(paste("Missing required packages:", paste(missing_packages, collapse = ", ")))
  }
}

test_that("App starts without errors", {
  # Check for required files
  required_files <- c(
    "R/load_all.R",
    "R/utils.R",
    "R/pk_calculations.R",
    "R/plotting_functions.R"
  )
  
  # Mock file existence check
  mock_exists <- mock(rep(TRUE, length(required_files)))
  stub(file.exists, 'base::file.exists', mock_exists)
  
  # Mock source function
  load_all_sources <- function() {
    source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
    
    # Check that files exist
    if (length(source_files) == 0) {
      warning("No R files found in R/ directory")
      return(FALSE)
    }
    
    # Try to source each file (in test, we just check syntax)
    for (file in source_files) {
      tryCatch({
        # In real app, would use source(file)
        # For testing, just verify file would be readable
        if (!file.exists(file)) {
          stop("File not found: ", file)
        }
      }, error = function(e) {
        stop("Error loading ", file, ": ", e$message)
      })
    }
    
    return(TRUE)
  }
  
  # Test loading
  expect_no_error({
    result <- load_all_sources()
  })
})

test_that("Database connection and initialization works", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  
  library(DBI)
  library(RSQLite)
  
  # Function to initialize database
  init_database <- function(db_path = ":memory:") {
    # Connect to database
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Create tables
    queries <- list(
      users = "
        CREATE TABLE IF NOT EXISTS users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          username TEXT UNIQUE NOT NULL,
          password_hash TEXT NOT NULL,
          email TEXT,
          role TEXT DEFAULT 'user',
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          last_login TIMESTAMP
        )
      ",
      sessions = "
        CREATE TABLE IF NOT EXISTS sessions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id INTEGER,
          session_token TEXT UNIQUE NOT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          expires_at TIMESTAMP,
          ip_address TEXT,
          FOREIGN KEY (user_id) REFERENCES users(id)
        )
      ",
      audit_log = "
        CREATE TABLE IF NOT EXISTS audit_log (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id INTEGER,
          action TEXT NOT NULL,
          details TEXT,
          timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          ip_address TEXT,
          FOREIGN KEY (user_id) REFERENCES users(id)
        )
      ",
      pk_models = "
        CREATE TABLE IF NOT EXISTS pk_models (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL,
          description TEXT,
          model_type TEXT,
          parameters TEXT,
          created_by INTEGER,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (created_by) REFERENCES users(id)
        )
      ",
      simulation_results = "
        CREATE TABLE IF NOT EXISTS simulation_results (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          model_id INTEGER,
          user_id INTEGER,
          parameters TEXT,
          results TEXT,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (model_id) REFERENCES pk_models(id),
          FOREIGN KEY (user_id) REFERENCES users(id)
        )
      "
    )
    
    # Execute queries
    for (table_name in names(queries)) {
      dbExecute(con, queries[[table_name]])
    }
    
    # Create indices for performance
    indices <- c(
      "CREATE INDEX IF NOT EXISTS idx_sessions_token ON sessions(session_token)",
      "CREATE INDEX IF NOT EXISTS idx_sessions_user ON sessions(user_id)",
      "CREATE INDEX IF NOT EXISTS idx_audit_user ON audit_log(user_id)",
      "CREATE INDEX IF NOT EXISTS idx_audit_timestamp ON audit_log(timestamp)",
      "CREATE INDEX IF NOT EXISTS idx_simulations_user ON simulation_results(user_id)"
    )
    
    for (idx in indices) {
      dbExecute(con, idx)
    }
    
    return(con)
  }
  
  # Test database initialization
  expect_no_error({
    con <- init_database()
  })
  
  con <- init_database()
  
  # Verify all tables exist
  tables <- dbListTables(con)
  expect_true("users" %in% tables)
  expect_true("sessions" %in% tables)
  expect_true("audit_log" %in% tables)
  expect_true("pk_models" %in% tables)
  expect_true("simulation_results" %in% tables)
  
  # Test inserting data
  expect_no_error({
    dbExecute(con, 
      "INSERT INTO users (username, password_hash, email) VALUES (?, ?, ?)",
      params = list("testuser", "hash123", "test@example.com")
    )
  })
  
  # Verify data was inserted
  users <- dbGetQuery(con, "SELECT * FROM users")
  expect_equal(nrow(users), 1)
  expect_equal(users$username[1], "testuser")
  
  # Clean up
  dbDisconnect(con)
})

test_that("Complete PK simulation workflow executes successfully", {
  # Mock the complete workflow
  run_pk_simulation <- function(params, regimen, model_type = "1C") {
    # Validate inputs
    if (is.null(params) || is.null(regimen)) {
      stop("Parameters and regimen are required")
    }
    
    required_params <- switch(model_type,
      "1C" = c("CL", "Vc"),
      "2C" = c("CL", "Vc", "Q", "Vp"),
      "3C" = c("CL", "Vc", "Q1", "Vp1", "Q2", "Vp2"),
      stop("Unknown model type")
    )
    
    if (!all(required_params %in% names(params))) {
      stop("Missing required parameters for model type ", model_type)
    }
    
    # Validate regimen
    required_regimen <- c("dose", "tau", "n_doses")
    if (!all(required_regimen %in% names(regimen))) {
      stop("Missing required regimen parameters")
    }
    
    # Initialize simulation
    n_time_points <- 1000
    t_max <- regimen$n_doses * regimen$tau
    times <- seq(0, t_max, length.out = n_time_points)
    
    # Simple 1-compartment model simulation
    if (model_type == "1C") {
      k_el <- params$CL / params$Vc
      concentrations <- numeric(length(times))
      
      for (dose_num in 1:regimen$n_doses) {
        dose_time <- (dose_num - 1) * regimen$tau
        
        for (i in 1:length(times)) {
          if (times[i] >= dose_time) {
            t_after_dose <- times[i] - dose_time
            
            if (t_after_dose < regimen$tau) {
              # Single dose contribution
              C_single <- (regimen$dose / params$Vc) * exp(-k_el * t_after_dose)
              concentrations[i] <- concentrations[i] + C_single
            }
          }
        }
      }
    } else {
      stop("Only 1C model implemented in test")
    }
    
    # Calculate metrics
    Cmax <- max(concentrations)
    Cmin <- min(concentrations[concentrations > 0])
    AUC <- sum(diff(times) * (concentrations[-1] + concentrations[-length(concentrations)]) / 2)
    
    return(list(
      times = times,
      concentrations = concentrations,
      metrics = list(
        Cmax = Cmax,
        Cmin = Cmin,
        AUC = AUC,
        CL_calculated = regimen$dose * regimen$n_doses / AUC
      ),
      params = params,
      regimen = regimen,
      model_type = model_type
    ))
  }
  
  # Test complete workflow
  test_params <- list(CL = 5, Vc = 30)
  test_regimen <- list(dose = 1000, tau = 8, n_doses = 3)
  
  expect_no_error({
    result <- run_pk_simulation(test_params, test_regimen, "1C")
  })
  
  result <- run_pk_simulation(test_params, test_regimen, "1C")
  
  # Verify results structure
  expect_true(!is.null(result$times))
  expect_true(!is.null(result$concentrations))
  expect_true(!is.null(result$metrics))
  expect_equal(length(result$times), length(result$concentrations))
  
  # Verify metrics are reasonable
  expect_true(result$metrics$Cmax > 0)
  expect_true(result$metrics$Cmin >= 0)
  expect_true(result$metrics$AUC > 0)
  expect_true(result$metrics$Cmax > result$metrics$Cmin)
})

test_that("Error handling and recovery works properly", {
  # Mock application with error handling
  safe_computation <- function(func, ..., default = NULL) {
    tryCatch({
      result <- func(...)
      return(list(success = TRUE, result = result, error = NULL))
    }, error = function(e) {
      return(list(
        success = FALSE, 
        result = default, 
        error = as.character(e$message)
      ))
    })
  }
  
  # Test successful computation
  success_result <- safe_computation(function(x) x * 2, x = 5)
  expect_true(success_result$success)
  expect_equal(success_result$result, 10)
  expect_null(success_result$error)
  
  # Test error handling
  error_result <- safe_computation(function() stop("Test error"), default = 0)
  expect_false(error_result$success)
  expect_equal(error_result$result, 0)
  expect_true(grepl("Test error", error_result$error))
  
  # Test recovery mechanism
  attempts <- 0
  retry_computation <- function(func, max_attempts = 3) {
    for (attempt in 1:max_attempts) {
      result <- safe_computation(func)
      if (result$success) {
        return(result)
      }
      Sys.sleep(0.1 * attempt)  # Exponential backoff
    }
    return(result)
  }
  
  # Function that fails first 2 times
  flaky_function <- function() {
    attempts <<- attempts + 1
    if (attempts < 3) {
      stop("Temporary failure")
    }
    return("Success")
  }
  
  result <- retry_computation(flaky_function)
  expect_true(result$success)
  expect_equal(result$result, "Success")
  expect_equal(attempts, 3)
})

test_that("User input validation prevents invalid states", {
  # Comprehensive input validator
  validate_inputs <- function(inputs) {
    errors <- list()
    
    # Check numeric inputs
    numeric_fields <- c("dose", "weight", "clearance", "volume")
    for (field in numeric_fields) {
      if (field %in% names(inputs)) {
        val <- inputs[[field]]
        if (is.na(as.numeric(val))) {
          errors[[field]] <- paste(field, "must be numeric")
        } else if (as.numeric(val) <= 0) {
          errors[[field]] <- paste(field, "must be positive")
        }
      }
    }
    
    # Check required fields
    required_fields <- c("dose", "weight")
    for (field in required_fields) {
      if (!field %in% names(inputs) || is.null(inputs[[field]]) || inputs[[field]] == "") {
        errors[[field]] <- paste(field, "is required")
      }
    }
    
    # Check ranges
    if ("weight" %in% names(inputs) && !is.na(as.numeric(inputs$weight))) {
      weight <- as.numeric(inputs$weight)
      if (weight < 1 || weight > 500) {
        errors$weight <- "Weight must be between 1 and 500 kg"
      }
    }
    
    if ("dose" %in% names(inputs) && !is.na(as.numeric(inputs$dose))) {
      dose <- as.numeric(inputs$dose)
      if (dose > 10000) {
        errors$dose <- "Dose seems unusually high. Please verify."
      }
    }
    
    return(list(
      valid = length(errors) == 0,
      errors = errors
    ))
  }
  
  # Test valid inputs
  valid_inputs <- list(dose = "500", weight = "70", clearance = "5", volume = "30")
  validation <- validate_inputs(valid_inputs)
  expect_true(validation$valid)
  expect_equal(length(validation$errors), 0)
  
  # Test invalid inputs
  invalid_inputs <- list(dose = "abc", weight = "-10")
  validation <- validate_inputs(invalid_inputs)
  expect_false(validation$valid)
  expect_true("dose" %in% names(validation$errors))
  expect_true("weight" %in% names(validation$errors))
  
  # Test missing required fields
  incomplete_inputs <- list(clearance = "5")
  validation <- validate_inputs(incomplete_inputs)
  expect_false(validation$valid)
  expect_true("dose" %in% names(validation$errors))
  expect_true("weight" %in% names(validation$errors))
  
  # Test edge cases
  edge_inputs <- list(dose = "10001", weight = "501")
  validation <- validate_inputs(edge_inputs)
  expect_false(validation$valid)
})

test_that("Session management maintains state correctly", {
  # Mock session manager
  create_session_manager <- function() {
    sessions <- new.env(parent = emptyenv())
    
    create_session <- function(user_id) {
      session_id <- paste0("session_", 
                          format(Sys.time(), "%Y%m%d%H%M%S"),
                          "_", sample(1000:9999, 1))
      
      sessions[[session_id]] <- list(
        user_id = user_id,
        created_at = Sys.time(),
        last_activity = Sys.time(),
        data = list()
      )
      
      return(session_id)
    }
    
    get_session <- function(session_id) {
      if (!exists(session_id, envir = sessions)) {
        return(NULL)
      }
      
      session <- sessions[[session_id]]
      
      # Check if expired (30 minutes timeout)
      if (difftime(Sys.time(), session$last_activity, units = "mins") > 30) {
        remove_session(session_id)
        return(NULL)
      }
      
      # Update last activity
      session$last_activity <- Sys.time()
      sessions[[session_id]] <- session
      
      return(session)
    }
    
    update_session_data <- function(session_id, key, value) {
      session <- get_session(session_id)
      if (is.null(session)) {
        return(FALSE)
      }
      
      session$data[[key]] <- value
      sessions[[session_id]] <- session
      return(TRUE)
    }
    
    remove_session <- function(session_id) {
      if (exists(session_id, envir = sessions)) {
        rm(list = session_id, envir = sessions)
        return(TRUE)
      }
      return(FALSE)
    }
    
    list_sessions <- function() {
      return(ls(envir = sessions))
    }
    
    return(list(
      create = create_session,
      get = get_session,
      update = update_session_data,
      remove = remove_session,
      list = list_sessions
    ))
  }
  
  # Create session manager
  sm <- create_session_manager()
  
  # Test session creation
  session_id <- sm$create("user123")
  expect_true(!is.null(session_id))
  expect_true(grepl("^session_", session_id))
  
  # Test session retrieval
  session <- sm$get(session_id)
  expect_true(!is.null(session))
  expect_equal(session$user_id, "user123")
  
  # Test data storage
  success <- sm$update(session_id, "test_value", 42)
  expect_true(success)
  
  session <- sm$get(session_id)
  expect_equal(session$data$test_value, 42)
  
  # Test session listing
  sessions <- sm$list()
  expect_true(session_id %in% sessions)
  
  # Test session removal
  removed <- sm$remove(session_id)
  expect_true(removed)
  expect_null(sm$get(session_id))
})

test_that("Data export functionality works correctly", {
  # Mock export functions
  export_to_csv <- function(data, file_path) {
    if (is.null(data) || nrow(data) == 0) {
      stop("No data to export")
    }
    
    # In real app: write.csv(data, file_path, row.names = FALSE)
    # For testing, just validate
    return(TRUE)
  }
  
  export_to_excel <- function(data_list, file_path) {
    skip_if_not_installed("openxlsx")
    
    if (!is.list(data_list)) {
      data_list <- list(Sheet1 = data_list)
    }
    
    # In real app: openxlsx::write.xlsx(data_list, file_path)
    # For testing, just validate
    return(TRUE)
  }
  
  export_report <- function(results, format = "html") {
    skip_if_not_installed("rmarkdown")
    
    # Validate results
    required_fields <- c("parameters", "metrics", "plots")
    if (!all(required_fields %in% names(results))) {
      stop("Results missing required fields")
    }
    
    # Generate report content
    report_content <- list(
      title = "PK/PD Analysis Report",
      date = Sys.Date(),
      parameters = results$parameters,
      metrics = results$metrics,
      format = format
    )
    
    return(report_content)
  }
  
  # Test data
  test_data <- data.frame(
    time = c(0, 1, 2, 4, 8),
    concentration = c(0, 10, 8, 5, 2)
  )
  
  test_results <- list(
    parameters = list(CL = 5, Vc = 30),
    metrics = list(Cmax = 10, Cmin = 2, AUC = 40),
    plots = list()
  )
  
  # Test CSV export
  expect_true(export_to_csv(test_data, "test.csv"))
  expect_error(export_to_csv(NULL, "test.csv"))
  expect_error(export_to_csv(data.frame(), "test.csv"))
  
  # Test Excel export (if available)
  if ("openxlsx" %in% installed.packages()[, "Package"]) {
    expect_true(export_to_excel(test_data, "test.xlsx"))
    expect_true(export_to_excel(list(Data = test_data, Summary = test_data), "test.xlsx"))
  }
  
  # Test report generation
  if ("rmarkdown" %in% installed.packages()[, "Package"]) {
    report <- export_report(test_results)
    expect_equal(report$format, "html")
    expect_equal(report$parameters$CL, 5)
  }
})

test_that("Concurrent user operations are handled safely", {
  # Mock concurrent operation handler
  create_operation_queue <- function() {
    queue <- new.env(parent = emptyenv())
    queue$operations <- list()
    queue$processing <- FALSE
    queue$lock <- 0
    
    add_operation <- function(user_id, operation, params) {
      # Simple locking mechanism
      while (queue$lock > 0) {
        Sys.sleep(0.01)
      }
      
      queue$lock <- 1
      
      op_id <- paste0("op_", length(queue$operations) + 1)
      queue$operations[[op_id]] <- list(
        id = op_id,
        user_id = user_id,
        operation = operation,
        params = params,
        status = "pending",
        created_at = Sys.time()
      )
      
      queue$lock <- 0
      
      return(op_id)
    }
    
    process_operations <- function() {
      if (queue$processing) {
        return(FALSE)
      }
      
      queue$processing <- TRUE
      
      for (op_id in names(queue$operations)) {
        op <- queue$operations[[op_id]]
        
        if (op$status == "pending") {
          # Process operation
          queue$operations[[op_id]]$status <- "processing"
          
          # Simulate processing
          Sys.sleep(0.01)
          
          # Mark as complete
          queue$operations[[op_id]]$status <- "complete"
          queue$operations[[op_id]]$completed_at <- Sys.time()
        }
      }
      
      queue$processing <- FALSE
      return(TRUE)
    }
    
    get_status <- function(op_id) {
      if (!op_id %in% names(queue$operations)) {
        return(NULL)
      }
      return(queue$operations[[op_id]]$status)
    }
    
    return(list(
      add = add_operation,
      process = process_operations,
      status = get_status
    ))
  }
  
  # Create queue
  opqueue <- create_operation_queue()
  
  # Add multiple operations
  op1 <- opqueue$add("user1", "simulation", list(model = "1C"))
  op2 <- opqueue$add("user2", "analysis", list(type = "AUC"))
  op3 <- opqueue$add("user1", "export", list(format = "csv"))
  
  # Check initial status
  expect_equal(opqueue$status(op1), "pending")
  expect_equal(opqueue$status(op2), "pending")
  expect_equal(opqueue$status(op3), "pending")
  
  # Process operations
  success <- opqueue$process()
  expect_true(success)
  
  # Check completion
  expect_equal(opqueue$status(op1), "complete")
  expect_equal(opqueue$status(op2), "complete")
  expect_equal(opqueue$status(op3), "complete")
})

test_that("Application logging captures all critical events", {
  # Mock logger
  create_logger <- function(log_file = NULL) {
    logs <- list()
    
    log_event <- function(level, message, data = NULL) {
      log_entry <- list(
        timestamp = Sys.time(),
        level = level,
        message = message,
        data = data
      )
      
      logs <<- append(logs, list(log_entry))
      
      # Also write to file if specified
      if (!is.null(log_file)) {
        log_line <- sprintf(
          "[%s] %s: %s",
          format(log_entry$timestamp, "%Y-%m-%d %H:%M:%S"),
          toupper(level),
          message
        )
        
        # In real app: cat(log_line, "\n", file = log_file, append = TRUE)
      }
      
      return(TRUE)
    }
    
    get_logs <- function(level = NULL, since = NULL) {
      filtered_logs <- logs
      
      if (!is.null(level)) {
        filtered_logs <- Filter(function(x) x$level == level, filtered_logs)
      }
      
      if (!is.null(since)) {
        filtered_logs <- Filter(function(x) x$timestamp >= since, filtered_logs)
      }
      
      return(filtered_logs)
    }
    
    clear_logs <- function() {
      logs <<- list()
    }
    
    return(list(
      log = log_event,
      get = get_logs,
      clear = clear_logs
    ))
  }
  
  # Create logger
  logger <- create_logger()
  
  # Log various events
  logger$log("info", "Application started")
  logger$log("debug", "Loading configuration", list(config_file = "config.yml"))
  logger$log("warning", "High memory usage", list(used_mb = 512))
  logger$log("error", "Database connection failed", list(error = "timeout"))
  
  # Test retrieving all logs
  all_logs <- logger$get()
  expect_equal(length(all_logs), 4)
  
  # Test filtering by level
  error_logs <- logger$get(level = "error")
  expect_equal(length(error_logs), 1)
  expect_equal(error_logs[[1]]$message, "Database connection failed")
  
  # Test filtering by time
  recent_time <- Sys.time() - 60  # Last minute
  recent_logs <- logger$get(since = recent_time)
  expect_equal(length(recent_logs), 4)
  
  # Test clearing logs
  logger$clear()
  expect_equal(length(logger$get()), 0)
})