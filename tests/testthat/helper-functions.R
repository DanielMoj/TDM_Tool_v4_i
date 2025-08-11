# tests/testthat/helper-functions.R
# Helper functions and data generators for tests

library(data.table)
library(jsonlite)

# ═══════════════════════════════════════════════════════════════════
# Test Data Generators
# ═══════════════════════════════════════════════════════════════════

#' Generate synthetic PK data
#'
#' @param n_patients Number of patients
#' @param n_timepoints Number of time points per patient
#' @param model_type PK model type ("1C", "2C", "3C")
#' @param add_noise Add random noise to concentrations
#' @return data.table with PK data
generate_pk_data <- function(n_patients = 10, 
                            n_timepoints = 50,
                            model_type = "1C",
                            add_noise = TRUE) {
  
  # Time points
  times <- seq(0, 24, length.out = n_timepoints)
  
  # Generate data for each patient
  all_data <- list()
  
  for (i in 1:n_patients) {
    # Random PK parameters
    if (model_type == "1C") {
      CL <- rlnorm(1, log(5), 0.3)
      Vc <- rlnorm(1, log(30), 0.3)
      k_el <- CL / Vc
      
      # Single dose concentration profile
      dose <- 1000
      conc <- (dose / Vc) * exp(-k_el * times)
      
    } else if (model_type == "2C") {
      CL <- rlnorm(1, log(5), 0.3)
      Vc <- rlnorm(1, log(30), 0.3)
      Q <- rlnorm(1, log(2), 0.3)
      Vp <- rlnorm(1, log(20), 0.3)
      
      # Simplified 2-compartment
      k10 <- CL / Vc
      k12 <- Q / Vc
      k21 <- Q / Vp
      
      # Bi-exponential decline
      alpha <- 0.5 * (k10 + k12 + k21 + sqrt((k10 + k12 + k21)^2 - 4 * k10 * k21))
      beta <- 0.5 * (k10 + k12 + k21 - sqrt((k10 + k12 + k21)^2 - 4 * k10 * k21))
      
      A <- 1000 / Vc * (alpha - k21) / (alpha - beta)
      B <- 1000 / Vc * (k21 - beta) / (alpha - beta)
      
      conc <- A * exp(-alpha * times) + B * exp(-beta * times)
      
    } else {
      stop("Model type not implemented in test generator")
    }
    
    # Add noise if requested
    if (add_noise) {
      conc <- conc * rlnorm(length(conc), 0, 0.1)
      conc[conc < 0] <- 0
    }
    
    # Create patient data
    patient_data <- data.table(
      patient_id = i,
      time = times,
      concentration = conc,
      dose = dose,
      model = model_type
    )
    
    all_data[[i]] <- patient_data
  }
  
  # Combine all patients
  result <- rbindlist(all_data)
  
  # Add additional columns
  result[, `:=`(
    study = "TEST001",
    visit = ifelse(time <= 12, "Day 1", "Day 2"),
    analyte = "Drug",
    unit = "mg/L"
  )]
  
  return(result)
}

#' Generate dosing regimen
#'
#' @param type Regimen type ("single", "multiple", "steady_state")
#' @param route Administration route ("iv", "oral")
#' @return List with regimen parameters
generate_regimen <- function(type = "multiple", route = "iv") {
  
  base_regimen <- list(
    dose = sample(c(100, 250, 500, 1000), 1),
    tau = sample(c(6, 8, 12, 24), 1),
    route = route
  )
  
  if (type == "single") {
    base_regimen$n_doses <- 1
    base_regimen$duration <- base_regimen$tau
    
  } else if (type == "multiple") {
    base_regimen$n_doses <- sample(3:10, 1)
    base_regimen$duration <- base_regimen$n_doses * base_regimen$tau
    
  } else if (type == "steady_state") {
    base_regimen$n_doses <- 20  # Enough to reach steady state
    base_regimen$duration <- base_regimen$n_doses * base_regimen$tau
    base_regimen$steady_state <- TRUE
  }
  
  # Add route-specific parameters
  if (route == "iv") {
    base_regimen$tinf <- runif(1, 0.5, 2)  # Infusion duration
  } else if (route == "oral") {
    base_regimen$ka <- rlnorm(1, log(1), 0.3)  # Absorption rate
    base_regimen$F <- runif(1, 0.5, 1)  # Bioavailability
    base_regimen$tlag <- runif(1, 0, 0.5)  # Lag time
  }
  
  return(base_regimen)
}

#' Generate patient covariates
#'
#' @param n Number of patients
#' @return data.frame with patient covariates
generate_covariates <- function(n = 100) {
  
  covariates <- data.frame(
    patient_id = 1:n,
    age = round(rnorm(n, 45, 15)),
    weight = round(rlnorm(n, log(70), 0.2), 1),
    height = round(rnorm(n, 170, 10)),
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.5, 0.5)),
    race = sample(c("White", "Black", "Asian", "Other"), n, 
                  replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05)),
    creatinine = round(rlnorm(n, log(1), 0.3), 2),
    albumin = round(rnorm(n, 4, 0.5), 1),
    alt = round(rlnorm(n, log(30), 0.4)),
    ast = round(rlnorm(n, log(25), 0.4))
  )
  
  # Calculate derived covariates
  covariates$bmi <- round(covariates$weight / (covariates$height/100)^2, 1)
  covariates$bsa <- round(sqrt(covariates$height * covariates$weight / 3600), 2)
  
  # Calculate creatinine clearance (Cockcroft-Gault)
  covariates$crcl <- with(covariates, {
    base_crcl <- (140 - age) * weight / (72 * creatinine)
    ifelse(sex == "F", base_crcl * 0.85, base_crcl)
  })
  covariates$crcl <- round(covariates$crcl, 1)
  
  # Add categorical renal function
  covariates$renal_function <- cut(
    covariates$crcl,
    breaks = c(0, 30, 60, 90, Inf),
    labels = c("Severe", "Moderate", "Mild", "Normal")
  )
  
  return(covariates)
}

# ═══════════════════════════════════════════════════════════════════
# Mock Functions for Testing
# ═══════════════════════════════════════════════════════════════════

#' Mock database connection
#'
#' @return Mock DBI connection object
mock_db_connection <- function() {
  structure(
    list(
      dbname = ":memory:",
      host = "localhost",
      user = "test_user",
      connected = TRUE
    ),
    class = "MockDBIConnection"
  )
}

#' Mock Stan model
#'
#' @param model_type Model type string
#' @return Mock Stan model object
mock_stan_model <- function(model_type = "pk_1cpt") {
  structure(
    list(
      model_name = model_type,
      model_code = "// Mock Stan code",
      compiled = TRUE,
      compile_time = Sys.time()
    ),
    class = "MockStanModel"
  )
}

#' Mock Shiny session
#'
#' @return Mock Shiny session object
mock_shiny_session <- function() {
  session <- list(
    token = paste0("mock_token_", sample(1000:9999, 1)),
    user = "test_user",
    groups = c("users"),
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = 3838,
      url_pathname = "/test/"
    ),
    input = list(),
    output = list(),
    userData = new.env(),
    ns = function(x) x,
    sendCustomMessage = function(type, message) NULL,
    onSessionEnded = function(callback) NULL
  )
  
  class(session) <- "MockShinySession"
  return(session)
}

# ═══════════════════════════════════════════════════════════════════
# Assertion Helpers
# ═══════════════════════════════════════════════════════════════════

#' Assert that data.frame has expected structure
#'
#' @param df Data frame to check
#' @param expected_cols Expected column names
#' @param min_rows Minimum number of rows
expect_valid_dataframe <- function(df, expected_cols = NULL, min_rows = 0) {
  expect_true(is.data.frame(df) || is.data.table(df))
  expect_gte(nrow(df), min_rows)
  
  if (!is.null(expected_cols)) {
    expect_true(all(expected_cols %in% names(df)))
  }
}

#' Assert that numeric value is within range
#'
#' @param value Numeric value to check
#' @param min Minimum value
#' @param max Maximum value
expect_in_range <- function(value, min, max) {
  expect_true(is.numeric(value))
  expect_gte(value, min)
  expect_lte(value, max)
}

#' Assert that file was created
#'
#' @param file_path Path to file
#' @param min_size Minimum file size in bytes
expect_file_created <- function(file_path, min_size = 0) {
  expect_true(file.exists(file_path))
  
  if (min_size > 0) {
    expect_gte(file.info(file_path)$size, min_size)
  }
}

# ═══════════════════════════════════════════════════════════════════
# Test Fixtures
# ═══════════════════════════════════════════════════════════════════

#' Get path to test fixture
#'
#' @param filename Name of fixture file
#' @return Full path to fixture file
fixture_path <- function(filename) {
  file.path("tests", "testthat", "fixtures", filename)
}

#' Load test fixture
#'
#' @param name Name of fixture (without extension)
#' @return Loaded fixture data
load_fixture <- function(name) {
  fixture_file <- fixture_path(paste0(name, ".rds"))
  
  if (!file.exists(fixture_file)) {
    stop("Fixture not found: ", fixture_file)
  }
  
  readRDS(fixture_file)
}

#' Save test fixture
#'
#' @param data Data to save
#' @param name Name for fixture (without extension)
save_fixture <- function(data, name) {
  fixture_dir <- file.path("tests", "testthat", "fixtures")
  
  if (!dir.exists(fixture_dir)) {
    dir.create(fixture_dir, recursive = TRUE)
  }
  
  fixture_file <- file.path(fixture_dir, paste0(name, ".rds"))
  saveRDS(data, fixture_file)
  
  message("Fixture saved: ", fixture_file)
}

# ═══════════════════════════════════════════════════════════════════
# Performance Testing Helpers
# ═══════════════════════════════════════════════════════════════════

#' Measure execution time
#'
#' @param expr Expression to time
#' @param units Time units ("secs", "mins", "hours")
#' @return Numeric time in specified units
measure_time <- function(expr, units = "secs") {
  start_time <- Sys.time()
  eval(expr)
  end_time <- Sys.time()
  
  as.numeric(difftime(end_time, start_time, units = units))
}

#' Check memory usage
#'
#' @param expr Expression to evaluate
#' @return List with memory statistics
measure_memory <- function(expr) {
  gc_before <- gc(reset = TRUE)
  result <- eval(expr)
  gc_after <- gc()
  
  list(
    result = result,
    memory_used_mb = sum(gc_after[, 2] - gc_before[, 2]),
    peak_memory_mb = sum(gc_after[, 6])
  )
}

# ═══════════════════════════════════════════════════════════════════
# Database Testing Helpers
# ═══════════════════════════════════════════════════════════════════

#' Create test database with schema
#'
#' @return DBI connection to test database
create_test_database <- function() {
  library(DBI)
  library(RSQLite)
  
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Create tables
  dbExecute(con, "
    CREATE TABLE users (
      id INTEGER PRIMARY KEY,
      username TEXT UNIQUE,
      password_hash TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE pk_data (
      id INTEGER PRIMARY KEY,
      patient_id INTEGER,
      time REAL,
      concentration REAL,
      dose REAL
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE audit_log (
      id INTEGER PRIMARY KEY,
      user_id INTEGER,
      action TEXT,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  return(con)
}

#' Clean up test database
#'
#' @param con Database connection
cleanup_test_database <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}

# ═══════════════════════════════════════════════════════════════════
# Shiny Testing Helpers
# ═══════════════════════════════════════════════════════════════════

#' Create mock input values for Shiny
#'
#' @return List of input values
mock_shiny_inputs <- function() {
  list(
    dose = 1000,
    weight = 70,
    clearance = 5,
    volume = 30,
    model_type = "1C",
    n_simulations = 100,
    run_simulation = 0,
    export_results = 0
  )
}

#' Simulate reactive values
#'
#' @param initial_values Initial values for reactives
#' @return Environment with reactive-like behavior
mock_reactive_values <- function(initial_values = list()) {
  values <- as.environment(initial_values)
  class(values) <- c("MockReactiveValues", class(values))
  return(values)
}

# ═══════════════════════════════════════════════════════════════════
# Cleanup Functions
# ═══════════════════════════════════════════════════════════════════

#' Clean up temporary test files
clean_temp_files <- function() {
  temp_pattern <- "^test_.*\\.(csv|rds|json|html)$"
  temp_files <- list.files(tempdir(), pattern = temp_pattern, full.names = TRUE)
  
  if (length(temp_files) > 0) {
    unlink(temp_files)
    message("Cleaned up ", length(temp_files), " temporary test files")
  }
}

# Register cleanup on exit
reg.finalizer(
  .GlobalEnv,
  function(e) clean_temp_files(),
  onexit = TRUE
)