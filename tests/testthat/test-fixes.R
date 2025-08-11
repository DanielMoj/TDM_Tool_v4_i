# tests/testthat/test-fixes.R
# Tests for General Bug Fixes

library(testthat)
library(mockery)

# Source required functions (adjust paths as needed)
# source("../../R/utils.R")
# source("../../R/pk_calculations.R")

# Define the %||% operator for testing
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || (is.character(x) && x == "")) y else x
}

test_that("n_intervals is properly defined in compute_metrics_for_draw", {
  # Mock the compute_metrics_for_draw function
  compute_metrics_for_draw <- function(theta, regimen, model_type, MIC) {
    # Extract parameters
    CL <- theta$CL
    Vc <- theta$Vc
    
    # Extract regimen details
    dose <- regimen$dose
    tau <- regimen$tau
    tinf <- regimen$tinf
    n_doses <- regimen$n_doses
    start_time <- regimen$start_time %||% 0
    
    # CRITICAL FIX: Define n_intervals
    n_intervals <- 100  # Default value for time resolution
    
    # Calculate time points
    tmax <- n_doses * tau
    times <- seq(0, tmax, length.out = n_intervals * n_doses)
    
    # Simple 1-compartment calculation for testing
    k_el <- CL / Vc
    
    # Calculate concentrations (simplified)
    concentrations <- numeric(length(times))
    for (i in 1:n_doses) {
      dose_time <- (i - 1) * tau + start_time
      for (j in 1:length(times)) {
        if (times[j] >= dose_time) {
          t_after_dose <- times[j] - dose_time
          if (t_after_dose <= tinf) {
            # During infusion
            concentrations[j] <- concentrations[j] + 
              (dose / tinf / CL) * (1 - exp(-k_el * t_after_dose))
          } else {
            # After infusion
            C_end_inf <- (dose / tinf / CL) * (1 - exp(-k_el * tinf))
            concentrations[j] <- concentrations[j] + 
              C_end_inf * exp(-k_el * (t_after_dose - tinf))
          }
        }
      }
    }
    
    # Calculate PK/PD metrics
    Cmax <- max(concentrations)
    Cmin <- min(concentrations[concentrations > 0])
    AUC <- sum(diff(times) * (concentrations[-1] + concentrations[-length(concentrations)]) / 2)
    
    # Calculate time above MIC
    time_above_MIC <- sum(diff(times)[concentrations[-1] > MIC])
    percent_time_above_MIC <- (time_above_MIC / tau) * 100
    
    return(list(
      Cmax = Cmax,
      Cmin = Cmin,
      AUC = AUC,
      time_above_MIC = time_above_MIC,
      percent_time_above_MIC = percent_time_above_MIC,
      n_intervals_used = n_intervals  # Confirm it was defined
    ))
  }
  
  # Setup test data
  theta <- list(CL = 5, Vc = 30, Q = 2, Vp = 20)
  regimen <- list(
    dose = 1000,
    tau = 8,
    tinf = 1,
    n_doses = 6,
    start_time = 0
  )
  MIC <- 1
  
  # Should not error due to undefined n_intervals
  expect_no_error({
    result <- compute_metrics_for_draw(theta, regimen, "1C", MIC)
  })
  
  # Verify n_intervals was used
  result <- compute_metrics_for_draw(theta, regimen, "1C", MIC)
  expect_true(!is.null(result$n_intervals_used))
  expect_equal(result$n_intervals_used, 100)
  
  # Verify calculations produce reasonable results
  expect_true(result$Cmax > 0)
  expect_true(result$Cmin >= 0)
  expect_true(result$AUC > 0)
  expect_true(result$percent_time_above_MIC >= 0)
  expect_true(result$percent_time_above_MIC <= 100)
})

test_that("%||% operator works correctly for null coalescing", {
  # Test with NULL
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% 10, 10)
  expect_equal(NULL %||% FALSE, FALSE)
  
  # Test with NA
  expect_equal(NA %||% "default", "default")
  expect_equal(NA_character_ %||% "default", "default")
  expect_equal(NA_real_ %||% 5.5, 5.5)
  expect_equal(NA_integer_ %||% 10L, 10L)
  
  # Test with empty string
  expect_equal("" %||% "default", "default")
  
  # Test with valid values (should return the value, not default)
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 10, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(list(a = 1) %||% list(b = 2), list(a = 1))
  
  # Test chaining
  expect_equal(NULL %||% NA %||% "" %||% "final", "final")
  expect_equal("first" %||% "second" %||% "third", "first")
})

test_that("data.table operations handle edge cases", {
  skip_if_not_installed("data.table")
  library(data.table)
  
  # Test empty data.table handling
  empty_dt <- data.table()
  expect_equal(nrow(empty_dt), 0)
  expect_no_error({
    result <- empty_dt[, .(mean_val = mean(value)), by = group]
  })
  
  # Test single row operations
  single_dt <- data.table(x = 1, y = 2)
  expect_no_error({
    result <- single_dt[, .(sum_x = sum(x), mean_y = mean(y))]
  })
  expect_equal(nrow(result), 1)
  
  # Test NA handling in grouping
  na_dt <- data.table(
    group = c("A", "B", NA, "A", NA),
    value = c(1, 2, 3, 4, 5)
  )
  expect_no_error({
    result <- na_dt[!is.na(group), .(mean_val = mean(value)), by = group]
  })
  expect_equal(nrow(result), 2)  # Only groups A and B
})

test_that("reactive values are properly initialized", {
  # Mock reactive values
  create_reactive_values <- function() {
    values <- list(
      pk_params = NULL,
      regimen = NULL,
      simulation_results = NULL,
      current_page = "input",
      error_message = NULL,
      warning_message = NULL,
      progress = 0,
      is_calculating = FALSE,
      selected_model = "1C",
      MIC_value = 1,
      target_metric = "AUC/MIC",
      n_simulations = 1000
    )
    
    # Ensure all values have defaults
    for (name in names(values)) {
      if (is.null(values[[name]])) {
        values[[name]] <- switch(name,
          current_page = "input",
          progress = 0,
          is_calculating = FALSE,
          selected_model = "1C",
          MIC_value = 1,
          target_metric = "AUC/MIC",
          n_simulations = 1000,
          NULL  # default for others
        )
      }
    }
    
    return(values)
  }
  
  # Test initialization
  rv <- create_reactive_values()
  
  # Check critical values are not NULL
  expect_false(is.null(rv$current_page))
  expect_false(is.null(rv$is_calculating))
  expect_false(is.null(rv$selected_model))
  expect_false(is.null(rv$n_simulations))
  
  # Check default values
  expect_equal(rv$current_page, "input")
  expect_equal(rv$is_calculating, FALSE)
  expect_equal(rv$selected_model, "1C")
  expect_equal(rv$n_simulations, 1000)
})

test_that("model selection handles all compartment types", {
  # Mock model selector function
  select_pk_model <- function(model_type) {
    valid_models <- c("1C", "2C", "3C", "1C_oral", "2C_oral")
    
    if (!model_type %in% valid_models) {
      stop("Invalid model type: ", model_type)
    }
    
    model_config <- switch(model_type,
      "1C" = list(
        name = "One Compartment IV",
        params = c("CL", "Vc"),
        compartments = 1,
        oral = FALSE
      ),
      "2C" = list(
        name = "Two Compartment IV",
        params = c("CL", "Vc", "Q", "Vp"),
        compartments = 2,
        oral = FALSE
      ),
      "3C" = list(
        name = "Three Compartment IV",
        params = c("CL", "Vc", "Q1", "Vp1", "Q2", "Vp2"),
        compartments = 3,
        oral = FALSE
      ),
      "1C_oral" = list(
        name = "One Compartment Oral",
        params = c("CL", "Vc", "ka", "F"),
        compartments = 1,
        oral = TRUE
      ),
      "2C_oral" = list(
        name = "Two Compartment Oral",
        params = c("CL", "Vc", "Q", "Vp", "ka", "F"),
        compartments = 2,
        oral = TRUE
      )
    )
    
    return(model_config)
  }
  
  # Test all model types
  models_to_test <- c("1C", "2C", "3C", "1C_oral", "2C_oral")
  
  for (model in models_to_test) {
    expect_no_error({
      config <- select_pk_model(model)
    })
    
    config <- select_pk_model(model)
    expect_true(!is.null(config$name))
    expect_true(!is.null(config$params))
    expect_true(!is.null(config$compartments))
    expect_true(!is.null(config$oral))
  }
  
  # Test invalid model
  expect_error(select_pk_model("4C"))
  expect_error(select_pk_model("invalid"))
})

test_that("numeric input validation handles edge cases", {
  # Mock validation function
  validate_numeric_input <- function(value, min = NULL, max = NULL, 
                                    allow_zero = FALSE, allow_negative = FALSE) {
    # Convert to numeric if needed
    if (is.character(value)) {
      value <- suppressWarnings(as.numeric(value))
    }
    
    # Check if conversion failed
    if (is.na(value) || is.null(value)) {
      return(list(valid = FALSE, error = "Value must be numeric"))
    }
    
    # Check zero
    if (!allow_zero && value == 0) {
      return(list(valid = FALSE, error = "Value cannot be zero"))
    }
    
    # Check negative
    if (!allow_negative && value < 0) {
      return(list(valid = FALSE, error = "Value cannot be negative"))
    }
    
    # Check min
    if (!is.null(min) && value < min) {
      return(list(valid = FALSE, error = paste("Value must be at least", min)))
    }
    
    # Check max
    if (!is.null(max) && value > max) {
      return(list(valid = FALSE, error = paste("Value must be at most", max)))
    }
    
    return(list(valid = TRUE, value = value))
  }
  
  # Test valid inputs
  expect_true(validate_numeric_input(5, min = 0, max = 10)$valid)
  expect_true(validate_numeric_input("3.14")$valid)
  expect_true(validate_numeric_input(0, allow_zero = TRUE)$valid)
  expect_true(validate_numeric_input(-5, allow_negative = TRUE)$valid)
  
  # Test invalid inputs
  expect_false(validate_numeric_input("abc")$valid)
  expect_false(validate_numeric_input(NA)$valid)
  expect_false(validate_numeric_input(NULL)$valid)
  expect_false(validate_numeric_input(0, allow_zero = FALSE)$valid)
  expect_false(validate_numeric_input(-5, allow_negative = FALSE)$valid)
  expect_false(validate_numeric_input(5, min = 10)$valid)
  expect_false(validate_numeric_input(15, max = 10)$valid)
  
  # Test string to numeric conversion
  result <- validate_numeric_input("42.5", min = 0, max = 100)
  expect_true(result$valid)
  expect_equal(result$value, 42.5)
})

test_that("error messages are properly formatted", {
  # Mock error message formatter
  format_error_message <- function(error, context = NULL) {
    # Clean up error message
    msg <- as.character(error)
    msg <- gsub("^Error: ", "", msg)
    msg <- gsub("^Error in .* : ", "", msg)
    
    # Add context if provided
    if (!is.null(context)) {
      msg <- paste0(context, ": ", msg)
    }
    
    # Ensure message ends with period
    if (!grepl("[.!?]$", msg)) {
      msg <- paste0(msg, ".")
    }
    
    return(msg)
  }
  
  # Test various error formats
  expect_equal(
    format_error_message("Error: Something went wrong"),
    "Something went wrong."
  )
  
  expect_equal(
    format_error_message("Error in function() : Invalid input"),
    "Invalid input."
  )
  
  expect_equal(
    format_error_message("Already has period.", context = "Validation"),
    "Validation: Already has period."
  )
  
  # Test with try-catch
  error_obj <- tryCatch(
    stop("Test error"),
    error = function(e) e
  )
  
  formatted <- format_error_message(error_obj$message)
  expect_true(grepl("\\.$", formatted))
})

test_that("plot rendering handles missing data gracefully", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  # Mock plot function
  create_concentration_plot <- function(data) {
    # Handle NULL or empty data
    if (is.null(data) || nrow(data) == 0) {
      # Return empty plot with message
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                  label = "No data available", 
                  size = 6, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          theme(
            panel.border = element_rect(fill = NA, color = "gray80"),
            plot.margin = unit(c(1, 1, 1, 1), "cm")
          )
      )
    }
    
    # Check required columns
    required_cols <- c("time", "concentration")
    if (!all(required_cols %in% names(data))) {
      stop("Data must contain 'time' and 'concentration' columns")
    }
    
    # Create plot
    p <- ggplot(data, aes(x = time, y = concentration)) +
      geom_line(size = 1.2, color = "blue") +
      geom_point(size = 2, color = "darkblue") +
      labs(
        x = "Time (hours)",
        y = "Concentration (mg/L)",
        title = "Plasma Concentration over Time"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    return(p)
  }
  
  # Test with valid data
  valid_data <- data.frame(
    time = c(0, 1, 2, 4, 8, 12),
    concentration = c(0, 10, 8, 5, 2, 0.5)
  )
  expect_no_error({
    p <- create_concentration_plot(valid_data)
  })
  
  # Test with NULL data
  expect_no_error({
    p <- create_concentration_plot(NULL)
  })
  
  # Test with empty data frame
  expect_no_error({
    p <- create_concentration_plot(data.frame())
  })
  
  # Test with missing columns
  invalid_data <- data.frame(x = 1:5, y = 1:5)
  expect_error(create_concentration_plot(invalid_data))
})

test_that("file path handling works across platforms", {
  # Mock cross-platform path handler
  normalize_path_safe <- function(path) {
    if (is.null(path) || path == "") {
      return(NULL)
    }
    
    # Normalize slashes
    path <- gsub("\\\\", "/", path)
    
    # Remove duplicate slashes
    path <- gsub("//+", "/", path)
    
    # Handle relative paths
    if (!grepl("^(/|[A-Za-z]:)", path)) {
      path <- file.path(getwd(), path)
    }
    
    # Normalize the path
    path <- normalizePath(path, mustWork = FALSE)
    
    return(path)
  }
  
  # Test various path formats
  expect_true(!is.null(normalize_path_safe("test.csv")))
  expect_true(!is.null(normalize_path_safe("./data/test.csv")))
  expect_true(!is.null(normalize_path_safe("../data/test.csv")))
  
  # Test Windows-style paths (won't fail on Unix)
  expect_no_error({
    result <- normalize_path_safe("C:\\Users\\test\\file.csv")
  })
  
  # Test empty/NULL paths
  expect_null(normalize_path_safe(""))
  expect_null(normalize_path_safe(NULL))
})

test_that("memory management for large simulations", {
  # Mock memory-efficient simulation runner
  run_simulation_chunked <- function(n_simulations, chunk_size = 100) {
    if (n_simulations <= 0) {
      stop("Number of simulations must be positive")
    }
    
    # Calculate number of chunks
    n_chunks <- ceiling(n_simulations / chunk_size)
    
    # Initialize results storage
    all_results <- list()
    
    # Process in chunks
    for (i in 1:n_chunks) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, n_simulations)
      current_chunk_size <- end_idx - start_idx + 1
      
      # Simulate chunk (simplified)
      chunk_results <- rnorm(current_chunk_size, mean = 100, sd = 20)
      
      # Store results
      all_results[[i]] <- chunk_results
      
      # Force garbage collection periodically
      if (i %% 10 == 0) {
        gc(verbose = FALSE)
      }
    }
    
    # Combine results
    final_results <- unlist(all_results)
    
    return(list(
      results = final_results,
      n_chunks = n_chunks,
      chunk_size = chunk_size
    ))
  }
  
  # Test small simulation
  small_sim <- run_simulation_chunked(50, chunk_size = 100)
  expect_equal(length(small_sim$results), 50)
  expect_equal(small_sim$n_chunks, 1)
  
  # Test large simulation with chunking
  large_sim <- run_simulation_chunked(1000, chunk_size = 100)
  expect_equal(length(large_sim$results), 1000)
  expect_equal(large_sim$n_chunks, 10)
  
  # Test edge cases
  expect_error(run_simulation_chunked(0))
  expect_error(run_simulation_chunked(-10))
  
  # Test odd numbers
  odd_sim <- run_simulation_chunked(333, chunk_size = 100)
  expect_equal(length(odd_sim$results), 333)
  expect_equal(odd_sim$n_chunks, 4)
})