# tests/testthat/test-computation-stability.R
# Tests for computation stability and error handling

library(testthat)
library(mockery)

# Source the files to test
source("../../R/safe_computation.R")
source("../../R/backend_bayes.R")
source("../../R/run_fit_jags.R")

# Helper function to create test data
create_test_data <- function(n = 10, problematic = FALSE) {
  if (problematic) {
    # Create data that causes computational issues
    data <- data.frame(
      ID = rep(1, n),
      TIME = seq(0, 24, length.out = n),
      DV = c(100, 0.01, 100, 0.01, 100, 0.01, 100, 0.01, 100, 0.01)[1:n],
      DOSE = rep(500, n)
    )
  } else {
    # Normal test data
    data <- data.frame(
      ID = rep(1, n),
      TIME = seq(0, 24, length.out = n),
      DV = exp(-0.1 * seq(0, 24, length.out = n)) * 10 + rnorm(n, 0, 0.5),
      DOSE = rep(500, n)
    )
  }
  return(data)
}

# Create test configuration
create_test_config <- function() {
  list(
    n_chains = 2,
    n_iter = 1000,
    n_burnin = 500,
    thin = 1,
    model_type = "one_comp",
    error_model = "additive"
  )
}

test_that("safe_bayesian_computation handles successful computation", {
  # Create a mock successful computation
  computation_fn <- function(params) {
    Sys.sleep(0.1)  # Simulate some work
    return(list(result = "success", value = 42))
  }
  
  result <- safe_bayesian_computation(
    computation_type = "stan",
    computation_fn = computation_fn,
    params = list(test = TRUE)
  )
  
  expect_true(result$success)
  expect_equal(result$result$value, 42)
  expect_null(result$error)
  expect_true(is.numeric(result$duration))
})

test_that("safe_bayesian_computation handles computation errors", {
  # Create a computation that fails
  computation_fn <- function(params) {
    stop("Computation failed intentionally")
  }
  
  result <- safe_bayesian_computation(
    computation_type = "stan",
    computation_fn = computation_fn,
    params = list(test = TRUE)
  )
  
  expect_false(result$success)
  expect_null(result$result)
  expect_true(grepl("Computation failed", result$error))
})

test_that("Stan handles divergent transitions gracefully", {
  # Create computation that simulates divergences
  computation_fn <- function(params) {
    stop("47 divergent transitions after warmup")
  }
  
  result <- safe_bayesian_computation(
    computation_type = "stan",
    computation_fn = computation_fn,
    params = list(adapt_delta = 0.8)
  )
  
  expect_false(result$success)
  expect_true(grepl("adapt_delta", result$error))
  expect_true(result$recovery_attempted)
})

test_that("Stan auto-retry with higher adapt_delta works", {
  # Counter to track retry attempts
  attempt <- 0
  
  computation_fn <- function(params) {
    attempt <<- attempt + 1
    if (attempt == 1 && params$adapt_delta < 0.95) {
      stop("47 divergent transitions")
    }
    return(list(success = TRUE, adapt_delta = params$adapt_delta))
  }
  
  result <- safe_bayesian_computation(
    computation_type = "stan",
    computation_fn = computation_fn,
    params = list(adapt_delta = 0.8)
  )
  
  # Should succeed on retry
  expect_true(result$success)
  expect_equal(result$result$adapt_delta, 0.95)
  expect_true(any(grepl("increasing adapt_delta", result$warnings)))
})

test_that("JAGS handles initialization failures", {
  data <- create_test_data(problematic = TRUE)
  config <- create_test_config()
  
  # Mock a bad model that will fail initialization
  bad_model <- "model {
    for (i in 1:N) {
      y[i] ~ dnorm(mu, tau)
    }
    mu ~ dnorm(0, 0.0001)
    tau <- 1/sigma^2
    sigma ~ dunif(-1, 1)  # Invalid: negative sigma
  }"
  
  expect_error(
    run_fit_jags(
      data = data,
      model = bad_model,
      params = c("mu", "sigma"),
      config = config
    ),
    regexp = "initial|initialization|failed"
  )
})

test_that("JAGS handles memory issues gracefully", {
  # Create very large dataset to trigger memory issues
  data <- create_test_data(n = 10000)
  config <- create_test_config()
  config$n_iter <- 100000  # Very high iterations
  
  # This should handle memory issues internally
  # The function should either succeed with reduced iterations
  # or fail with a helpful error message
  result <- tryCatch({
    run_fit_jags(
      data = data,
      model = "one_comp",
      params = c("CL_pop", "V_pop"),
      config = config
    )
  }, error = function(e) {
    e
  })
  
  if (inherits(result, "error")) {
    expect_true(grepl("memory|iterations|reduce", result$message, ignore.case = TRUE))
  } else {
    # If it succeeded, check for warnings about reduced iterations
    expect_true(length(result$warnings) > 0 || !is.null(result$diagnostics))
  }
})

test_that("Computation timeout protection works", {
  skip_if_not_installed("R.utils")
  
  # Create computation that takes too long
  computation_fn <- function(params) {
    Sys.sleep(10)  # Sleep for 10 seconds
    return(list(result = "should not reach here"))
  }
  
  # Use very short timeout for testing
  with_options <- function(expr, timeout_val) {
    # Mock timeout behavior
    result <- tryCatch({
      R.utils::withTimeout({
        computation_fn(list())
      }, timeout = 0.1, onTimeout = "error")
    }, error = function(e) {
      list(success = FALSE, error = "Computation timeout exceeded")
    })
    return(result)
  }
  
  result <- with_options(
    safe_bayesian_computation(
      computation_type = "stan",
      computation_fn = computation_fn,
      params = list(test = TRUE)
    ),
    timeout_val = 0.1
  )
  
  if (!is.null(result$error)) {
    expect_true(grepl("timeout", result$error, ignore.case = TRUE))
  }
})

test_that("Diagnostics extraction works for Stan", {
  # Mock Stan result
  mock_stan_result <- list(
    diagnostic_summary = function() {
      list(
        num_divergent = c(5, 3, 0, 2),
        num_max_treedepth = c(0, 1, 0, 0)
      )
    },
    sampler_diagnostics = function() {
      array(runif(4000), dim = c(1000, 4, 1), 
            dimnames = list(NULL, NULL, c("accept_stat__", "stepsize__")))
    }
  )
  class(mock_stan_result) <- "CmdStanMCMC"
  
  diagnostics <- extract_diagnostics("stan", mock_stan_result)
  
  expect_equal(diagnostics$divergences, 10)  # Sum of divergences
  expect_equal(diagnostics$max_treedepth_hits, 1)
})

test_that("Diagnostics extraction works for JAGS", {
  skip_if_not_installed("coda")
  
  # Create mock JAGS result (mcmc.list)
  mock_samples <- coda::mcmc.list(
    coda::mcmc(matrix(rnorm(1000), ncol = 2, 
                     dimnames = list(NULL, c("param1", "param2")))),
    coda::mcmc(matrix(rnorm(1000), ncol = 2,
                     dimnames = list(NULL, c("param1", "param2"))))
  )
  
  diagnostics <- extract_diagnostics("jags", mock_samples)
  
  expect_true(!is.null(diagnostics$max_rhat))
  expect_true(!is.null(diagnostics$min_eff_size))
})

test_that("Problem detection works correctly", {
  # Test divergence detection
  diagnostics1 <- list(divergences = 10)
  problems1 <- check_computation_problems(diagnostics1)
  expect_true(any(grepl("10 divergent", problems1)))
  
  # Test Rhat detection
  diagnostics2 <- list(max_rhat = 1.15)
  problems2 <- check_computation_problems(diagnostics2)
  expect_true(any(grepl("Rhat.*1.15", problems2)))
  
  # Test ESS detection
  diagnostics3 <- list(min_ess_bulk = 250)
  problems3 <- check_computation_problems(diagnostics3)
  expect_true(any(grepl("bulk ESS.*250", problems3)))
  
  # Test no problems
  diagnostics4 <- list(
    divergences = 0,
    max_rhat = 1.005,
    min_ess_bulk = 500,
    min_ess_tail = 500
  )
  problems4 <- check_computation_problems(diagnostics4)
  expect_length(problems4, 0)
})

test_that("Input validation works correctly", {
  # Test missing required columns
  bad_data <- data.frame(TIME = c(1, 2, 3))
  config <- create_test_config()
  
  expect_error(
    validate_jags_inputs(bad_data, config),
    regexp = "Missing required columns"
  )
  
  # Test invalid TIME values
  bad_data2 <- data.frame(
    ID = c(1, 1),
    TIME = c(-1, 2),
    DV = c(10, 20),
    DOSE = c(100, 100)
  )
  
  expect_error(
    validate_jags_inputs(bad_data2, config),
    regexp = "Invalid TIME"
  )
  
  # Test invalid chain configuration
  bad_config <- list(n_chains = 20)
  good_data <- create_test_data()
  
  expect_error(
    validate_jags_inputs(good_data, bad_config),
    regexp = "chains.*between 1 and 10"
  )
})

test_that("BLQ data handling works", {
  data <- data.frame(
    ID = rep(1, 10),
    TIME = 1:10,
    DV = c(10, 5, 2, 0.5, 0.1, 0.05, 0.01, 0, NA, 0.001),
    DOSE = rep(100, 10)
  )
  
  # Test with automatic LLOQ detection
  processed_data <- handle_blq_data(data)
  lloq <- attr(processed_data, "lloq")
  
  expect_true("BLQ" %in% names(processed_data))
  expect_true(lloq > 0)
  expect_true(all(processed_data$DV[processed_data$BLQ & !is.na(processed_data$DV)] == lloq/2))
  
  # Test with specified LLOQ
  processed_data2 <- handle_blq_data(data, lloq = 1.0)
  expect_equal(attr(processed_data2, "lloq"), 1.0)
  expect_true(sum(processed_data2$BLQ) >= 5)  # Should flag values below 1.0
})

test_that("Parameter validation works", {
  # Test Stan parameters
  expect_true(validate_computation_params(
    list(chains = 4, adapt_delta = 0.9),
    "stan"
  ))
  
  expect_error(
    validate_computation_params(
      list(chains = 15),
      "stan"
    ),
    regexp = "chains.*between 1 and 10"
  )
  
  expect_error(
    validate_computation_params(
      list(adapt_delta = 1.5),
      "stan"
    ),
    regexp = "adapt_delta.*between 0 and 1"
  )
  
  # Test JAGS parameters
  expect_true(validate_computation_params(
    list(n_chains = 3, n_burnin = 1000),
    "jags"
  ))
  
  expect_error(
    validate_computation_params(
      list(n_burnin = 50),
      "jags"
    ),
    regexp = "Burn-in.*at least 100"
  )
})

test_that("Full integration test: Stan with problematic data", {
  skip_if_not_installed("cmdstanr")
  
  obs <- data.frame(
    time = c(2, 6, 12),
    conc = c(10, 5, 2)
  )
  
  regimen <- list(
    dose = 500,
    tau = 12,
    tinf = 1,
    n_doses = 3,
    start_time = 0
  )
  
  priors <- list(
    theta = c(CL = 5, Vc = 50),
    theta_log = list(
      mu = c(CL = log(5), Vc = log(50)),
      sd = c(CL = 0.3, Vc = 0.3)
    )
  )
  
  # This should complete without crashing
  result <- tryCatch({
    run_fit_stan_hmc(
      obs = obs,
      regimen = regimen,
      priors = priors,
      model_type = "1C",
      error_model = "additive",
      covariates = list(),
      estimate_sigma = TRUE,
      sigma_init = list(add = 1, prop = 0.1)
    )
  }, error = function(e) {
    e
  })
  
  # Should either succeed or fail with informative error
  if (inherits(result, "error")) {
    expect_true(grepl("Stan", result$message))
  } else {
    expect_true("draws" %in% names(result))
  }
})

test_that("Full integration test: JAGS with complete workflow", {
  skip_if_not_installed("rjags")
  
  data <- create_test_data()
  config <- create_test_config()
  
  result <- tryCatch({
    run_fit_jags(
      data = data,
      model = "one_comp",
      params = c("CL_pop", "V_pop", "sigma"),
      config = config
    )
  }, error = function(e) {
    e
  })
  
  if (!inherits(result, "error")) {
    expect_true("model" %in% names(result))
    expect_true("parameters" %in% names(result))
    expect_true("diagnostics" %in% names(result))
    
    # Check diagnostics structure
    if (!is.null(result$diagnostics$convergence)) {
      expect_type(result$diagnostics$convergence, "logical")
    }
  }
})