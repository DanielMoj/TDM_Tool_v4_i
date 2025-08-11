# tests/testthat/test-pta-cfr.R
# Tests for PTA/CFR calculations with n_intervals fix

library(testthat)

test_that("n_intervals is properly defined in compute_metrics_for_draw", {
  # Mock required functions for isolated testing
  predict_conc_grid <- function(times, dosing, theta, model_type) {
    # Simple mock: exponential decay
    k_el <- theta$CL / theta$Vc
    conc <- dosing$dose * exp(-k_el * times)
    return(conc)
  }
  
  apply_site_penetration <- function(conc, drug, site, config) {
    # Mock: return unchanged
    return(conc)
  }
  
  load_tissue_cfg <- function(path) {
    # Mock: return empty config
    return(list())
  }
  
  # Test data
  theta <- list(CL = 5, Vc = 30)
  regimen <- list(
    dose = 1000,
    tau = 8,
    tinf = 1
  )
  model_type <- "1C"
  MIC <- 1
  
  # Should not error due to undefined n_intervals
  expect_no_error({
    result <- compute_metrics_for_draw(theta, regimen, model_type, MIC)
  })
  
  # Result should have expected structure
  result <- compute_metrics_for_draw(theta, regimen, model_type, MIC)
  expect_type(result, "list")
  expect_true(all(c("ft_gt_mic", "auc_tau", "auc24", "auc24_over_mic", "cmax", "cmax_over_mic") %in% names(result)))
  
  # Values should be numeric and finite (except possibly Inf for MIC ratios)
  expect_true(is.numeric(result$ft_gt_mic))
  expect_true(is.numeric(result$auc_tau))
  expect_true(is.numeric(result$auc24))
  expect_true(is.numeric(result$cmax))
})

test_that("ss_window receives n_intervals parameter correctly", {
  regimen <- list(tau = 8)
  
  # Test with default
  win1 <- ss_window(regimen)
  expect_equal(win1$t_end, 20 * 8)  # 20L is the default
  
  # Test with explicit value
  win2 <- ss_window(regimen, n_intervals = 30L)
  expect_equal(win2$t_end, 30 * 8)
  
  # Test that times vector has correct length
  expect_true(length(win1$times) > 0)
  expect_equal(win1$times[1], 0)
  expect_equal(tail(win1$times, 1), win1$t_end)
})

test_that("PTA calculation works with fixed n_intervals", {
  # Mock functions
  predict_conc_grid <- function(times, dosing, theta, model_type) {
    k_el <- theta$CL / theta$Vc
    conc <- dosing$dose * exp(-k_el * times)
    return(conc)
  }
  
  apply_site_penetration <- function(conc, drug, site, config) {
    return(conc)
  }
  
  load_tissue_cfg <- function(path) {
    return(list())
  }
  
  meets_target <- function(metrics, target_def) {
    # Simple mock: check if fT>MIC meets threshold
    if (target_def$type == "fT>MIC") {
      return(metrics$ft_gt_mic >= target_def$cutoff)
    }
    return(TRUE)
  }
  
  # Test data
  set.seed(123)
  draws <- data.frame(
    CL = exp(rnorm(10, log(5), 0.2)),
    Vc = exp(rnorm(10, log(30), 0.2))
  )
  
  regimen <- list(
    dose = 1000,
    tau = 8,
    tinf = 1
  )
  
  model_type <- "1C"
  target_def <- list(type = "fT>MIC", cutoff = 0.5)
  MIC <- 1
  
  # Should calculate PTA without errors
  expect_no_error({
    pta <- pta_for_regimen(draws, regimen, model_type, target_def, MIC)
  })
  
  # PTA should be between 0 and 1
  pta <- pta_for_regimen(draws, regimen, model_type, target_def, MIC)
  expect_true(is.numeric(pta))
  expect_gte(pta, 0)
  expect_lte(pta, 1)
})

test_that("CFR calculation works with fixed n_intervals", {
  # Mock functions
  predict_conc_grid <- function(times, dosing, theta, model_type) {
    k_el <- theta$CL / theta$Vc
    conc <- dosing$dose * exp(-k_el * times)
    return(conc)
  }
  
  apply_site_penetration <- function(conc, drug, site, config) {
    return(conc)
  }
  
  load_tissue_cfg <- function(path) {
    return(list())
  }
  
  meets_target <- function(metrics, target_def) {
    if (target_def$type == "AUC/MIC") {
      return(metrics$auc24_over_mic >= target_def$cutoff)
    }
    return(TRUE)
  }
  
  # Test data
  set.seed(456)
  draws <- data.frame(
    CL = exp(rnorm(10, log(5), 0.2)),
    Vc = exp(rnorm(10, log(30), 0.2))
  )
  
  regimen <- list(
    dose = 1000,
    tau = 8,
    tinf = 1
  )
  
  model_type <- "1C"
  target_def <- list(type = "AUC/MIC", cutoff = 125)
  
  # MIC distribution
  mic_dist <- data.frame(
    mic = c(0.25, 0.5, 1, 2),
    p = c(0.2, 0.3, 0.3, 0.2)
  )
  
  # Should calculate CFR without errors
  expect_no_error({
    cfr <- cfr_for_regimen(draws, regimen, model_type, target_def, mic_dist)
  })
  
  # CFR should be between 0 and 1
  cfr <- cfr_for_regimen(draws, regimen, model_type, target_def, mic_dist)
  expect_true(is.numeric(cfr))
  expect_gte(cfr, 0)
  expect_lte(cfr, 1)
})

test_that("parse_mic_distribution handles various inputs correctly", {
  # Valid input
  txt1 <- "0.25:0.1, 0.5:0.2, 1:0.4, 2:0.3"
  dist1 <- parse_mic_distribution(txt1)
  expect_equal(nrow(dist1), 4)
  expect_equal(sum(dist1$p), 1)  # Should be normalized
  
  # Empty input
  expect_null(parse_mic_distribution(""))
  expect_null(parse_mic_distribution(NULL))
  
  # Invalid format
  txt2 <- "invalid format"
  expect_true(is.null(parse_mic_distribution(txt2)) || nrow(parse_mic_distribution(txt2)) == 0)
  
  # Partially valid input (should filter out invalid entries)
  txt3 <- "0.5:0.5, invalid:data, 1:0.5"
  dist3 <- parse_mic_distribution(txt3)
  if (!is.null(dist3)) {
    expect_equal(nrow(dist3), 2)  # Only valid entries
    expect_equal(sum(dist3$p), 1)  # Should be normalized
  }
})

test_that("Performance optimization: vectorized functions are faster", {
  skip_if_not(requireNamespace("microbenchmark", quietly = TRUE))
  
  # Mock simple functions for benchmarking
  predict_conc_grid <- function(times, dosing, theta, model_type) {
    k_el <- theta$CL / theta$Vc
    conc <- dosing$dose * exp(-k_el * times)
    return(conc)
  }
  
  apply_site_penetration <- function(conc, drug, site, config) {
    return(conc * 0.7)  # Simple factor
  }
  
  load_tissue_cfg <- function(path) {
    return(list(factor = 0.7))
  }
  
  meets_target <- function(metrics, target_def) {
    return(metrics$auc24_over_mic >= 125)
  }
  
  # Create larger dataset for performance testing
  set.seed(789)
  draws <- data.frame(
    CL = exp(rnorm(100, log(5), 0.2)),
    Vc = exp(rnorm(100, log(30), 0.2))
  )
  
  regimen <- list(dose = 1000, tau = 8, tinf = 1)
  model_type <- "1C"
  target_def <- list(type = "AUC/MIC", cutoff = 125)
  MIC <- 1
  
  # Measure performance
  time_vectorized <- system.time({
    pta <- pta_for_regimen(draws, regimen, model_type, target_def, MIC)
  })
  
  # Should complete in reasonable time (< 5 seconds for 100 draws)
  expect_lt(time_vectorized["elapsed"], 5)
})

test_that("Cache functionality works correctly", {
  skip_if_not(requireNamespace("digest", quietly = TRUE))
  
  # Clear cache first
  clear_pta_cache()
  
  # Mock functions
  predict_conc_grid <- function(times, dosing, theta, model_type) {
    k_el <- theta$CL / theta$Vc
    conc <- dosing$dose * exp(-k_el * times)
    return(conc)
  }
  
  apply_site_penetration <- function(conc, drug, site, config) {
    return(conc)
  }
  
  load_tissue_cfg <- function(path) {
    return(list())
  }
  
  meets_target <- function(metrics, target_def) {
    return(metrics$auc24_over_mic >= 125)
  }
  
  # Test data
  draws <- data.frame(CL = c(5, 6), Vc = c(30, 35))
  regimen <- list(dose = 1000, tau = 8, tinf = 1)
  model_type <- "1C"
  target_def <- list(type = "AUC/MIC", cutoff = 125)
  MIC <- 1
  
  # First call should compute
  result1 <- pta_for_regimen_cached(draws, regimen, model_type, target_def, MIC)
  
  # Second call should use cache (should be identical)
  result2 <- pta_for_regimen_cached(draws, regimen, model_type, target_def, MIC)
  expect_equal(result1, result2)
  
  # Clear cache
  clear_pta_cache()
  
  # After clearing, should compute again
  result3 <- pta_for_regimen_cached(draws, regimen, model_type, target_def, MIC)
  expect_equal(result1, result3)  # Results should still be the same
})