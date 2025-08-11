# R/backend_bayes.R
# Backends: Laplace (optim), Stan (cmdstanr/rstan), JAGS (rjags)
# Modified with comprehensive error handling

suppressPackageStartupMessages({
  library(numDeriv)
})

# Source safety wrapper if available
if (file.exists("R/safe_computation.R")) {
  source("R/safe_computation.R")
}

# ===== PERFORMANCE OPTIMIZATION: Global Stan Model Cache =====
.stan_model_cache <- new.env(parent = emptyenv())

get_compiled_model <- function(stan_file) {
  if (!file.exists(stan_file)) {
    stop("Stan file not found: ", stan_file)
  }
  
  # Create cache key from file name and modification time
  key <- digest::digest(c(stan_file, file.mtime(stan_file)))
  
  # Check cache
  if (!exists(key, envir = .stan_model_cache)) {
    message("Compiling Stan model: ", basename(stan_file))
    
    # Compile with error handling
    tryCatch({
      .stan_model_cache[[key]] <- cmdstanr::cmdstan_model(stan_file)
    }, error = function(e) {
      stop(sprintf("Failed to compile Stan model %s: %s", basename(stan_file), e$message))
    })
  } else {
    message("Using cached Stan model: ", basename(stan_file))
  }
  
  .stan_model_cache[[key]]
}
# ===== END PERFORMANCE OPTIMIZATION =====

backend_status <- function() {
  has_cmdstan <- requireNamespace("cmdstanr", quietly = TRUE)
  has_rstan   <- requireNamespace("rstan", quietly = TRUE)
  has_rjags   <- requireNamespace("rjags", quietly = TRUE)
  glue::glue("cmdstanr: {has_cmdstan}, rstan: {has_rstan}, rjags: {has_rjags}")
}

# Prior-Parsing: erwartet priors$theta_log (mu/sd auf log-Skala)
draw_from_priors <- function(priors) {
  th <- priors$theta
  exp(setNames(rnorm(length(th), priors$theta_log$mu[names(th)], priors$theta_log$sd[names(th)]), names(th)))
}

# Helper function for covariate adjustment
apply_covariates <- function(theta, covariates, drug_name = NULL) {
  # Simple allometric scaling for CL and V
  if (!is.null(covariates$weight) && !is.null(theta[["CL"]])) {
    theta[["CL"]] <- theta[["CL"]] * (covariates$weight / 70)^0.75
  }
  if (!is.null(covariates$weight) && !is.null(theta[["Vc"]])) {
    theta[["Vc"]] <- theta[["Vc"]] * (covariates$weight / 70)^1.0
  }
  theta
}

# Modified Stan HMC implementation with error handling
run_fit_stan_hmc <- function(obs, regimen, priors, model_type, error_model, 
                            covariates, estimate_sigma, sigma_init, 
                            blq_lloq = NA_real_, is_blq = NULL) {
  
  # Get HMC controls from options or defaults
  .hmc <- getOption("tdmx_hmc", default = list(
    chains = 4L, iter_warmup = 1000L, iter_sampling = 1000L,
    parallel_chains = NULL, adapt_delta = 0.8, max_treedepth = 10L, seed = 1234L
  ))
  
  # Prepare parameters
  params <- list(
    obs = obs,
    regimen = regimen,
    priors = priors,
    model_type = model_type,
    error_model = error_model,
    covariates = covariates,
    estimate_sigma = estimate_sigma,
    sigma_init = sigma_init,
    blq_lloq = blq_lloq,
    is_blq = is_blq,
    adapt_delta = .hmc$adapt_delta,
    max_treedepth = .hmc$max_treedepth,
    chains = .hmc$chains,
    iter_warmup = .hmc$iter_warmup,
    iter_sampling = .hmc$iter_sampling,
    seed = .hmc$seed,
    parallel_chains = .hmc$parallel_chains
  )
  
  # Computation function
  stan_computation <- function(p) {
    # Get Stan file
    stan_file <- stan_file_for_model(p$model_type)
    
    # Prepare data
    data_list <- stan_data_list2(
      p$obs, p$regimen, p$priors, p$model_type, 
      p$error_model, p$estimate_sigma, p$sigma_init, 
      p$blq_lloq, p$is_blq
    )
    
    # Validate data
    if (any(is.na(data_list$y_obs)) || any(is.infinite(data_list$y_obs))) {
      stop("Invalid observation data: contains NA or Inf values")
    }
    
    # Get compiled model
    mod <- get_compiled_model(stan_file)
    
    # Run sampling with specific parameters
    fit <- mod$sample(
      data = data_list,
      seed = p$seed,
      chains = p$chains,
      parallel_chains = p$parallel_chains %||% p$chains,
      iter_warmup = p$iter_warmup,
      iter_sampling = p$iter_sampling,
      adapt_delta = p$adapt_delta,
      max_treedepth = p$max_treedepth,
      refresh = 0,  # Suppress Stan output
      show_messages = FALSE
    )
    
    return(fit)
  }
  
  # Run with safety wrapper if available
  if (exists("safe_bayesian_computation") && is.function(safe_bayesian_computation)) {
    result <- safe_bayesian_computation(
      computation_type = "stan",
      computation_fn = stan_computation,
      params = params,
      session = getDefaultReactiveDomain()  # Get Shiny session if available
    )
    
    if (!result$success) {
      # Provide detailed error message
      error_msg <- sprintf("Stan computation failed: %s", result$error)
      if (length(result$warnings) > 0) {
        error_msg <- paste(error_msg, "\nWarnings:", paste(result$warnings, collapse = "; "))
      }
      stop(error_msg)
    }
    
    fit <- result$result
    computation_diagnostics <- result$diagnostics
    computation_warnings <- result$warnings
    computation_time <- result$duration
    
  } else {
    # Fallback: direct execution with basic error handling
    fit <- tryCatch({
      stan_computation(params)
    }, error = function(e) {
      stop(sprintf("Stan computation failed: %s", e$message))
    })
    computation_diagnostics <- NULL
    computation_warnings <- character()
    computation_time <- NA
  }
  
  # Process results
  draws <- tryCatch({
    as.data.frame(fit$draws(
      variables = c("CL_out","Vc_out","Q1_out","Vp1_out","Q2_out","Vp2_out",
                   "sigma_add","sigma_prop","nu"), 
      format = "df"
    ))
  }, error = function(e) {
    stop(sprintf("Failed to extract draws: %s", e$message))
  })
  
  # Rename outputs
  names(draws) <- sub("_out$", "", names(draws))
  
  # Extract additional diagnostics if not already done
  if (is.null(computation_diagnostics)) {
    diagnostics <- NULL
    try({
      if (requireNamespace("posterior", quietly = TRUE)) {
        summ <- posterior::summarise_draws(fit$draws())
        keep <- intersect(c("CL_out","Vc_out","Q1_out","Vp1_out","Q2_out","Vp2_out",
                          "sigma_add","sigma_prop","nu"), summ$variable)
        summ <- summ[summ$variable %in% keep, c("variable","rhat","ess_bulk","ess_tail"), drop = FALSE]
      } else { 
        summ <- NULL 
      }
      
      sdiag <- try(fit$diagnostic_summary(), silent = TRUE)
      div <- try(sdiag$num_divergent[1], silent = TRUE)
      treedepth <- try(sdiag$num_max_treedepth[1], silent = TRUE)
      stepsize <- try(as.numeric(fit$metadata()$step_size_adaptation), silent = TRUE)
      
      diagnostics <- list(
        summary = summ, 
        divergences = div, 
        treedepth_hits = treedepth, 
        stepsize = stepsize
      )
    }, silent = TRUE)
  } else {
    diagnostics <- computation_diagnostics
  }
  
  return(list(
    draws = draws,
    diagnostics = diagnostics,
    warnings = computation_warnings,
    computation_time = computation_time
  ))
}

# Modified ADVI implementation with error handling
run_fit_stan_advi <- function(obs, regimen, priors, model_type, error_model, 
                             covariates, estimate_sigma, sigma_init, 
                             blq_lloq = NA_real_, is_blq = NULL) {
  
  if (!requireNamespace("cmdstanr", quietly = TRUE) && !requireNamespace("rstan", quietly = TRUE)) {
    warning("Stan-ADVI nicht verfügbar, fallback auf Laplace.")
    return(run_fit_laplace(obs, regimen, priors, model_type, error_model, 
                          covariates, estimate_sigma, sigma_init, blq_lloq, is_blq))
  }
  
  params <- list(
    obs = obs,
    regimen = regimen,
    priors = priors,
    model_type = model_type,
    error_model = error_model,
    estimate_sigma = estimate_sigma,
    sigma_init = sigma_init,
    blq_lloq = blq_lloq,
    is_blq = is_blq
  )
  
  advi_computation <- function(p) {
    stan_file <- stan_file_for_model(p$model_type)
    data_list <- stan_data_list2(
      p$obs, p$regimen, p$priors, p$model_type, 
      p$error_model, p$estimate_sigma, p$sigma_init, 
      p$blq_lloq, p$is_blq
    )
    
    if (requireNamespace("cmdstanr", quietly = TRUE)) {
      mod <- get_compiled_model(stan_file)
      fit <- mod$variational(
        data = data_list, 
        output_samples = 1000, 
        seed = 123,
        refresh = 0
      )
      dr <- as.data.frame(fit$draws(
        variables = c("CL","Vc","Q1","Vp1","Q2","Vp2"), 
        format = "df"
      ))
    } else {
      stan_text <- readChar(stan_file, file.info(stan_file)$size)
      sm <- rstan::stan_model(model_code = stan_text)
      fit <- rstan::vb(sm, data = data_list, output_samples = 1000, seed = 123)
      dr <- as.data.frame(rstan::extract(fit, pars = c("CL","Vc","Q1","Vp1","Q2","Vp2")))
    }
    
    return(dr)
  }
  
  # Run with safety wrapper if available
  if (exists("safe_bayesian_computation") && is.function(safe_bayesian_computation)) {
    result <- safe_bayesian_computation(
      computation_type = "stan",
      computation_fn = advi_computation,
      params = params,
      session = getDefaultReactiveDomain()
    )
    
    if (!result$success) {
      stop(sprintf("Stan ADVI failed: %s", result$error))
    }
    
    dr <- result$result
  } else {
    dr <- tryCatch({
      advi_computation(params)
    }, error = function(e) {
      stop(sprintf("Stan ADVI failed: %s", e$message))
    })
  }
  
  keep <- intersect(colnames(dr), names(priors$theta))
  dr <- dr[, keep, drop = FALSE]
  
  return(list(draws = dr, diagnostics = NULL))
}

# Existing helper functions (keeping compatibility)
run_fit_stan <- function(obs, regimen, priors, model_type, error_model, covariates,
                        estimate_sigma, sigma_init, blq_lloq = NA_real_, is_blq = NULL) {
  run_fit_stan_hmc(obs, regimen, priors, model_type, error_model, 
                   covariates, estimate_sigma, sigma_init, blq_lloq, is_blq)
}

# Helper to select appropriate Stan model file
stan_file_for_model <- function(model_type) {
  if (model_type == "MM-1C") {
    return("models/stan/pk_mm_onecpt_ode.stan")
  } else if (model_type == "TMDD-QSS-1C") {
    return("models/stan/pk_tmdd_qss_onecpt_ode.stan")
  } else {
    return("models/stan/pk_multicpt_ode.stan")
  }
}

# Helper to build Stan data list
stan_data_list2 <- function(obs, regimen, priors, model_type, error_model, 
                           estimate_sigma, sigma_init, blq_lloq, is_blq) {
  # Map error model string to numeric code
  error_code <- switch(error_model,
    "additiv" = 1L,
    "proportional" = 2L,
    "kombiniert" = 3L,
    "t-additiv" = 4L,
    "t-proportional" = 5L,
    "mixture" = 6L,
    3L  # default to combined
  )
  
  # Build infusion schedule
  n_inf <- regimen$n_doses
  t0 <- regimen$start_time + (0:(n_inf-1)) * regimen$tau
  tinf <- rep(regimen$tinf, n_inf)
  rate <- rep(regimen$dose / regimen$tinf, n_inf)
  
  # BLQ handling
  if (is.null(is_blq)) is_blq <- rep(0L, nrow(obs))
  if (is.na(blq_lloq)) blq_lloq <- 0.0
  
  # Create data list
  data_list <- list(
    N = nrow(obs),
    n_inf = n_inf,
    t_obs = obs$time,
    y_obs = obs$conc,
    t0 = t0,
    tinf = tinf,
    rate = rate,
    error_model = error_code,
    estimate_sigma = as.integer(estimate_sigma),
    sigma_add_init = sigma_init[["add"]] %||% 1.0,
    sigma_prop_init = sigma_init[["prop"]] %||% 0.1,
    is_blq = as.integer(is_blq),
    lloq = blq_lloq,
    # Priors
    mu_log_CL = priors$theta_log$mu[["CL"]] %||% log(5),
    sd_log_CL = priors$theta_log$sd[["CL"]] %||% 0.5,
    mu_log_Vc = priors$theta_log$mu[["Vc"]] %||% log(50),
    sd_log_Vc = priors$theta_log$sd[["Vc"]] %||% 0.5
  )
  
  # Add additional parameters based on model type
  if (model_type %in% c("2C", "3C", "2C_CRRT", "3C_CRRT")) {
    data_list$mu_log_Q1 <- priors$theta_log$mu[["Q1"]] %||% log(10)
    data_list$sd_log_Q1 <- priors$theta_log$sd[["Q1"]] %||% 0.5
    data_list$mu_log_Vp1 <- priors$theta_log$mu[["Vp1"]] %||% log(100)
    data_list$sd_log_Vp1 <- priors$theta_log$sd[["Vp1"]] %||% 0.5
  }
  
  if (model_type %in% c("3C", "3C_CRRT")) {
    data_list$mu_log_Q2 <- priors$theta_log$mu[["Q2"]] %||% log(5)
    data_list$sd_log_Q2 <- priors$theta_log$sd[["Q2"]] %||% 0.5
    data_list$mu_log_Vp2 <- priors$theta_log$mu[["Vp2"]] %||% log(200)
    data_list$sd_log_Vp2 <- priors$theta_log$sd[["Vp2"]] %||% 0.5
  }
  
  return(data_list)
}

# Modified Laplace/MAP implementation with basic error handling
run_fit_laplace <- function(obs, regimen, priors, model_type, error_model, 
                          covariates, estimate_sigma, sigma_init, 
                          blq_lloq = NA_real_, is_blq = NULL) {
  
  tryCatch({
    validate_inputs_units(regimen, obs)
    
    th0 <- log(priors$theta) # Start bei prior-Mean
    sigma_add <- sigma_init[["add"]]
    sigma_prop <- sigma_init[["prop"]]
    creatinine_data <- NULL  # placeholder
    
    obj <- function(p) {
      neg_log_post_map(p, obs, regimen, priors, model_type, error_model, 
                      sigma_add, sigma_prop, covariates, blq_lloq, is_blq, 
                      creatinine_data)
    }
    
    opt <- optim(th0, obj, method = "BFGS", hessian = TRUE, 
                control = list(maxit = 1000))
    
    if (opt$convergence != 0) {
      warning(sprintf("Optimization did not converge (code: %d)", opt$convergence))
    }
    
    cov <- tryCatch(
      solve(opt$hessian), 
      error = function(e) {
        warning("Hessian not invertible, using diagonal approximation")
        diag(rep(0.05^2, length(th0)))
      }
    )
    
    draws <- MASS::mvrnorm(n = 800, mu = opt$par, Sigma = cov)
    draws_nat <- exp(draws)
    colnames(draws_nat) <- names(priors$theta)
    
    list(draws = draws_nat)
    
  }, error = function(e) {
    stop(sprintf("Laplace approximation failed: %s", e$message))
  })
}

# Placeholder functions (need to be implemented or imported)
neg_log_post_map <- function(par_log, obs, regimen, priors, model_type, error_model,
                           sigma_add, sigma_prop, covariates, blq_lloq, is_blq, 
                           creatinine_data) {
  # This would need actual implementation
  # For now, return a simple quadratic
  sum((par_log - log(priors$theta))^2 / 2)
}

validate_inputs_units <- function(regimen, obs) {
  if (is.null(regimen$dose) || regimen$dose <= 0) {
    stop("Invalid dose")
  }
  if (is.null(obs$time) || any(obs$time < 0)) {
    stop("Invalid observation times")
  }
  TRUE
}

cl_creatinine_penalty <- function(CL, age, weight, sex, creatinine_data) {
  # Simplified - returns 0 if no creatinine data
  if (is.null(creatinine_data) || nrow(creatinine_data) == 0) return(0)
  0
}

# Get default reactive domain if in Shiny context
getDefaultReactiveDomain <- function() {
  if (requireNamespace("shiny", quietly = TRUE)) {
    try(shiny::getDefaultReactiveDomain(), silent = TRUE)
  } else {
    NULL
  }
}