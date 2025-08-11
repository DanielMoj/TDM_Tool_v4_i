# R/safe_computation.R
# Safe execution wrapper for Bayesian computations with comprehensive error handling

#' Sichere Ausführung von Bayes'schen Berechnungen mit Progress und Error Handling
#' @param computation_type "stan" oder "jags"
#' @param computation_fn Die eigentliche Berechnungsfunktion
#' @param params Parameter für die Berechnung
#' @param session Shiny session für Progress updates (optional)
#' @return Liste mit success, result, error, diagnostics
safe_bayesian_computation <- function(computation_type, 
                                    computation_fn, 
                                    params,
                                    session = NULL) {
  
  # Initialize result structure
  result <- list(
    success = FALSE,
    computation_type = computation_type,
    start_time = Sys.time(),
    result = NULL,
    error = NULL,
    warnings = character(),
    diagnostics = list(),
    recovery_attempted = FALSE
  )
  
  # Progress tracking
  if (!is.null(session)) {
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    progress$set(message = sprintf("Starting %s computation...", computation_type), value = 10)
  }
  
  # Warning handler
  warning_handler <- function(w) {
    result$warnings <<- c(result$warnings, w$message)
    invokeRestart("muffleWarning")
  }
  
  # Main computation with error handling
  withCallingHandlers({
    tryCatch({
      # Memory check before starting (Linux specific)
      mem_check <- try({
        mem_info <- system("awk '/MemAvailable/ {print $2}' /proc/meminfo", intern = TRUE, ignore.stderr = TRUE)
        if (length(mem_info) > 0) {
          mem_available <- as.numeric(mem_info) / 1024  # Convert to MB
          if (!is.na(mem_available) && mem_available < 500) {  # Less than 500MB
            stop("Insufficient memory available for computation")
          }
        }
      }, silent = TRUE)
      
      if (!is.null(session)) progress$set(value = 20, message = "Initializing model...")
      
      # Run computation with timeout protection
      computation_result <- tryCatch({
        if (computation_type == "stan") {
          # Stan might take longer, allow up to 5 minutes
          R.utils::withTimeout({
            computation_fn(params)
          }, timeout = 300, onTimeout = "error")
        } else {
          # JAGS typically faster, 3 minute timeout
          R.utils::withTimeout({
            computation_fn(params)
          }, timeout = 180, onTimeout = "error")
        }
      }, error = function(e) {
        if (grepl("timeout", e$message, ignore.case = TRUE)) {
          stop("Computation timeout exceeded. Consider reducing iterations or chains.")
        } else {
          stop(e$message)
        }
      })
      
      if (!is.null(session)) progress$set(value = 80, message = "Finalizing...")
      
      # Validate result
      if (is.null(computation_result)) {
        stop("Computation returned NULL")
      }
      
      # Extract diagnostics
      result$diagnostics <- extract_diagnostics(computation_type, computation_result)
      
      # Check for problems
      problems <- check_computation_problems(result$diagnostics)
      if (length(problems) > 0) {
        result$warnings <- c(result$warnings, problems)
      }
      
      result$success <- TRUE
      result$result <- computation_result
      
      if (!is.null(session)) progress$set(value = 100, message = "Complete!")
      
    }, error = function(e) {
      result$error <- e$message
      
      # Attempt recovery based on error type
      if (grepl("divergent|diverg", e$message, ignore.case = TRUE)) {
        result$error <- paste(result$error, 
          "\nSuggestion: Try increasing 'adapt_delta' parameter (e.g., 0.95 or 0.99)")
        result$recovery_attempted <- TRUE
        
        # Auto-retry with higher adapt_delta if Stan
        if (computation_type == "stan" && !is.null(params$adapt_delta) && params$adapt_delta < 0.95) {
          if (!is.null(session)) {
            progress$set(value = 50, message = "Retrying with higher adapt_delta...")
          }
          
          params$adapt_delta <- 0.95
          params$max_treedepth <- max(params$max_treedepth %||% 10, 15)
          
          retry_result <- try({
            R.utils::withTimeout({
              computation_fn(params)
            }, timeout = 300, onTimeout = "error")
          }, silent = TRUE)
          
          if (!inherits(retry_result, "try-error")) {
            result$success <- TRUE
            result$result <- retry_result
            result$warnings <- c(result$warnings, 
              "Computation succeeded after increasing adapt_delta to 0.95")
            result$diagnostics <- extract_diagnostics(computation_type, retry_result)
          }
        }
      } else if (grepl("memory|alloc", e$message, ignore.case = TRUE)) {
        result$error <- paste(result$error,
          "\nSuggestion: Reduce number of chains or iterations")
      } else if (grepl("initial|init", e$message, ignore.case = TRUE)) {
        result$error <- paste(result$error,
          "\nSuggestion: Check initial values or use different initialization")
      } else if (grepl("compilation|compile", e$message, ignore.case = TRUE)) {
        result$error <- paste(result$error,
          "\nSuggestion: Check model specification or Stan/JAGS installation")
      }
    })
  }, warning = warning_handler)
  
  # Calculate duration
  result$duration <- difftime(Sys.time(), result$start_time, units = "secs")
  
  # Log to audit if available
  if (exists("audit_event") && is.function(audit_event)) {
    try({
      audit_event(
        action = paste0(computation_type, "_computation"),
        payload = list(
          success = result$success,
          duration = as.numeric(result$duration),
          warnings = length(result$warnings),
          error = result$error,
          diagnostics = if(result$success) summarize_diagnostics(result$diagnostics) else NULL
        )
      )
    }, silent = TRUE)
  }
  
  return(result)
}

#' Extract diagnostics based on computation type
#' @param type "stan" or "jags"
#' @param result Computation result object
#' @return List of diagnostics
extract_diagnostics <- function(type, result) {
  diagnostics <- list()
  
  if (type == "stan") {
    # Stan-specific diagnostics
    if (inherits(result, "CmdStanMCMC") || inherits(result, "stanfit")) {
      try({
        if (inherits(result, "CmdStanMCMC")) {
          # cmdstanr output
          diag_summary <- result$diagnostic_summary()
          diagnostics$divergences <- sum(diag_summary$num_divergent)
          diagnostics$max_treedepth_hits <- sum(diag_summary$num_max_treedepth)
          diagnostics$ebfmi <- diag_summary$ebfmi
          
          # Get sampler diagnostics
          sampler_diag <- result$sampler_diagnostics()
          if (!is.null(sampler_diag)) {
            diagnostics$mean_accept_stat <- mean(sampler_diag[,,"accept_stat__"], na.rm = TRUE)
            diagnostics$mean_stepsize <- mean(sampler_diag[,,"stepsize__"], na.rm = TRUE)
          }
        } else {
          # rstan output
          sampler_params <- rstan::get_sampler_params(result)
          diagnostics$divergences <- sum(sapply(sampler_params, function(x) sum(x[,"divergent__"])))
          diagnostics$max_treedepth_hits <- sum(sapply(sampler_params, function(x) sum(x[,"treedepth__"] >= 10)))
        }
      }, silent = TRUE)
    }
    
    # Rhat and ESS
    if (requireNamespace("posterior", quietly = TRUE)) {
      try({
        if (inherits(result, "CmdStanMCMC")) {
          draws <- posterior::as_draws_df(result$draws())
        } else {
          draws <- posterior::as_draws_df(result)
        }
        summary <- posterior::summarise_draws(draws)
        
        # Filter to actual parameters (exclude lp__ and other internals)
        param_rows <- !grepl("^(lp__|log_lik|y_rep)", summary$variable)
        if (any(param_rows)) {
          diagnostics$max_rhat <- max(summary$rhat[param_rows], na.rm = TRUE)
          diagnostics$min_ess_bulk <- min(summary$ess_bulk[param_rows], na.rm = TRUE)
          diagnostics$min_ess_tail <- min(summary$ess_tail[param_rows], na.rm = TRUE)
          diagnostics$num_params <- sum(param_rows)
        }
      }, silent = TRUE)
    }
    
  } else if (type == "jags") {
    # JAGS-specific diagnostics
    if (requireNamespace("coda", quietly = TRUE)) {
      try({
        if (inherits(result, "mcmc.list")) {
          # Gelman-Rubin diagnostic
          if (length(result) > 1) {  # Need multiple chains
            gelman_result <- coda::gelman.diag(result, multivariate = FALSE)
            diagnostics$max_rhat <- max(gelman_result$psrf[,1], na.rm = TRUE)
            diagnostics$gelman_mpsrf <- gelman_result$mpsrf
          }
          
          # Effective size
          eff_size <- coda::effectiveSize(result)
          diagnostics$min_eff_size <- min(eff_size, na.rm = TRUE)
          diagnostics$mean_eff_size <- mean(eff_size, na.rm = TRUE)
          
          # Heidelberger-Welch diagnostic
          heidel_result <- coda::heidel.diag(result)
          diagnostics$heidel_passed <- all(sapply(heidel_result, function(x) all(x[,"pvalue"] > 0.05)))
          
          # Raftery-Lewis diagnostic
          raftery_result <- try(coda::raftery.diag(result), silent = TRUE)
          if (!inherits(raftery_result, "try-error")) {
            diagnostics$raftery <- raftery_result
          }
        }
      }, silent = TRUE)
    }
  }
  
  return(diagnostics)
}

#' Check for computation problems
#' @param diagnostics List of diagnostics
#' @return Character vector of problem descriptions
check_computation_problems <- function(diagnostics) {
  problems <- character()
  
  # Stan-specific checks
  if (!is.null(diagnostics$divergences) && diagnostics$divergences > 0) {
    problems <- c(problems, sprintf("%d divergent transitions detected", diagnostics$divergences))
  }
  
  if (!is.null(diagnostics$max_treedepth_hits) && diagnostics$max_treedepth_hits > 0) {
    problems <- c(problems, sprintf("%d transitions hit max treedepth", diagnostics$max_treedepth_hits))
  }
  
  if (!is.null(diagnostics$ebfmi) && any(diagnostics$ebfmi < 0.3)) {
    problems <- c(problems, "Low E-BFMI detected (< 0.3), indicating potential problems")
  }
  
  # General MCMC checks
  if (!is.null(diagnostics$max_rhat) && diagnostics$max_rhat > 1.01) {
    problems <- c(problems, sprintf("Convergence issue: max Rhat = %.3f (should be < 1.01)", 
                                  diagnostics$max_rhat))
  }
  
  if (!is.null(diagnostics$min_ess_bulk) && diagnostics$min_ess_bulk < 400) {
    problems <- c(problems, sprintf("Low bulk ESS: %.0f (should be > 400)", 
                                  diagnostics$min_ess_bulk))
  }
  
  if (!is.null(diagnostics$min_ess_tail) && diagnostics$min_ess_tail < 400) {
    problems <- c(problems, sprintf("Low tail ESS: %.0f (should be > 400)", 
                                  diagnostics$min_ess_tail))
  }
  
  # JAGS-specific checks
  if (!is.null(diagnostics$min_eff_size) && diagnostics$min_eff_size < 400) {
    problems <- c(problems, sprintf("Low effective sample size: %.0f (should be > 400)", 
                                  diagnostics$min_eff_size))
  }
  
  if (!is.null(diagnostics$heidel_passed) && !diagnostics$heidel_passed) {
    problems <- c(problems, "Heidelberger-Welch test failed for some parameters")
  }
  
  return(problems)
}

#' Summarize diagnostics for logging
#' @param diagnostics List of diagnostics
#' @return Simplified diagnostics for logging
summarize_diagnostics <- function(diagnostics) {
  summary <- list()
  
  # Key metrics only
  if (!is.null(diagnostics$divergences)) summary$divergences <- diagnostics$divergences
  if (!is.null(diagnostics$max_rhat)) summary$max_rhat <- round(diagnostics$max_rhat, 3)
  if (!is.null(diagnostics$min_ess_bulk)) summary$min_ess_bulk <- round(diagnostics$min_ess_bulk, 0)
  if (!is.null(diagnostics$min_eff_size)) summary$min_eff_size <- round(diagnostics$min_eff_size, 0)
  
  return(summary)
}

#' Validate computation parameters
#' @param params List of parameters
#' @param computation_type Type of computation
#' @return TRUE if valid, otherwise throws error
validate_computation_params <- function(params, computation_type) {
  if (computation_type == "stan") {
    # Validate Stan parameters
    if (!is.null(params$chains) && (params$chains < 1 || params$chains > 10)) {
      stop("Number of chains must be between 1 and 10")
    }
    if (!is.null(params$iter_warmup) && params$iter_warmup < 100) {
      stop("Warmup iterations should be at least 100")
    }
    if (!is.null(params$adapt_delta) && (params$adapt_delta <= 0 || params$adapt_delta >= 1)) {
      stop("adapt_delta must be between 0 and 1")
    }
  } else if (computation_type == "jags") {
    # Validate JAGS parameters
    if (!is.null(params$n_chains) && (params$n_chains < 1 || params$n_chains > 10)) {
      stop("Number of chains must be between 1 and 10")
    }
    if (!is.null(params$n_burnin) && params$n_burnin < 100) {
      stop("Burn-in iterations should be at least 100")
    }
  }
  
  return(TRUE)
}