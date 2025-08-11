# tests/performance/benchmark.R
# Performance Benchmark Suite for PK/PD Application

library(microbenchmark)
library(bench)
library(data.table)
library(ggplot2)
library(jsonlite)

# Source required functions
source("R/utils.R")
source("R/pk_calculations.R")
source("R/plotting_functions.R")

#' Run comprehensive performance benchmarks
#'
#' @param output_file File to save benchmark results
#' @param iterations Number of iterations for each benchmark
#' @return List of benchmark results
run_benchmarks <- function(output_file = "benchmark-results.json", 
                          iterations = 100) {
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("           PK/PD Application Performance Benchmarks           \n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  results <- list()
  timestamp <- Sys.time()
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 1: PK Calculation Performance
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 1: PK Calculations\n")
  cat("  Testing concentration calculations...\n")
  
  # Test data
  params_1c <- list(CL = 5, Vc = 30)
  params_2c <- list(CL = 5, Vc = 30, Q = 2, Vp = 20)
  params_3c <- list(CL = 5, Vc = 30, Q1 = 2, Vp1 = 20, Q2 = 1, Vp2 = 10)
  regimen <- list(dose = 1000, tau = 8, tinf = 1, n_doses = 6)
  
  pk_bench <- microbenchmark(
    "1-Compartment" = {
      calculate_pk_1c <- function(params, regimen) {
        times <- seq(0, regimen$n_doses * regimen$tau, by = 0.1)
        k_el <- params$CL / params$Vc
        conc <- numeric(length(times))
        
        for (i in 1:regimen$n_doses) {
          dose_time <- (i - 1) * regimen$tau
          for (j in 1:length(times)) {
            if (times[j] >= dose_time) {
              t_after <- times[j] - dose_time
              if (t_after < regimen$tau) {
                conc[j] <- conc[j] + (regimen$dose / params$Vc) * exp(-k_el * t_after)
              }
            }
          }
        }
        return(conc)
      }
      calculate_pk_1c(params_1c, regimen)
    },
    
    "2-Compartment" = {
      calculate_pk_2c <- function(params, regimen) {
        times <- seq(0, regimen$n_doses * regimen$tau, by = 0.1)
        # Simplified 2-compartment calculation
        k_el <- params$CL / params$Vc
        k12 <- params$Q / params$Vc
        k21 <- params$Q / params$Vp
        conc <- numeric(length(times))
        
        for (i in 1:regimen$n_doses) {
          dose_time <- (i - 1) * regimen$tau
          for (j in 1:length(times)) {
            if (times[j] >= dose_time) {
              t_after <- times[j] - dose_time
              if (t_after < regimen$tau) {
                # Simplified bi-exponential
                A <- regimen$dose / params$Vc
                alpha <- k_el + k12
                conc[j] <- conc[j] + A * exp(-alpha * t_after)
              }
            }
          }
        }
        return(conc)
      }
      calculate_pk_2c(params_2c, regimen)
    },
    
    times = iterations
  )
  
  results$pk_calculations <- summary(pk_bench)
  print(results$pk_calculations)
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 2: Data Processing Performance
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 2: Data Processing\n")
  cat("  Testing data manipulation performance...\n")
  
  # Generate test data
  n_patients <- 100
  n_timepoints <- 50
  test_data <- data.table(
    patient_id = rep(1:n_patients, each = n_timepoints),
    time = rep(seq(0, 24, length.out = n_timepoints), n_patients),
    concentration = rnorm(n_patients * n_timepoints, 10, 2),
    dose = rep(c(100, 200), length.out = n_patients * n_timepoints)
  )
  
  data_bench <- microbenchmark(
    "Base R aggregation" = {
      aggregate(concentration ~ patient_id, 
                data = test_data, 
                FUN = function(x) c(mean = mean(x), max = max(x)))
    },
    
    "data.table aggregation" = {
      test_data[, .(
        mean_conc = mean(concentration),
        max_conc = max(concentration),
        auc = sum(diff(time) * (concentration[-1] + concentration[-.N]) / 2)
      ), by = patient_id]
    },
    
    "dplyr aggregation" = {
      test_data %>%
        dplyr::group_by(patient_id) %>%
        dplyr::summarise(
          mean_conc = mean(concentration),
          max_conc = max(concentration),
          .groups = "drop"
        )
    },
    
    times = iterations
  )
  
  results$data_processing <- summary(data_bench)
  print(results$data_processing)
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 3: Matrix Operations
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 3: Matrix Operations\n")
  cat("  Testing numerical computation performance...\n")
  
  # Test matrices
  mat_sizes <- c(10, 50, 100)
  
  matrix_benchmarks <- list()
  for (size in mat_sizes) {
    A <- matrix(rnorm(size^2), size, size)
    B <- matrix(rnorm(size^2), size, size)
    
    bench_result <- microbenchmark(
      "Matrix multiplication" = A %*% B,
      "Matrix inversion" = solve(A),
      "Eigenvalues" = eigen(A),
      "Cholesky decomposition" = chol(crossprod(A)),
      times = iterations
    )
    
    matrix_benchmarks[[paste0("size_", size)]] <- summary(bench_result)
  }
  
  results$matrix_operations <- matrix_benchmarks
  print(matrix_benchmarks[["size_50"]])
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 4: File I/O Performance
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 4: File I/O Operations\n")
  cat("  Testing read/write performance...\n")
  
  # Create temporary test files
  temp_dir <- tempdir()
  test_df <- data.frame(
    id = 1:1000,
    value = rnorm(1000),
    category = sample(letters, 1000, replace = TRUE)
  )
  
  csv_file <- file.path(temp_dir, "test.csv")
  rds_file <- file.path(temp_dir, "test.rds")
  
  # Write files first
  write.csv(test_df, csv_file, row.names = FALSE)
  saveRDS(test_df, rds_file)
  
  io_bench <- microbenchmark(
    "CSV write" = write.csv(test_df, csv_file, row.names = FALSE),
    "CSV read" = read.csv(csv_file),
    "RDS write" = saveRDS(test_df, rds_file),
    "RDS read" = readRDS(rds_file),
    "fwrite" = data.table::fwrite(test_df, csv_file),
    "fread" = data.table::fread(csv_file),
    times = iterations
  )
  
  results$file_io <- summary(io_bench)
  print(results$file_io)
  cat("\n")
  
  # Clean up temp files
  unlink(c(csv_file, rds_file))
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 5: Parallel Processing
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 5: Parallel Processing\n")
  cat("  Testing parallelization efficiency...\n")
  
  # Function to parallelize
  compute_intensive <- function(n) {
    result <- 0
    for (i in 1:n) {
      result <- result + sqrt(i) * log(i + 1)
    }
    return(result)
  }
  
  # Test with different core counts
  n_tasks <- 20
  task_complexity <- 10000
  
  if (.Platform$OS.type != "windows") {
    parallel_bench <- microbenchmark(
      "Sequential" = {
        lapply(rep(task_complexity, n_tasks), compute_intensive)
      },
      
      "Parallel-2cores" = {
        parallel::mclapply(rep(task_complexity, n_tasks), 
                          compute_intensive, 
                          mc.cores = 2)
      },
      
      "Parallel-4cores" = {
        cores_available <- parallel::detectCores()
        if (cores_available >= 4) {
          parallel::mclapply(rep(task_complexity, n_tasks), 
                           compute_intensive, 
                           mc.cores = 4)
        } else {
          parallel::mclapply(rep(task_complexity, n_tasks), 
                           compute_intensive, 
                           mc.cores = cores_available)
        }
      },
      
      times = 10  # Fewer iterations for parallel benchmarks
    )
    
    results$parallel_processing <- summary(parallel_bench)
    print(results$parallel_processing)
  } else {
    cat("  Skipping parallel benchmarks on Windows\n")
  }
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 6: Memory Usage
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 6: Memory Usage Analysis\n")
  cat("  Testing memory efficiency...\n")
  
  measure_memory <- function(expr) {
    gc_before <- gc(reset = TRUE)
    result <- eval(expr)
    gc_after <- gc()
    
    memory_used <- sum(gc_after[, 2]) - sum(gc_before[, 2])
    return(list(result = result, memory_mb = memory_used))
  }
  
  # Test memory usage for different operations
  memory_tests <- list(
    "Small dataset" = measure_memory(quote({
      data.frame(x = 1:1000, y = rnorm(1000))
    })),
    
    "Large dataset" = measure_memory(quote({
      data.frame(x = 1:100000, y = rnorm(100000))
    })),
    
    "Matrix operation" = measure_memory(quote({
      A <- matrix(rnorm(1000000), 1000, 1000)
      B <- solve(A)
    }))
  )
  
  results$memory_usage <- memory_tests
  
  for (test_name in names(memory_tests)) {
    cat(sprintf("  %s: %.2f MB\n", test_name, memory_tests[[test_name]]$memory_mb))
  }
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Benchmark 7: Optimization Algorithms
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Benchmark 7: Optimization Algorithms\n")
  cat("  Testing optimization performance...\n")
  
  # Test function (Rosenbrock)
  rosenbrock <- function(x) {
    n <- length(x)
    sum(100 * (x[2:n] - x[1:(n-1)]^2)^2 + (1 - x[1:(n-1)])^2)
  }
  
  # Starting point
  x0 <- rep(0, 10)
  
  optim_bench <- microbenchmark(
    "Nelder-Mead" = optim(x0, rosenbrock, method = "Nelder-Mead"),
    "BFGS" = optim(x0, rosenbrock, method = "BFGS"),
    "L-BFGS-B" = optim(x0, rosenbrock, method = "L-BFGS-B"),
    "CG" = optim(x0, rosenbrock, method = "CG"),
    times = 50
  )
  
  results$optimization <- summary(optim_bench)
  print(results$optimization)
  cat("\n")
  
  # ─────────────────────────────────────────────────────────────
  # Save Results
  # ─────────────────────────────────────────────────────────────
  
  cat("▶ Saving benchmark results...\n")
  
  # Prepare results for JSON export
  json_results <- list(
    metadata = list(
      timestamp = as.character(timestamp),
      r_version = R.version.string,
      platform = Sys.info()["sysname"],
      cpu_cores = parallel::detectCores(),
      iterations = iterations
    ),
    benchmarks = results
  )
  
  # Save as JSON
  write(toJSON(json_results, pretty = TRUE), output_file)
  cat(sprintf("  Results saved to: %s\n", output_file))
  
  # ─────────────────────────────────────────────────────────────
  # Generate Performance Report
  # ─────────────────────────────────────────────────────────────
  
  cat("\n▶ Generating performance report...\n")
  
  # Create performance plot
  plot_benchmarks <- function(bench_data, title) {
    df <- as.data.frame(bench_data)
    df$expr <- factor(df$expr, levels = unique(df$expr))
    
    p <- ggplot(df, aes(x = expr, y = mean)) +
      geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0.2) +
      coord_flip() +
      labs(title = title,
           x = "",
           y = "Time (milliseconds)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    return(p)
  }
  
  # Save plots
  if (!is.null(results$pk_calculations)) {
    p1 <- plot_benchmarks(results$pk_calculations, "PK Calculation Performance")
    ggsave("benchmark_pk_calculations.png", p1, width = 10, height = 6)
  }
  
  if (!is.null(results$data_processing)) {
    p2 <- plot_benchmarks(results$data_processing, "Data Processing Performance")
    ggsave("benchmark_data_processing.png", p2, width = 10, height = 6)
  }
  
  cat("  Performance plots saved\n")
  
  # ─────────────────────────────────────────────────────────────
  # Summary
  # ─────────────────────────────────────────────────────────────
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("                 Benchmark Summary                          \n")
  cat("═══════════════════════════════════════════════════════════\n")
  
  # Find fastest methods
  cat("\n🏆 Fastest Methods:\n")
  
  if (!is.null(results$pk_calculations)) {
    fastest_pk <- results$pk_calculations[which.min(results$pk_calculations$mean), "expr"]
    cat(sprintf("  • PK Calculations: %s\n", fastest_pk))
  }
  
  if (!is.null(results$data_processing)) {
    fastest_data <- results$data_processing[which.min(results$data_processing$mean), "expr"]
    cat(sprintf("  • Data Processing: %s\n", fastest_data))
  }
  
  if (!is.null(results$file_io)) {
    fastest_read <- results$file_io[grep("read", results$file_io$expr), ]
    fastest_read <- fastest_read[which.min(fastest_read$mean), "expr"]
    cat(sprintf("  • File Reading: %s\n", fastest_read))
  }
  
  cat("\n✅ Benchmarks completed successfully!\n\n")
  
  return(results)
}

# ─────────────────────────────────────────────────────────────
# Compare benchmark results
# ─────────────────────────────────────────────────────────────

compare_benchmarks <- function(baseline_file, current_file) {
  baseline <- fromJSON(baseline_file)
  current <- fromJSON(current_file)
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("              Performance Comparison                        \n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  # Compare PK calculations
  if (!is.null(baseline$benchmarks$pk_calculations) && 
      !is.null(current$benchmarks$pk_calculations)) {
    
    cat("PK Calculations:\n")
    for (i in 1:nrow(baseline$benchmarks$pk_calculations)) {
      baseline_time <- baseline$benchmarks$pk_calculations[i, "mean"]
      current_time <- current$benchmarks$pk_calculations[i, "mean"]
      improvement <- (baseline_time - current_time) / baseline_time * 100
      
      symbol <- if (improvement > 0) "✅" else "❌"
      cat(sprintf("  %s %s: %.1f%% %s\n",
                 symbol,
                 baseline$benchmarks$pk_calculations[i, "expr"],
                 abs(improvement),
                 if (improvement > 0) "faster" else "slower"))
    }
  }
  
  cat("\n")
  
  # Overall performance score
  total_baseline <- sum(unlist(lapply(baseline$benchmarks, function(x) {
    if (is.data.frame(x)) sum(x$mean, na.rm = TRUE) else 0
  })))
  
  total_current <- sum(unlist(lapply(current$benchmarks, function(x) {
    if (is.data.frame(x)) sum(x$mean, na.rm = TRUE) else 0
  })))
  
  overall_improvement <- (total_baseline - total_current) / total_baseline * 100
  
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("Overall Performance: %.1f%% %s\n",
             abs(overall_improvement),
             if (overall_improvement > 0) "IMPROVEMENT ✅" else "REGRESSION ❌"))
  cat("═══════════════════════════════════════════════════════════\n\n")
}

# Run benchmarks if executed directly
if (!interactive()) {
  results <- run_benchmarks()
}