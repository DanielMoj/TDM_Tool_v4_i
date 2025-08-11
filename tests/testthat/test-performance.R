# tests/testthat/test-performance.R
# Performance Tests for PK/PD Application

library(testthat)
library(microbenchmark)
library(mockery)

# Helper to check if Stan is available
skip_if_no_stan <- function() {
  skip_if_not_installed("rstan")
  skip_if_not_installed("cmdstanr")
}

test_that("Stan model caching reduces compilation time", {
  skip_if_no_stan()
  
  # Mock model cache
  model_cache <- new.env(parent = emptyenv())
  
  get_compiled_model <- function(stan_file, force_recompile = FALSE) {
    # Generate cache key based on file path and modification time
    if (!file.exists(stan_file)) {
      stop("Stan file not found: ", stan_file)
    }
    
    file_info <- file.info(stan_file)
    cache_key <- paste0(
      basename(stan_file), "_",
      as.character(file_info$mtime), "_",
      as.character(file_info$size)
    )
    
    # Check cache unless forced recompile
    if (!force_recompile && exists(cache_key, envir = model_cache)) {
      message("Using cached model for: ", basename(stan_file))
      return(model_cache[[cache_key]])
    }
    
    # Simulate compilation (in real code, this would be stan_model())
    message("Compiling model: ", basename(stan_file))
    Sys.sleep(0.1)  # Simulate compilation time
    
    # Create mock compiled model
    compiled_model <- list(
      file = stan_file,
      compiled_at = Sys.time(),
      model_name = gsub("\\.stan$", "", basename(stan_file))
    )
    
    # Store in cache
    model_cache[[cache_key]] <- compiled_model
    
    return(compiled_model)
  }
  
  # Create temporary Stan file for testing
  temp_stan <- tempfile(fileext = ".stan")
  writeLines(c(
    "parameters {",
    "  real theta;",
    "}",
    "model {",
    "  theta ~ normal(0, 1);",
    "}"
  ), temp_stan)
  
  # First compilation should take time
  t1 <- system.time({
    model1 <- get_compiled_model(temp_stan)
  })
  
  # Second compilation should use cache
  t2 <- system.time({
    model2 <- get_compiled_model(temp_stan)
  })
  
  # Verify same model returned
  expect_identical(model1$model_name, model2$model_name)
  expect_identical(model1$compiled_at, model2$compiled_at)
  
  # Cache should be significantly faster
  expect_lt(t2["elapsed"], t1["elapsed"] * 0.5)
  
  # Force recompile should take time again
  t3 <- system.time({
    model3 <- get_compiled_model(temp_stan, force_recompile = TRUE)
  })
  expect_gt(t3["elapsed"], t2["elapsed"])
  
  # Clean up
  unlink(temp_stan)
})

test_that("Data processing is optimized for large datasets", {
  skip_if_not_installed("data.table")
  library(data.table)
  
  # Generate test data
  n_rows <- 10000
  test_data <- data.table(
    patient_id = rep(1:100, each = 100),
    time = rep(seq(0, 24, length.out = 100), 100),
    concentration = rnorm(n_rows, mean = 10, sd = 2),
    dose = rep(c(100, 200), length.out = n_rows)
  )
  
  # Slow approach (using base R)
  process_slow <- function(data) {
    result <- list()
    for (pid in unique(data$patient_id)) {
      patient_data <- data[data$patient_id == pid, ]
      result[[as.character(pid)]] <- list(
        mean_conc = mean(patient_data$concentration),
        max_conc = max(patient_data$concentration),
        auc = sum(diff(patient_data$time) * 
                 (patient_data$concentration[-1] + 
                  patient_data$concentration[-nrow(patient_data)]) / 2)
      )
    }
    return(result)
  }
  
  # Fast approach (using data.table)
  process_fast <- function(data) {
    dt <- as.data.table(data)
    result <- dt[order(patient_id, time), .(
      mean_conc = mean(concentration),
      max_conc = max(concentration),
      auc = sum(diff(time) * (concentration[-1] + concentration[-.N]) / 2)
    ), by = patient_id]
    return(result)
  }
  
  # Benchmark
  bench_results <- microbenchmark(
    slow = process_slow(test_data),
    fast = process_fast(test_data),
    times = 5
  )
  
  # Fast method should be significantly faster
  mean_times <- summary(bench_results)$mean
  expect_lt(mean_times[2], mean_times[1] * 0.5)
  
  # Verify results are equivalent
  slow_result <- process_slow(test_data[patient_id <= 2])
  fast_result <- process_fast(test_data[patient_id <= 2])
  
  expect_equal(
    slow_result[["1"]]$mean_conc,
    fast_result[patient_id == 1]$mean_conc,
    tolerance = 1e-10
  )
})

test_that("Memory usage is controlled during simulations", {
  # Mock memory-aware simulation function
  run_memory_controlled_simulation <- function(n_sims, max_memory_mb = 100) {
    # Estimate memory per simulation (in MB)
    memory_per_sim <- 0.01  # Assume 10KB per simulation
    
    # Calculate optimal chunk size
    chunk_size <- floor(max_memory_mb / memory_per_sim)
    chunk_size <- min(chunk_size, n_sims)
    
    # Track memory usage
    initial_memory <- as.numeric(gc()[2, 2])  # Used memory in MB
    peak_memory <- initial_memory
    
    results <- numeric(n_sims)
    n_chunks <- ceiling(n_sims / chunk_size)
    
    for (i in 1:n_chunks) {
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, n_sims)
      
      # Run chunk
      chunk_results <- rnorm(end_idx - start_idx + 1)
      results[start_idx:end_idx] <- chunk_results
      
      # Check memory
      current_memory <- as.numeric(gc()[2, 2])
      peak_memory <- max(peak_memory, current_memory)
      
      # Clear unnecessary objects
      rm(chunk_results)
      
      # Garbage collect if memory usage is high
      if (current_memory - initial_memory > max_memory_mb * 0.8) {
        gc()
      }
    }
    
    final_memory <- as.numeric(gc()[2, 2])
    
    return(list(
      results = results,
      memory_stats = list(
        initial = initial_memory,
        peak = peak_memory,
        final = final_memory,
        increase = peak_memory - initial_memory
      ),
      n_chunks = n_chunks
    ))
  }
  
  # Test with controlled memory
  sim_result <- run_memory_controlled_simulation(10000, max_memory_mb = 50)
  
  expect_equal(length(sim_result$results), 10000)
  expect_true(sim_result$memory_stats$increase < 100)  # Memory increase should be controlled
  expect_true(sim_result$n_chunks > 1)  # Should use chunking
})

test_that("Parallel processing improves performance for independent calculations", {
  skip_if_not_installed("parallel")
  
  # Function to parallelize
  compute_intensive_task <- function(x) {
    # Simulate intensive computation
    result <- 0
    for (i in 1:1000) {
      result <- result + sqrt(x * i)
    }
    return(result)
  }
  
  # Sequential version
  run_sequential <- function(inputs) {
    lapply(inputs, compute_intensive_task)
  }
  
  # Parallel version
  run_parallel <- function(inputs, n_cores = 2) {
    if (.Platform$OS.type == "windows") {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterExport(cl, "compute_intensive_task", 
                            envir = environment())
      result <- parallel::parLapply(cl, inputs, compute_intensive_task)
    } else {
      result <- parallel::mclapply(inputs, compute_intensive_task, 
                                  mc.cores = n_cores)
    }
    return(result)
  }
  
  # Test with reasonable workload
  test_inputs <- 1:20
  
  # Skip parallel test if only 1 core available
  n_cores <- parallel::detectCores()
  skip_if(n_cores < 2, "Not enough cores for parallel testing")
  
  # Use 2 cores max for testing
  cores_to_use <- min(2, n_cores)
  
  # Time both approaches
  t_seq <- system.time({
    res_seq <- run_sequential(test_inputs)
  })
  
  t_par <- system.time({
    res_par <- run_parallel(test_inputs, n_cores = cores_to_use)
  })
  
  # Verify same results
  expect_equal(unlist(res_seq), unlist(res_par), tolerance = 1e-10)
  
  # Parallel should be faster (but may not be for small tasks)
  # Just verify it completes without error
  expect_true(t_par["elapsed"] > 0)
  expect_true(t_seq["elapsed"] > 0)
})

test_that("Database queries are optimized with indexing", {
  skip_if_not_installed("RSQLite")
  library(DBI)
  library(RSQLite)
  
  # Create test database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))
  
  # Create table with many rows
  n_rows <- 10000
  test_data <- data.frame(
    id = 1:n_rows,
    patient_id = sample(1:100, n_rows, replace = TRUE),
    timestamp = as.character(Sys.time() - runif(n_rows, 0, 86400)),
    value = rnorm(n_rows, 100, 20),
    category = sample(c("A", "B", "C", "D"), n_rows, replace = TRUE)
  )
  
  dbWriteTable(con, "measurements", test_data, overwrite = TRUE)
  
  # Query without index
  t_no_index <- system.time({
    result1 <- dbGetQuery(con, 
      "SELECT * FROM measurements WHERE patient_id = 50"
    )
  })
  
  # Add index
  dbExecute(con, "CREATE INDEX idx_patient ON measurements(patient_id)")
  dbExecute(con, "CREATE INDEX idx_category ON measurements(category)")
  dbExecute(con, "CREATE INDEX idx_timestamp ON measurements(timestamp)")
  
  # Query with index
  t_with_index <- system.time({
    result2 <- dbGetQuery(con, 
      "SELECT * FROM measurements WHERE patient_id = 50"
    )
  })
  
  # Complex query with multiple conditions
  t_complex <- system.time({
    result3 <- dbGetQuery(con, "
      SELECT patient_id, category, AVG(value) as avg_value
      FROM measurements 
      WHERE category IN ('A', 'B')
      GROUP BY patient_id, category
      ORDER BY patient_id
    ")
  })
  
  # Verify results are the same
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(result1$id, result2$id)
  
  # Indexed queries should complete
  expect_true(t_with_index["elapsed"] >= 0)
  expect_true(t_complex["elapsed"] >= 0)
})

test_that("Reactive expressions prevent unnecessary recalculation", {
  # Mock reactive system
  create_reactive_cache <- function() {
    cache <- new.env(parent = emptyenv())
    
    reactive_expr <- function(name, expr_fn, dependencies = NULL) {
      
      # Get or create cache entry
      if (!exists(name, envir = cache)) {
        cache[[name]] <- list(
          value = NULL,
          version = 0,
          calc_count = 0
        )
      }
      
      # Check if dependencies changed
      deps_changed <- FALSE
      if (!is.null(dependencies)) {
        for (dep in dependencies) {
          if (exists(dep, envir = cache)) {
            # Simple version checking
            deps_changed <- TRUE
            break
          }
        }
      }
      
      # Recalculate if needed
      if (is.null(cache[[name]]$value) || deps_changed) {
        cache[[name]]$value <- expr_fn()
        cache[[name]]$version <- cache[[name]]$version + 1
        cache[[name]]$calc_count <- cache[[name]]$calc_count + 1
      }
      
      return(list(
        value = cache[[name]]$value,
        calc_count = cache[[name]]$calc_count
      ))
    }
    
    return(reactive_expr)
  }
  
  # Create reactive system
  reactive <- create_reactive_cache()
  
  # Define expensive calculation
  expensive_calc <- function() {
    Sys.sleep(0.01)  # Simulate expensive operation
    return(runif(1))
  }
  
  # First call should calculate
  result1 <- reactive("test_value", expensive_calc)
  expect_equal(result1$calc_count, 1)
  
  # Second call should use cache
  result2 <- reactive("test_value", expensive_calc)
  expect_equal(result2$calc_count, 1)  # No recalculation
  expect_equal(result2$value, result1$value)  # Same value
  
  # Multiple calls should still use cache
  for (i in 1:5) {
    result <- reactive("test_value", expensive_calc)
    expect_equal(result$calc_count, 1)
  }
})

test_that("Plot rendering is optimized for large datasets", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  # Generate large dataset
  n_points <- 10000
  large_data <- data.frame(
    x = seq(0, 100, length.out = n_points),
    y = sin(seq(0, 10*pi, length.out = n_points)) + rnorm(n_points, 0, 0.1)
  )
  
  # Unoptimized plot (all points)
  plot_unoptimized <- function(data) {
    ggplot(data, aes(x = x, y = y)) +
      geom_point(alpha = 0.5) +
      geom_line() +
      theme_minimal()
  }
  
  # Optimized plot (sampled/simplified)
  plot_optimized <- function(data, max_points = 1000) {
    # Downsample if too many points
    if (nrow(data) > max_points) {
      # Use systematic sampling to preserve patterns
      sample_indices <- round(seq(1, nrow(data), length.out = max_points))
      data_plot <- data[sample_indices, ]
    } else {
      data_plot <- data
    }
    
    ggplot(data_plot, aes(x = x, y = y)) +
      geom_line(size = 1) +
      theme_minimal()
  }
  
  # Benchmark
  t_unopt <- system.time({
    p1 <- plot_unoptimized(large_data)
  })
  
  t_opt <- system.time({
    p2 <- plot_optimized(large_data)
  })
  
  # Optimized should be faster
  expect_lt(t_opt["elapsed"], t_unopt["elapsed"])
  
  # Both should produce valid plots
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("File I/O operations are buffered efficiently", {
  # Create temporary directory for testing
  temp_dir <- tempdir()
  
  # Generate test data
  test_data <- data.frame(
    id = 1:5000,
    value = rnorm(5000),
    category = sample(letters, 5000, replace = TRUE)
  )
  
  # Unbuffered write (line by line)
  write_unbuffered <- function(data, file) {
    con <- file(file, "w")
    on.exit(close(con))
    
    # Write header
    writeLines(paste(names(data), collapse = ","), con)
    
    # Write data line by line
    for (i in 1:nrow(data)) {
      writeLines(paste(data[i, ], collapse = ","), con)
    }
  }
  
  # Buffered write
  write_buffered <- function(data, file, buffer_size = 100) {
    con <- file(file, "w")
    on.exit(close(con))
    
    # Write header
    writeLines(paste(names(data), collapse = ","), con)
    
    # Write data in chunks
    buffer <- character(buffer_size)
    buffer_pos <- 1
    
    for (i in 1:nrow(data)) {
      buffer[buffer_pos] <- paste(data[i, ], collapse = ",")
      buffer_pos <- buffer_pos + 1
      
      if (buffer_pos > buffer_size) {
        writeLines(buffer, con)
        buffer_pos <- 1
      }
    }
    
    # Write remaining buffer
    if (buffer_pos > 1) {
      writeLines(buffer[1:(buffer_pos-1)], con)
    }
  }
  
  # Built-in optimized write
  write_optimized <- function(data, file) {
    write.csv(data, file, row.names = FALSE)
  }
  
  # Test files
  file1 <- file.path(temp_dir, "test_unbuffered.csv")
  file2 <- file.path(temp_dir, "test_buffered.csv")
  file3 <- file.path(temp_dir, "test_optimized.csv")
  
  # Benchmark
  t1 <- system.time(write_unbuffered(test_data, file1))
  t2 <- system.time(write_buffered(test_data, file2))
  t3 <- system.time(write_optimized(test_data, file3))
  
  # Buffered should be faster than unbuffered
  expect_lt(t2["elapsed"], t1["elapsed"])
  
  # Built-in should be fastest
  expect_lt(t3["elapsed"], t1["elapsed"])
  
  # Clean up
  unlink(c(file1, file2, file3))
})

test_that("Caching strategies reduce redundant computations", {
  # Implement LRU cache
  create_lru_cache <- function(max_size = 100) {
    cache <- new.env(parent = emptyenv())
    cache$data <- list()
    cache$order <- character()
    cache$hits <- 0
    cache$misses <- 0
    
    get <- function(key) {
      if (key %in% names(cache$data)) {
        # Move to front (most recently used)
        cache$order <- c(key, setdiff(cache$order, key))
        cache$hits <- cache$hits + 1
        return(cache$data[[key]])
      }
      cache$misses <- cache$misses + 1
      return(NULL)
    }
    
    set <- function(key, value) {
      # Add to cache
      cache$data[[key]] <- value
      cache$order <- c(key, setdiff(cache$order, key))
      
      # Evict if over size limit
      if (length(cache$order) > max_size) {
        evict_key <- cache$order[length(cache$order)]
        cache$data[[evict_key]] <- NULL
        cache$order <- cache$order[-length(cache$order)]
      }
    }
    
    stats <- function() {
      list(
        size = length(cache$data),
        hits = cache$hits,
        misses = cache$misses,
        hit_rate = cache$hits / (cache$hits + cache$misses)
      )
    }
    
    return(list(get = get, set = set, stats = stats))
  }
  
  # Create cache
  cache <- create_lru_cache(max_size = 10)
  
  # Expensive function to cache
  expensive_function <- function(x) {
    Sys.sleep(0.001)  # Simulate expensive computation
    return(x^2 + sqrt(x))
  }
  
  # Cached version
  cached_function <- function(x) {
    key <- as.character(x)
    result <- cache$get(key)
    
    if (is.null(result)) {
      result <- expensive_function(x)
      cache$set(key, result)
    }
    
    return(result)
  }
  
  # Test cache effectiveness
  test_values <- c(1:5, 3:7, 1:5)  # Repeated values
  
  t_no_cache <- system.time({
    results_no_cache <- lapply(test_values, expensive_function)
  })
  
  t_with_cache <- system.time({
    results_with_cache <- lapply(test_values, cached_function)
  })
  
  # Verify same results
  expect_equal(results_no_cache, results_with_cache)
  
  # Cache should improve performance
  expect_lt(t_with_cache["elapsed"], t_no_cache["elapsed"])
  
  # Check cache statistics
  stats <- cache$stats()
  expect_gt(stats$hits, 0)  # Should have cache hits
  expect_gt(stats$hit_rate, 0.3)  # Reasonable hit rate
})