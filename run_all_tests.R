# run_all_tests.R
# Test Runner with Coverage Report for PK/PD Application

# Load required libraries
library(testthat)
library(covr)

#' Run all tests with detailed reporting
#'
#' @param test_dir Directory containing test files (default: "tests/testthat")
#' @param coverage Whether to generate coverage report (default: TRUE)
#' @param verbose Verbose output (default: TRUE)
#' @return Test results and coverage metrics
run_all_tests <- function(test_dir = "tests/testthat", 
                         coverage = TRUE, 
                         verbose = TRUE) {
  
  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║           PK/PD Application Test Suite                      ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  # Check if test directory exists
  if (!dir.exists(test_dir)) {
    stop("Test directory not found: ", test_dir)
  }
  
  # Initialize results storage
  results <- list(
    start_time = Sys.time(),
    test_results = NULL,
    coverage_report = NULL,
    summary = list()
  )
  
  # Run tests by category
  test_categories <- list(
    security = "test-security.R",
    fixes = "test-fixes.R",
    performance = "test-performance.R",
    integration = "test-integration.R"
  )
  
  category_results <- list()
  
  for (category in names(test_categories)) {
    test_file <- file.path(test_dir, test_categories[[category]])
    
    if (file.exists(test_file)) {
      cat(sprintf("\n▶ Running %s tests...\n", toupper(category)))
      cat(rep("-", 50), "\n", sep = "")
      
      # Run tests for this category
      category_result <- tryCatch({
        test_file(test_file, reporter = if(verbose) "progress" else "silent")
      }, error = function(e) {
        cat(sprintf("  ✗ Error in %s tests: %s\n", category, e$message))
        return(NULL)
      })
      
      category_results[[category]] <- category_result
      
      # Print summary for this category
      if (!is.null(category_result)) {
        passed <- sum(sapply(category_result, function(x) x$passed))
        failed <- sum(sapply(category_result, function(x) x$failed))
        skipped <- sum(sapply(category_result, function(x) x$skipped))
        
        if (failed == 0) {
          cat(sprintf("  ✓ %s tests: %d passed, %d skipped\n", 
                     toupper(category), passed, skipped))
        } else {
          cat(sprintf("  ✗ %s tests: %d passed, %d FAILED, %d skipped\n", 
                     toupper(category), passed, failed, skipped))
        }
      }
    } else {
      cat(sprintf("  ⚠ %s test file not found: %s\n", category, test_file))
    }
  }
  
  results$test_results <- category_results
  
  # Run all tests together for overall statistics
  cat("\n\n▶ Running complete test suite...\n")
  cat(rep("=", 60), "\n", sep = "")
  
  overall_results <- test_dir(test_dir, reporter = if(verbose) "summary" else "silent")
  
  # Calculate overall statistics
  total_tests <- length(overall_results)
  passed_tests <- sum(sapply(overall_results, function(x) {
    if (inherits(x, "expectation_success")) 1 else 0
  }))
  failed_tests <- sum(sapply(overall_results, function(x) {
    if (inherits(x, "expectation_failure")) 1 else 0
  }))
  error_tests <- sum(sapply(overall_results, function(x) {
    if (inherits(x, "expectation_error")) 1 else 0
  }))
  skipped_tests <- sum(sapply(overall_results, function(x) {
    if (inherits(x, "expectation_skip")) 1 else 0
  }))
  
  # Generate coverage report if requested
  if (coverage) {
    cat("\n\n▶ Generating code coverage report...\n")
    cat(rep("-", 50), "\n", sep = "")
    
    tryCatch({
      # Define source files to check coverage
      source_files <- c(
        "R/utils.R",
        "R/pk_calculations.R",
        "R/plotting_functions.R",
        "R/database_functions.R",
        "R/security_functions.R"
      )
      
      # Filter to existing files
      existing_files <- source_files[file.exists(source_files)]
      
      if (length(existing_files) > 0) {
        cov <- package_coverage(
          path = ".",
          type = "tests",
          code = existing_files
        )
        
        # Calculate coverage percentage
        coverage_pct <- percent_coverage(cov)
        
        results$coverage_report <- list(
          overall = coverage_pct,
          by_file = coverage_by_file(cov),
          zero_coverage = zero_coverage(cov)
        )
        
        cat(sprintf("  📊 Overall code coverage: %.1f%%\n", coverage_pct))
        
        # Show file-by-file coverage
        if (length(results$coverage_report$by_file) > 0) {
          cat("\n  Coverage by file:\n")
          for (i in 1:nrow(results$coverage_report$by_file)) {
            file_cov <- results$coverage_report$by_file[i, ]
            cat(sprintf("    • %s: %.1f%%\n", 
                       basename(as.character(file_cov$filename)), 
                       file_cov$coverage))
          }
        }
        
        # Highlight functions with zero coverage
        if (length(results$coverage_report$zero_coverage) > 0) {
          cat("\n  ⚠ Functions with zero coverage:\n")
          for (func in head(results$coverage_report$zero_coverage, 10)) {
            cat(sprintf("    • %s\n", func$function_name))
          }
        }
        
      } else {
        cat("  ⚠ No source files found for coverage analysis\n")
      }
      
    }, error = function(e) {
      cat(sprintf("  ✗ Coverage report failed: %s\n", e$message))
      results$coverage_report <- NULL
    })
  }
  
  # Generate summary
  results$end_time <- Sys.time()
  results$duration <- difftime(results$end_time, results$start_time, units = "secs")
  
  results$summary <- list(
    total_tests = total_tests,
    passed = passed_tests,
    failed = failed_tests,
    errors = error_tests,
    skipped = skipped_tests,
    pass_rate = if(total_tests > 0) passed_tests / total_tests * 100 else 0,
    duration = results$duration
  )
  
  # Print final summary
  cat("\n\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║                    TEST SUMMARY                           ║\n")
  cat("╠══════════════════════════════════════════════════════════╣\n")
  
  if (results$summary$failed == 0 && results$summary$errors == 0) {
    cat("║  ✅ ALL TESTS PASSED!                                    ║\n")
  } else {
    cat("║  ❌ SOME TESTS FAILED                                    ║\n")
  }
  
  cat("╠══════════════════════════════════════════════════════════╣\n")
  cat(sprintf("║  Total Tests:    %-40d ║\n", results$summary$total_tests))
  cat(sprintf("║  Passed:         %-40d ║\n", results$summary$passed))
  cat(sprintf("║  Failed:         %-40d ║\n", results$summary$failed))
  cat(sprintf("║  Errors:         %-40d ║\n", results$summary$errors))
  cat(sprintf("║  Skipped:        %-40d ║\n", results$summary$skipped))
  cat(sprintf("║  Pass Rate:      %-39.1f%% ║\n", results$summary$pass_rate))
  cat(sprintf("║  Duration:       %-38.2f sec ║\n", as.numeric(results$summary$duration)))
  
  if (!is.null(results$coverage_report)) {
    cat("╠══════════════════════════════════════════════════════════╣\n")
    cat(sprintf("║  Code Coverage:  %-39.1f%% ║\n", results$coverage_report$overall))
  }
  
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  
  # Return results
  invisible(results)
}

#' Generate HTML test report
#'
#' @param results Test results from run_all_tests()
#' @param output_file Output HTML file path
generate_html_report <- function(results, output_file = "test_report.html") {
  
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>PK/PD Application Test Report</title>
    <meta charset="UTF-8">
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }
        .summary-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .summary-card {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .summary-card h3 {
            margin-top: 0;
            color: #667eea;
        }
        .summary-card .value {
            font-size: 2em;
            font-weight: bold;
        }
        .passed { color: #10b981; }
        .failed { color: #ef4444; }
        .skipped { color: #f59e0b; }
        .coverage-bar {
            width: 100%%;
            height: 30px;
            background: #e5e7eb;
            border-radius: 15px;
            overflow: hidden;
            position: relative;
        }
        .coverage-fill {
            height: 100%%;
            background: linear-gradient(90deg, #10b981 0%%, #059669 100%%);
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: bold;
        }
        table {
            width: 100%%;
            background: white;
            border-radius: 8px;
            overflow: hidden;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        th {
            background: #f3f4f6;
            padding: 12px;
            text-align: left;
            font-weight: 600;
        }
        td {
            padding: 12px;
            border-top: 1px solid #e5e7eb;
        }
        .timestamp {
            color: #6b7280;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>PK/PD Application Test Report</h1>
        <p class="timestamp">Generated: %s</p>
        <p>Duration: %.2f seconds</p>
    </div>
    
    <div class="summary-grid">
        <div class="summary-card">
            <h3>Total Tests</h3>
            <div class="value">%d</div>
        </div>
        <div class="summary-card">
            <h3>Passed</h3>
            <div class="value passed">%d</div>
        </div>
        <div class="summary-card">
            <h3>Failed</h3>
            <div class="value failed">%d</div>
        </div>
        <div class="summary-card">
            <h3>Skipped</h3>
            <div class="value skipped">%d</div>
        </div>
    </div>
    
    <div class="summary-card">
        <h3>Code Coverage</h3>
        <div class="coverage-bar">
            <div class="coverage-fill" style="width: %.1f%%;">
                %.1f%%
            </div>
        </div>
    </div>
    
    <h2>Test Categories</h2>
    <table>
        <thead>
            <tr>
                <th>Category</th>
                <th>Status</th>
                <th>Tests</th>
                <th>Pass Rate</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Security</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>
            <tr>
                <td>Bug Fixes</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>
            <tr>
                <td>Performance</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>
            <tr>
                <td>Integration</td>
                <td>%s</td>
                <td>%s</td>
                <td>%s</td>
            </tr>
        </tbody>
    </table>
</body>
</html>',
    format(results$end_time, "%Y-%m-%d %H:%M:%S"),
    as.numeric(results$duration),
    results$summary$total_tests,
    results$summary$passed,
    results$summary$failed,
    results$summary$skipped,
    ifelse(is.null(results$coverage_report), 0, results$coverage_report$overall),
    ifelse(is.null(results$coverage_report), 0, results$coverage_report$overall),
    # Category placeholders - would be filled from actual results
    "✓", "-", "-",
    "✓", "-", "-",
    "✓", "-", "-",
    "✓", "-", "-"
  )
  
  writeLines(html_content, output_file)
  cat(sprintf("\n📄 HTML report generated: %s\n", output_file))
}

#' Check if all critical fixes are tested
#'
#' @return List of untested critical functions
check_critical_coverage <- function() {
  critical_functions <- c(
    # Security functions
    "sanitize_input",
    "validate_sql_params",
    "hash_password",
    "verify_password",
    "check_session",
    
    # Core fixes
    "compute_metrics_for_draw",
    "%||%",
    "get_compiled_model",
    
    # Data processing
    "process_pk_data",
    "calculate_auc",
    "calculate_cmax",
    
    # Error handling
    "safe_numeric",
    "validate_inputs",
    "handle_error"
  )
  
  # This would check actual coverage in production
  # For now, return placeholder
  return(list(
    critical_functions = critical_functions,
    covered = character(0),
    not_covered = critical_functions,
    coverage_pct = 0
  ))
}

# Main execution
if (interactive()) {
  cat("\n🚀 Starting PK/PD Application Test Suite\n")
  cat("========================================\n")
  
  # Check for required packages
  required_packages <- c("testthat", "covr")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    cat("\n⚠ Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, quiet = TRUE)
  }
  
  # Run tests
  test_results <- run_all_tests(
    test_dir = "tests/testthat",
    coverage = TRUE,
    verbose = TRUE
  )
  
  # Generate HTML report
  generate_html_report(test_results)
  
  # Check critical coverage
  critical_check <- check_critical_coverage()
  
  if (length(critical_check$not_covered) > 0) {
    cat("\n⚠ Warning: Some critical functions lack test coverage:\n")
    for (func in head(critical_check$not_covered, 5)) {
      cat(sprintf("  • %s\n", func))
    }
  }
  
  # Exit with appropriate code
  if (test_results$summary$failed > 0 || test_results$summary$errors > 0) {
    cat("\n❌ Tests failed. Please fix issues before deployment.\n")
    quit(status = 1)
  } else {
    cat("\n✅ All tests passed! Ready for deployment.\n")
    quit(status = 0)
  }
}