# PK/PD Application Test Suite

## 📋 Overview

This comprehensive test suite validates all critical fixes and functionality of the PK/PD Shiny application. The tests are organized into four main categories:

1. **Security Tests** - SQL injection prevention, password hashing, XSS protection
2. **Bug Fix Tests** - Validation of all implemented fixes
3. **Performance Tests** - Optimization verification and benchmarks
4. **Integration Tests** - End-to-end workflow validation

## 🚀 Quick Start

### Running All Tests

```r
# Run complete test suite with coverage
source("run_all_tests.R")
test_results <- run_all_tests()
```

### Running Specific Test Categories

```r
library(testthat)

# Security tests only
test_file("tests/testthat/test-security.R")

# Bug fix tests only
test_file("tests/testthat/test-fixes.R")

# Performance tests only
test_file("tests/testthat/test-performance.R")

# Integration tests only
test_file("tests/testthat/test-integration.R")
```

## 📊 Code Coverage

### Generate Coverage Report

```r
library(covr)

# Full coverage report
cov <- package_coverage(type = "tests")
report(cov)

# Coverage for specific files
cov <- file_coverage(
  source_files = c("R/utils.R", "R/pk_calculations.R"),
  test_files = c("tests/testthat/test-fixes.R")
)
percent_coverage(cov)
```

### View Coverage in Browser

```r
# Interactive HTML report
covr::report(cov, browse = TRUE)
```

## ✅ Test Categories

### 1. Security Tests (`test-security.R`)

- ✓ SQL injection prevention
- ✓ Password hashing and verification
- ✓ XSS attack prevention
- ✓ File upload validation
- ✓ Session management
- ✓ Rate limiting
- ✓ Audit logging
- ✓ Data encryption

**Run individually:**
```r
test_file("tests/testthat/test-security.R")
```

### 2. Bug Fix Tests (`test-fixes.R`)

- ✓ `n_intervals` definition in `compute_metrics_for_draw`
- ✓ Null coalescing operator (`%||%`)
- ✓ Data.table edge cases
- ✓ Reactive value initialization
- ✓ Model selection handling
- ✓ Input validation
- ✓ Error message formatting
- ✓ Plot rendering with missing data
- ✓ Cross-platform file paths
- ✓ Memory management for large simulations

**Run individually:**
```r
test_file("tests/testthat/test-fixes.R")
```

### 3. Performance Tests (`test-performance.R`)

- ✓ Stan model caching
- ✓ Data processing optimization
- ✓ Memory usage control
- ✓ Parallel processing
- ✓ Database query optimization
- ✓ Reactive caching
- ✓ Plot rendering optimization
- ✓ File I/O buffering
- ✓ LRU cache implementation

**Run individually:**
```r
test_file("tests/testthat/test-performance.R")
```

### 4. Integration Tests (`test-integration.R`)

- ✓ Application startup
- ✓ Database initialization
- ✓ Complete PK simulation workflow
- ✓ Error handling and recovery
- ✓ User input validation
- ✓ Session management
- ✓ Data export functionality
- ✓ Concurrent operations
- ✓ Logging system

**Run individually:**
```r
test_file("tests/testthat/test-integration.R")
```

## 🎯 Acceptance Criteria

### ✅ All Tests Must Pass
```r
# Verify all tests pass
results <- test_dir("tests/testthat")
all(sapply(results, function(x) x$passed))
```

### ✅ Code Coverage > 80%
```r
# Check coverage percentage
cov <- package_coverage(type = "tests")
percent_coverage(cov) > 80
```

### ✅ Critical Functions Covered
```r
# List of critical functions that must be tested
critical_functions <- c(
  "compute_metrics_for_draw",
  "sanitize_input",
  "validate_sql_params",
  "get_compiled_model"
)

# Verify coverage
zero_cov <- zero_coverage(cov)
!any(critical_functions %in% zero_cov$functions)
```

## 🔧 CI/CD Integration

### GitHub Actions

The test suite is automatically run via GitHub Actions on:
- Every push to `main` or `develop`
- Every pull request
- Daily at 2 AM UTC

Configuration: `.github/workflows/ci.yml`

### Local CI Simulation

```bash
# Run tests as CI would
R CMD check .
R -e "testthat::test_dir('tests/testthat')"
R -e "covr::package_coverage(type = 'tests')"
```

## 📈 Performance Benchmarks

### Run Benchmarks

```r
source("tests/performance/benchmark.R")
run_benchmarks()
```

### Compare Performance

```r
# Before optimization
baseline <- run_benchmarks(version = "baseline")

# After optimization
optimized <- run_benchmarks(version = "optimized")

# Compare
compare_benchmarks(baseline, optimized)
```

## 🐛 Debugging Failed Tests

### Verbose Output

```r
# Run with detailed output
test_file("tests/testthat/test-fixes.R", reporter = "progress")
```

### Interactive Debugging

```r
# Debug specific test
debugonce(test_that)
test_file("tests/testthat/test-fixes.R")
```

### Test Isolation

```r
# Run single test
test_that("n_intervals is defined in compute_metrics_for_draw", {
  # Test code here
})
```

## 📝 Writing New Tests

### Test Template

```r
test_that("description of what is being tested", {
  # Arrange - Set up test data
  test_data <- create_test_data()
  
  # Act - Execute the function
  result <- function_under_test(test_data)
  
  # Assert - Check the results
  expect_equal(result$value, expected_value)
  expect_true(result$success)
  expect_no_error(validate_result(result))
})
```

### Best Practices

1. **One assertion per test** - Keep tests focused
2. **Use descriptive names** - Test names should explain what they verify
3. **Test edge cases** - NULL, NA, empty, and boundary values
4. **Mock external dependencies** - Use `mockery` for database/API calls
5. **Clean up** - Use `on.exit()` for cleanup code

## 🔍 Test Data

### Fixtures

Test fixtures are located in `tests/testthat/fixtures/`:
- `test_pk_data.rds` - Sample PK data
- `test_regimen.json` - Example dosing regimens
- `test_config.yml` - Test configuration

### Creating Test Data

```r
# Generate test PK data
create_test_pk_data <- function(n = 100) {
  data.frame(
    time = seq(0, 24, length.out = n),
    concentration = rlnorm(n, meanlog = 2, sdlog = 0.5),
    patient_id = rep(1:10, length.out = n)
  )
}

# Save as fixture
saveRDS(create_test_pk_data(), "tests/testthat/fixtures/test_pk_data.rds")
```

## 🚨 Common Issues

### Issue: Tests fail on Windows

**Solution:** Check file path separators
```r
# Use file.path() instead of paste()
test_file <- file.path("tests", "testthat", "test-fixes.R")
```

### Issue: Stan tests skip

**Solution:** Install cmdstanr
```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
```

### Issue: Coverage report fails

**Solution:** Ensure source files exist
```r
# Check that R files are in the correct location
list.files("R", pattern = "\\.R$", full.names = TRUE)
```

## 📞 Support

For test-related issues:
1. Check test output for specific error messages
2. Review the test documentation
3. Consult the CI/CD logs in GitHub Actions
4. Contact the development team

## 📄 License

Tests are part of the main application and covered under the same license.