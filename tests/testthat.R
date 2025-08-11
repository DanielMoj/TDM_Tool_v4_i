# tests/testthat.R
# Configuration file for testthat test suite

library(testthat)
library(PKPDApp)  # Replace with actual package name

# Set test options
options(
  # Testthat options
  testthat.use_colours = TRUE,
  testthat.progress.max_fails = 10,
  
  # Warning handling
  warn = 1,  # Print warnings as they occur
  
  # Parallel processing for tests
  testthat.parallel = TRUE,
  testthat.parallel.threads = 2,
  
  # Output options
  width = 80,
  scipen = 999,  # Avoid scientific notation
  
  # Timezone for consistent testing
  tz = "UTC"
)

# Set random seed for reproducibility
set.seed(42)

# Configure test environment
Sys.setenv(
  TESTTHAT = "true",
  R_TESTS = "",
  TESTING_ENV = "local",
  
  # Database settings for testing
  TEST_DB_PATH = ":memory:",
  
  # Disable external API calls during testing
  DISABLE_EXTERNAL_API = "true",
  
  # Set test timeout
  TEST_TIMEOUT = "60"
)

# Load helper functions
if (file.exists("tests/testthat/helper-functions.R")) {
  source("tests/testthat/helper-functions.R")
}

# Load test fixtures
if (file.exists("tests/testthat/fixtures/test-data.R")) {
  source("tests/testthat/fixtures/test-data.R")
}

# Custom test reporter for CI/CD
if (Sys.getenv("CI") == "true") {
  reporter <- MultiReporter$new(
    reporters = list(
      ProgressReporter$new(),
      JunitReporter$new(file = "test-results.xml"),
      CheckReporter$new()
    )
  )
} else {
  reporter <- "progress"
}

# Run tests
test_check("PKPDApp", reporter = reporter)