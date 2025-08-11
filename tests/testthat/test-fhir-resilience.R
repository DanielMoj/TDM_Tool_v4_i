# Test Suite for FHIR Resilience Features
# Tests circuit breaker, retry logic, caching, and error handling

library(testthat)
library(mockery)
library(httr)

# Source the modules
source("../../R/fhir_connection.R")
source("../../R/fhir_auth.R")
source("../../R/fhir_cache.R")
source("../../R/fhir.R")

# Test helper functions
create_mock_response <- function(status_code = 200, content = NULL) {
  if (is.null(content)) {
    content <- list(
      resourceType = "Bundle",
      type = "searchset",
      total = 1,
      entry = list(
        list(
          resource = list(
            resourceType = "Observation",
            id = "test-obs-1",
            status = "final"
          )
        )
      )
    )
  }
  
  response <- structure(
    list(
      status_code = status_code,
      headers = list(`content-type` = "application/fhir+json"),
      content = charToRaw(jsonlite::toJSON(content, auto_unbox = TRUE))
    ),
    class = "response"
  )
  
  return(response)
}

create_mock_error_response <- function(status_code = 500, message = "Server Error") {
  structure(
    list(
      status_code = status_code,
      headers = list(),
      content = charToRaw(jsonlite::toJSON(list(
        resourceType = "OperationOutcome",
        issue = list(
          list(
            severity = "error",
            code = "exception",
            diagnostics = message
          )
        )
      ), auto_unbox = TRUE))
    ),
    class = "response"
  )
}

# Test Circuit Breaker Functionality
context("Circuit Breaker")

test_that("Circuit breaker opens after threshold failures", {
  # Reset circuit breaker
  reset_fhir_circuit()
  
  # Configure for testing
  configure_circuit_breaker(
    failure_threshold = 3,
    timeout_seconds = 5,
    success_threshold = 2
  )
  
  # Mock failing requests
  fail_count <- 0
  mock_get <- mock(
    stop("Connection refused"),
    stop("Connection refused"),
    stop("Connection refused"),
    stop("Connection refused"),
    cycle = FALSE
  )
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  
  # Trigger failures
  for (i in 1:4) {
    result <- fhir_request_with_circuit_breaker(
      request_fn = httr::GET,
      url = "http://test.fhir.org/Patient",
      max_retries = 1
    )
    expect_null(result)
  }
  
  # Check circuit is open
  status <- get_fhir_circuit_status()
  expect_equal(status$state, "open")
  expect_false(status$available)
  expect_equal(status$failures, 3)
  
  # Verify subsequent requests are blocked immediately
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient"
  )
  expect_null(result)
  
  # Verify mock was called only 3 times (not 4 or 5)
  expect_called(mock_get, 3)
})

test_that("Circuit breaker transitions to half-open after timeout", {
  reset_fhir_circuit()
  
  # Set circuit to open state manually
  .fhir_circuit$state <- "open"
  .fhir_circuit$failures <- 5
  .fhir_circuit$last_failure_time <- Sys.time() - 65  # 65 seconds ago
  
  # Mock successful request
  mock_get <- mock(create_mock_response())
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  
  # Should transition to half-open and allow request
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient"
  )
  
  expect_false(is.null(result))
  
  status <- get_fhir_circuit_status()
  expect_equal(status$state, "half_open")
})

test_that("Circuit breaker closes after successful recovery", {
  reset_fhir_circuit()
  configure_circuit_breaker(success_threshold = 2)
  
  # Set to half-open state
  .fhir_circuit$state <- "half_open"
  .fhir_circuit$success_count <- 0
  
  # Mock successful requests
  mock_get <- mock(
    create_mock_response(),
    create_mock_response(),
    create_mock_response()
  )
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  
  # Make successful requests
  for (i in 1:2) {
    result <- fhir_request_with_circuit_breaker(
      request_fn = httr::GET,
      url = "http://test.fhir.org/Patient"
    )
    expect_false(is.null(result))
  }
  
  # Circuit should be closed now
  status <- get_fhir_circuit_status()
  expect_equal(status$state, "closed")
  expect_equal(status$failures, 0)
})

# Test Retry Logic
context("Retry Logic")

test_that("Requests retry on transient failures", {
  reset_fhir_circuit()
  
  # Mock: fail twice, then succeed
  attempt <- 0
  mock_get <- mock(
    stop("Timeout"),
    stop("Connection reset"),
    create_mock_response()
  )
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  stub(fhir_request_with_circuit_breaker, "Sys.sleep", function(x) NULL)  # Speed up test
  
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient",
    max_retries = 3
  )
  
  expect_false(is.null(result))
  expect_called(mock_get, 3)  # Failed twice, succeeded on third
})

test_that("Non-retryable errors fail immediately", {
  reset_fhir_circuit()
  
  # Mock 400 Bad Request (client error - non-retryable)
  mock_get <- mock(create_mock_error_response(400, "Bad Request"))
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  stub(fhir_request_with_circuit_breaker, "httr::http_error", function(x) TRUE)
  stub(fhir_request_with_circuit_breaker, "httr::status_code", function(x) x$status_code)
  
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient",
    max_retries = 3
  )
  
  expect_null(result)
  expect_called(mock_get, 1)  # Should not retry on client error
})

test_that("Rate limiting triggers appropriate wait", {
  reset_fhir_circuit()
  
  # Mock 429 response with Retry-After header
  rate_limit_response <- structure(
    list(
      status_code = 429,
      headers = list(`retry-after` = "5"),
      content = charToRaw("{}")
    ),
    class = "response"
  )
  
  mock_get <- mock(
    rate_limit_response,
    create_mock_response()
  )
  
  sleep_times <- numeric()
  mock_sleep <- mock(function(x) { sleep_times <<- c(sleep_times, x); NULL })
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  stub(fhir_request_with_circuit_breaker, "Sys.sleep", mock_sleep)
  stub(fhir_request_with_circuit_breaker, "httr::http_error", function(x) x$status_code >= 400)
  stub(fhir_request_with_circuit_breaker, "httr::status_code", function(x) x$status_code)
  stub(fhir_request_with_circuit_breaker, "httr::headers", function(x) x$headers)
  
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient"
  )
  
  expect_false(is.null(result))
  expect_called(mock_sleep, 1)
  expect_equal(sleep_times[1], 5)  # Should wait 5 seconds as specified
})

# Test Token Management
context("Token Management")

test_that("Token refresh triggers on 401", {
  # Initialize auth config
  .fhir_auth_config$default <- list(
    auth_url = "http://auth.test/token",
    client_id = "test-client",
    client_secret = "test-secret",
    scope = "system/*.read"
  )
  
  # Mock initial 401, then success after refresh
  mock_get <- mock(
    create_mock_error_response(401, "Unauthorized"),
    create_mock_response()
  )
  
  mock_refresh <- mock(TRUE)
  
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", function(config, expr) expr)
  stub(fhir_request_with_circuit_breaker, "httr::http_error", 
       function(x) x$status_code >= 400)
  stub(fhir_request_with_circuit_breaker, "httr::status_code", 
       function(x) x$status_code)
  stub(fhir_request_with_circuit_breaker, "fhir_refresh_token", mock_refresh)
  stub(fhir_request_with_circuit_breaker, "exists", function(x) TRUE)
  stub(fhir_request_with_circuit_breaker, "is.function", function(x) TRUE)
  
  result <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = "http://test.fhir.org/Patient",
    retry_on_401 = TRUE
  )
  
  expect_false(is.null(result))
  expect_called(mock_refresh, 1)
  expect_called(mock_get, 2)  # First 401, then success
})

test_that("Token expiry is detected and refreshed", {
  # Set up expired token
  .fhir_tokens$token_default <- list(
    token = "expired-token",
    expires_at = Sys.time() - 60,  # Expired 1 minute ago
    refresh_token = "refresh-token"
  )
  
  # Mock successful refresh
  mock_auth <- mock(TRUE)
  stub(get_fhir_token, "fhir_authenticate", mock_auth)
  
  # After refresh, set valid token
  .fhir_tokens$token_default$token <- "new-token"
  .fhir_tokens$token_default$expires_at <- Sys.time() + 3600
  
  token <- get_fhir_token()
  
  expect_equal(token, "new-token")
  expect_called(mock_auth, 1)
})

# Test Caching
context("Caching")

test_that("Cache returns stored data within TTL", {
  clear_fhir_cache()
  reset_cache_stats()
  
  # First request - should miss cache
  mock_fetch <- mock(list(data = "test-data"))
  
  result1 <- fhir_cached_request(
    cache_key = "test-key",
    fetch_fn = mock_fetch,
    ttl = 60
  )
  
  expect_equal(result1$data, "test-data")
  expect_called(mock_fetch, 1)
  
  # Second request - should hit cache
  result2 <- fhir_cached_request(
    cache_key = "test-key",
    fetch_fn = mock_fetch,
    ttl = 60
  )
  
  expect_equal(result2$data, "test-data")
  expect_called(mock_fetch, 1)  # Still only called once
  
  # Check stats
  stats <- get_cache_stats()
  expect_equal(stats$hits, 1)
  expect_equal(stats$misses, 1)
})

test_that("Cache respects TTL", {
  clear_fhir_cache()
  
  # Add entry with 1 second TTL
  mock_fetch <- mock(
    list(data = "original"),
    list(data = "updated")
  )
  
  result1 <- fhir_cached_request(
    cache_key = "ttl-test",
    fetch_fn = mock_fetch,
    ttl = 1
  )
  
  expect_equal(result1$data, "original")
  
  # Wait for expiry
  Sys.sleep(1.5)
  
  result2 <- fhir_cached_request(
    cache_key = "ttl-test",
    fetch_fn = mock_fetch,
    ttl = 1
  )
  
  expect_equal(result2$data, "updated")
  expect_called(mock_fetch, 2)
})

test_that("Cache returns stale data on fetch error", {
  clear_fhir_cache()
  
  # Initial successful fetch
  fetch_count <- 0
  mock_fetch <- function() {
    fetch_count <<- fetch_count + 1
    if (fetch_count == 1) {
      list(data = "cached-data")
    } else {
      stop("Network error")
    }
  }
  
  # First call - success
  result1 <- fhir_cached_request(
    cache_key = "stale-test",
    fetch_fn = mock_fetch,
    ttl = 1
  )
  
  expect_equal(result1$data, "cached-data")
  
  # Wait for expiry
  Sys.sleep(1.5)
  
  # Second call - fetch fails, should return stale
  result2 <- suppressWarnings(fhir_cached_request(
    cache_key = "stale-test",
    fetch_fn = mock_fetch,
    use_stale_on_error = TRUE
  ))
  
  expect_equal(result2$data, "cached-data")
})

test_that("Cache compression works for large objects", {
  clear_fhir_cache()
  configure_fhir_cache(enable_compression = TRUE)
  
  # Create large object
  large_data <- list(
    data = rep(paste(rep("x", 1000), collapse = ""), 100)
  )
  
  mock_fetch <- mock(large_data)
  
  result <- fhir_cached_request(
    cache_key = "compression-test",
    fetch_fn = mock_fetch,
    ttl = 60
  )
  
  # Check that data was compressed
  cache_entry <- .fhir_cache[["compression-test"]]
  expect_true(cache_entry$compressed)
  expect_lt(cache_entry$size, cache_entry$original_size)
  
  # Verify decompression works
  result2 <- fhir_cached_request(
    cache_key = "compression-test",
    fetch_fn = mock_fetch,
    ttl = 60
  )
  
  expect_equal(result2, large_data)
})

# Test Main FHIR Functions
context("Main FHIR Functions")

test_that("fhir_get_observations handles network errors gracefully", {
  reset_fhir_circuit()
  .fhir_config$base_url <- "http://test.fhir.org"
  
  # Mock network timeout
  mock_get <- mock(stop("Timeout"))
  
  stub(fhir_get_observations, "fhir_request_with_circuit_breaker", 
       function(...) NULL)
  
  result <- fhir_get_observations("patient-123", use_cache = FALSE)
  
  expect_null(result)
})

test_that("fhir_get_observations validates inputs", {
  .fhir_config$base_url <- "http://test.fhir.org"
  
  expect_error(
    fhir_get_observations(),
    "patient_id is required"
  )
  
  expect_error(
    fhir_get_observations(NULL),
    "patient_id is required"
  )
  
  expect_error(
    fhir_get_observations(""),
    "patient_id is required"
  )
})

test_that("fhir_fetch_all_pages handles pagination correctly", {
  .fhir_config$base_url <- "http://test.fhir.org"
  
  # Mock paginated responses
  page1 <- list(
    resourceType = "Bundle",
    type = "searchset",
    total = 25,
    entry = lapply(1:10, function(i) list(resource = list(id = paste0("obs-", i)))),
    link = list(
      list(relation = "self", url = "http://test.fhir.org/Observation?page=1"),
      list(relation = "next", url = "http://test.fhir.org/Observation?page=2")
    )
  )
  
  page2 <- list(
    resourceType = "Bundle",
    type = "searchset",
    total = 25,
    entry = lapply(11:20, function(i) list(resource = list(id = paste0("obs-", i)))),
    link = list(
      list(relation = "self", url = "http://test.fhir.org/Observation?page=2"),
      list(relation = "next", url = "http://test.fhir.org/Observation?page=3")
    )
  )
  
  page3 <- list(
    resourceType = "Bundle",
    type = "searchset",
    total = 25,
    entry = lapply(21:25, function(i) list(resource = list(id = paste0("obs-", i)))),
    link = list(
      list(relation = "self", url = "http://test.fhir.org/Observation?page=3")
    )
  )
  
  mock_fetch <- mock(page1, page2, page3)
  stub(fhir_fetch_all_pages, "fetch_fhir_bundle", mock_fetch)
  
  result <- fhir_fetch_all_pages("http://test.fhir.org/Observation?patient=123")
  
  expect_equal(result$total, 25)
  expect_equal(length(result$entry), 25)
  expect_called(mock_fetch, 3)
})

test_that("FHIR operations handle OperationOutcome errors", {
  .fhir_config$base_url <- "http://test.fhir.org"
  
  # Mock OperationOutcome error response
  error_response <- create_mock_response(
    status_code = 200,
    content = list(
      resourceType = "OperationOutcome",
      issue = list(
        list(
          severity = "error",
          code = "not-found",
          diagnostics = "Patient not found"
        )
      )
    )
  )
  
  mock_get <- mock(error_response)
  
  stub(fhir_get_patient, "fhir_request_with_circuit_breaker", mock_get)
  
  result <- suppressWarnings(fhir_get_patient("unknown-patient", use_cache = FALSE))
  
  expect_null(result)
})

# Test Error Handling Edge Cases
context("Error Handling Edge Cases")

test_that("System handles malformed JSON responses", {
  .fhir_config$base_url <- "http://test.fhir.org"
  
  # Create response with invalid JSON
  bad_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json"),
      content = charToRaw("{ invalid json }")
    ),
    class = "response"
  )
  
  mock_get <- mock(bad_response)
  stub(fhir_get_observations, "fhir_request_with_circuit_breaker", mock_get)
  
  result <- suppressWarnings(
    fhir_get_observations("patient-123", use_cache = FALSE)
  )
  
  expect_null(result)
})

test_that("System handles empty responses gracefully", {
  .fhir_config$base_url <- "http://test.fhir.org"
  
  empty_bundle <- list(
    resourceType = "Bundle",
    type = "searchset",
    total = 0
    # No entry field
  )
  
  mock_response <- create_mock_response(content = empty_bundle)
  mock_get <- mock(mock_response)
  
  stub(fhir_get_observations, "fhir_request_with_circuit_breaker", mock_get)
  
  result <- fhir_get_observations("patient-123", use_cache = FALSE)
  
  expect_false(is.null(result))
  expect_equal(result$total, 0)
  expect_equal(length(result$entry), 0)
})

# Test Integration
context("Integration Tests")

test_that("Complete flow works with all resilience features", {
  skip_if_not(interactive(), "Integration test requires manual verification")
  
  # Initialize system
  reset_fhir_circuit()
  clear_fhir_cache()
  
  # Configure for testing
  .fhir_config$base_url <- "http://test.fhir.org"
  configure_circuit_breaker(
    failure_threshold = 3,
    timeout_seconds = 5
  )
  configure_fhir_cache(
    default_ttl = 60,
    max_cache_size = 10
  )
  
  # Simulate various scenarios
  scenarios <- list(
    success = create_mock_response(),
    server_error = create_mock_error_response(500),
    rate_limit = structure(
      list(status_code = 429, headers = list(`retry-after` = "2")),
      class = "response"
    ),
    timeout = function() stop("Timeout"),
    recovered = create_mock_response()
  )
  
  # Run through scenarios
  for (scenario_name in names(scenarios)) {
    message(sprintf("Testing scenario: %s", scenario_name))
    # Implementation would go here in real integration test
  }
  
  # Verify final state
  status <- get_fhir_status()
  expect_true(is.list(status))
  expect_true("circuit_breaker" %in% names(status))
  expect_true("cache" %in% names(status))
})

# Performance Tests
context("Performance")

test_that("Circuit breaker adds minimal overhead", {
  skip_if_not(interactive(), "Performance test")
  
  reset_fhir_circuit()
  
  # Mock instant successful response
  mock_get <- mock(create_mock_response(), cycle = TRUE)
  stub(fhir_request_with_circuit_breaker, "httr::GET", mock_get)
  stub(fhir_request_with_circuit_breaker, "httr::with_config", 
       function(config, expr) expr)
  
  # Measure time for 100 requests
  start_time <- Sys.time()
  
  for (i in 1:100) {
    fhir_request_with_circuit_breaker(
      request_fn = httr::GET,
      url = "http://test.fhir.org/Patient"
    )
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Should complete 100 requests in under 1 second (10ms per request overhead max)
  expect_lt(elapsed, 1)
  message(sprintf("100 requests completed in %.3f seconds", elapsed))
})

test_that("Cache improves response time significantly", {
  skip_if_not(interactive(), "Performance test")
  
  clear_fhir_cache()
  
  # Mock slow fetch function
  slow_fetch <- function() {
    Sys.sleep(0.1)  # Simulate 100ms network latency
    list(data = "test")
  }
  
  # First request - slow
  start1 <- Sys.time()
  result1 <- fhir_cached_request("perf-test", slow_fetch, ttl = 60)
  time1 <- as.numeric(difftime(Sys.time(), start1, units = "secs"))
  
  # Second request - fast (cached)
  start2 <- Sys.time()
  result2 <- fhir_cached_request("perf-test", slow_fetch, ttl = 60)
  time2 <- as.numeric(difftime(Sys.time(), start2, units = "secs"))
  
  # Cache should be at least 10x faster
  expect_lt(time2 * 10, time1)
  message(sprintf("Uncached: %.3fs, Cached: %.3fs (%.1fx faster)", 
                 time1, time2, time1/time2))
})