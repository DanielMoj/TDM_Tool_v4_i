# tests/testthat/test-utils.R
# Unit tests for utility functions, especially the %||% operator

library(testthat)

# Source the utils file
source(file.path("R", "utils.R"))

context("Null coalescing operator tests")

test_that("%||% operator works correctly with NULL values", {
  # Basic NULL handling
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% 10, 10)
  expect_equal(NULL %||% FALSE, FALSE)
  expect_equal(NULL %||% list(a = 1), list(a = 1))
  expect_equal(NULL %||% c(1, 2, 3), c(1, 2, 3))
})

test_that("%||% operator returns non-NULL values", {
  # Should return the first value if it's not NULL
  expect_equal("value" %||% "default", "value")
  expect_equal(42 %||% 0, 42)
  expect_equal(TRUE %||% FALSE, TRUE)
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(0 %||% 10, 0)
  expect_equal("" %||% "default", "")
  expect_equal(list(a = 1) %||% list(b = 2), list(a = 1))
  expect_equal(c(1, 2, 3) %||% c(4, 5, 6), c(1, 2, 3))
})

test_that("%||% operator handles NA values correctly", {
  # NA is not NULL, so should return NA
  expect_equal(NA %||% "default", NA)
  expect_equal(NA_character_ %||% "default", NA_character_)
  expect_equal(NA_real_ %||% 5.5, NA_real_)
  expect_equal(NA_integer_ %||% 10L, NA_integer_)
  expect_equal(NA_complex_ %||% 1+2i, NA_complex_)
})

test_that("%||% operator handles empty vectors correctly", {
  # Empty vectors are not NULL
  expect_equal(numeric(0) %||% c(1, 2, 3), numeric(0))
  expect_equal(character(0) %||% "default", character(0))
  expect_equal(logical(0) %||% TRUE, logical(0))
  expect_equal(list() %||% list(a = 1), list())
})

test_that("%||% operator works with complex data types", {
  # Data frames
  df1 <- data.frame(x = 1:3, y = 4:6)
  df2 <- data.frame(a = 7:9, b = 10:12)
  expect_equal(df1 %||% df2, df1)
  expect_equal(NULL %||% df2, df2)
  
  # Matrices
  m1 <- matrix(1:9, nrow = 3)
  m2 <- matrix(10:18, nrow = 3)
  expect_equal(m1 %||% m2, m1)
  expect_equal(NULL %||% m2, m2)
  
  # Functions
  f1 <- function() "first"
  f2 <- function() "second"
  expect_equal(f1 %||% f2, f1)
  expect_equal(NULL %||% f2, f2)
})

test_that("%||% operator can be chained", {
  # Multiple operations
  expect_equal(NULL %||% NULL %||% "final", "final")
  expect_equal("first" %||% "second" %||% "third", "first")
  expect_equal(NULL %||% NULL %||% NULL %||% 42, 42)
  
  # With variables
  a <- NULL
  b <- NULL
  c <- "value"
  expect_equal(a %||% b %||% c, "value")
})

test_that("%||% operator preserves attributes", {
  # Named vectors
  v1 <- c(a = 1, b = 2, c = 3)
  v2 <- c(x = 4, y = 5, z = 6)
  result <- v1 %||% v2
  expect_equal(result, v1)
  expect_equal(names(result), names(v1))
  
  # Data frames with custom attributes
  df <- data.frame(x = 1:3)
  attr(df, "custom") <- "attribute"
  result <- df %||% data.frame(y = 4:6)
  expect_equal(attr(result, "custom"), "attribute")
})

test_that("%||% operator handles expressions correctly", {
  # Function calls as arguments
  get_null <- function() NULL
  get_value <- function() "value"
  
  expect_equal(get_null() %||% "default", "default")
  expect_equal(get_value() %||% "default", "value")
  
  # Expressions
  expect_equal((1 + 1) %||% 0, 2)
  expect_equal((NULL) %||% (2 + 2), 4)
})

test_that("%||% operator performance with large objects", {
  # Large objects should be handled efficiently
  large_list <- as.list(1:10000)
  result <- large_list %||% list()
  expect_equal(length(result), 10000)
  
  # NULL should return quickly
  result <- NULL %||% large_list
  expect_equal(length(result), 10000)
})

# Test other utility functions if they exist
context("Other utility functions")

test_that("parse_num_list handles various inputs", {
  expect_equal(parse_num_list("1,2,3"), c(1, 2, 3))
  expect_equal(parse_num_list("1, 2, 3"), c(1, 2, 3))
  expect_equal(parse_num_list(""), numeric(0))
  expect_equal(parse_num_list(NULL), numeric(0))
  expect_equal(parse_num_list(NA), numeric(0))
  
  # With warnings for invalid values
  expect_warning(result <- parse_num_list("1,a,3"), "Could not parse")
  expect_equal(result, c(1, 3))
})

test_that("is_empty correctly identifies empty values", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(""))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(numeric(0)))
  expect_true(is_empty(c(NA, NA)))
  expect_true(is_empty(c("", "", "")))
  
  expect_false(is_empty(0))
  expect_false(is_empty(FALSE))
  expect_false(is_empty("text"))
  expect_false(is_empty(c(1, 2, 3)))
  expect_false(is_empty(list(a = 1)))
})

test_that("coalesce returns first non-NULL value", {
  expect_equal(coalesce(NULL, NULL, "value", "other"), "value")
  expect_equal(coalesce("first", "second"), "first")
  expect_equal(coalesce(NULL, NULL, NULL), NULL)
  expect_equal(coalesce(), NULL)
})

test_that("safe_extract handles missing keys gracefully", {
  lst <- list(a = 1, b = 2, c = 3)
  
  expect_equal(safe_extract(lst, "a"), 1)
  expect_equal(safe_extract(lst, "b"), 2)
  expect_equal(safe_extract(lst, "d"), NULL)
  expect_equal(safe_extract(lst, "d", default = 0), 0)
  expect_equal(safe_extract(NULL, "a"), NULL)
  expect_equal(safe_extract(NULL, "a", default = 10), 10)
})

test_that("safe_divide handles division by zero", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0), NA_real_)
  expect_equal(safe_divide(0, 0), NA_real_)
  expect_equal(safe_divide(10, 0, na_value = 0), 0)
  expect_equal(safe_divide(NULL, 5), NA_real_)
  expect_equal(safe_divide(5, NULL), NA_real_)
})

test_that("clamp restricts values to range", {
  expect_equal(clamp(5, 0, 10), 5)
  expect_equal(clamp(-5, 0, 10), 0)
  expect_equal(clamp(15, 0, 10), 10)
  expect_equal(clamp(c(-5, 5, 15), 0, 10), c(0, 5, 10))
})

test_that("generate_id creates unique identifiers", {
  id1 <- generate_id()
  id2 <- generate_id()
  
  expect_true(nchar(id1) > 10)
  expect_true(grepl("^id_", id1))
  expect_true(id1 != id2)
  
  custom_id <- generate_id(prefix = "test", length = 4)
  expect_true(grepl("^test_", custom_id))
})

test_that("is_valid_email validates email addresses", {
  expect_true(is_valid_email("user@example.com"))
  expect_true(is_valid_email("user.name@example.co.uk"))
  expect_true(is_valid_email("user+tag@example.com"))
  
  expect_false(is_valid_email("notanemail"))
  expect_false(is_valid_email("@example.com"))
  expect_false(is_valid_email("user@"))
  expect_false(is_valid_email(NULL))
  expect_false(is_valid_email(NA))
  expect_false(is_valid_email(123))
})