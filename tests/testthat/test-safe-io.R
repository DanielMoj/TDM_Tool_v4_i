# tests/testthat/test-safe-io.R
# Test suite for safe I/O operations

library(testthat)
library(mockery)

# Source the module to test
source("R/safe_io.R")
source("R/auth_safe_upgrade.R")

# Helper function to create temp directory for tests
create_test_dir <- function() {
  temp_dir <- tempfile("test_safe_io_")
  dir.create(temp_dir, recursive = TRUE)
  return(temp_dir)
}

# Helper to create test users YAML
create_test_users <- function(path) {
  users <- list(
    users = list(
      list(username = "admin", role = "admin", password = "admin123"),
      list(username = "user1", role = "viewer", password = "user123"),
      list(username = "user2", role = "clinician", 
           password_hash = "$argon2id$v=19$m=65536,t=2,p=1$...")
    )
  )
  yaml::write_yaml(users, path)
}

test_that("safe_write_file creates automatic backups", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  test_file <- file.path(temp_dir, "test.txt")
  
  # Write initial content
  writeLines("version 1", test_file)
  
  # Configure safe I/O for this test
  old_config <- get_safe_io_config()
  configure_safe_io(backup_dir = "backups")
  on.exit(configure_safe_io(
    backup_dir = old_config$backup_dir,
    max_backups = old_config$max_backups
  ), add = TRUE)
  
  # Write new content with safe I/O
  result <- safe_write_file("version 2", test_file, create_backup = TRUE)
  expect_true(result)
  
  # Check backup was created
  backup_dir <- file.path(temp_dir, "backups")
  expect_true(dir.exists(backup_dir))
  
  backups <- list.files(backup_dir, pattern = "test\\.txt\\.backup_.*")
  expect_true(length(backups) > 0)
  
  # Verify backup content
  backup_content <- readLines(file.path(backup_dir, backups[1]))
  expect_equal(backup_content, "version 1")
  
  # Verify new content
  new_content <- readLines(test_file)
  expect_equal(new_content, "version 2")
})

test_that("safe_write_file validates content after writing", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  temp_file <- file.path(temp_dir, "test.yaml")
  
  test_data <- list(
    user1 = list(password = "hash123", role = "admin"),
    user2 = list(password = "hash456", role = "viewer")
  )
  
  # Write with validation
  result <- safe_write_yaml(test_data, temp_file)
  expect_true(result)
  
  # Read back and verify
  read_data <- yaml::read_yaml(temp_file)
  expect_identical(read_data, test_data)
})

test_that("safe_read_file tries fallbacks on failure", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  primary <- file.path(temp_dir, "primary.txt")
  fallback1 <- file.path(temp_dir, "fallback1.txt")
  fallback2 <- file.path(temp_dir, "fallback2.txt")
  
  # Only fallback2 exists
  writeLines("fallback content", fallback2)
  
  # Should read from fallback2
  content <- safe_read_file(
    primary,
    fallback_paths = c(fallback1, fallback2)
  )
  
  expect_equal(content, "fallback content")
})

test_that("atomic writes prevent corruption on failure", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  temp_file <- file.path(temp_dir, "test.txt")
  writeLines("original", temp_file)
  
  # Mock file.rename to simulate failure
  mock_rename <- mock(stop("Simulated failure"))
  
  with_mock(
    file.rename = mock_rename,
    {
      # Configure to use atomic writes
      configure_safe_io(use_atomic_writes = TRUE)
      
      expect_error(
        safe_write_file("corrupted", temp_file, 
                       writer_fn = writeLines,
                       create_backup = TRUE),
        "Simulated failure"
      )
    }
  )
  
  # Original should be intact or restored from backup
  expect_true(file.exists(temp_file))
  content <- readLines(temp_file)
  expect_equal(content, "original")
})

test_that("cleanup_old_backups removes excess backups", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Configure with low max_backups
  configure_safe_io(backup_dir = "backups", max_backups = 3)
  
  test_file <- file.path(temp_dir, "test.txt")
  
  # Create multiple versions
  for (i in 1:5) {
    writeLines(paste("version", i), test_file)
    Sys.sleep(0.1)  # Ensure different timestamps
    safe_write_file(paste("version", i + 1), test_file, create_backup = TRUE)
  }
  
  # Check that only max_backups remain
  backup_dir <- file.path(temp_dir, "backups")
  backups <- list.files(backup_dir, pattern = "test\\.txt\\.backup_.*")
  
  expect_lte(length(backups), 3)
})

test_that("safe_write_csv validates row count", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  test_file <- file.path(temp_dir, "test.csv")
  
  test_data <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )
  
  # Write CSV
  result <- safe_write_csv(test_data, test_file)
  expect_true(result)
  
  # Read back and verify
  read_data <- readr::read_csv(test_file, show_col_types = FALSE)
  expect_equal(nrow(read_data), nrow(test_data))
  expect_equal(read_data$id, test_data$id)
})

test_that("safe_write_rds handles binary data correctly", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  test_file <- file.path(temp_dir, "test.rds")
  
  # Complex test object
  test_data <- list(
    matrix = matrix(1:100, nrow = 10),
    df = data.frame(x = 1:10, y = letters[1:10]),
    nested = list(a = 1, b = list(c = 2, d = 3))
  )
  
  # Write RDS
  result <- safe_write_rds(test_data, test_file)
  expect_true(result)
  
  # Read back and verify
  read_data <- readRDS(test_file)
  expect_identical(read_data, test_data)
})

test_that("auth_upgrade_hashes safely upgrades passwords", {
  skip_if_not_installed("sodium")
  
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  users_file <- file.path(temp_dir, "users.yaml")
  
  # Create test users with plaintext passwords
  users <- list(
    users = list(
      list(username = "admin", role = "admin", password = "admin123"),
      list(username = "user1", role = "viewer", password = "user123")
    )
  )
  yaml::write_yaml(users, users_file)
  
  # Run upgrade
  result <- auth_upgrade_hashes(users_file, backup = TRUE)
  expect_true(result)
  
  # Check upgraded file
  upgraded_users <- yaml::read_yaml(users_file)
  
  # Passwords should be removed
  expect_null(upgraded_users$users[[1]]$password)
  expect_null(upgraded_users$users[[2]]$password)
  
  # Password hashes should exist
  expect_false(is.null(upgraded_users$users[[1]]$password_hash))
  expect_false(is.null(upgraded_users$users[[2]]$password_hash))
  
  # Hashes should be argon2 format
  expect_true(grepl("^\\$argon2", upgraded_users$users[[1]]$password_hash))
  
  # Metadata should be added
  expect_equal(upgraded_users$users[[1]]$hash_version, "2.0")
  expect_false(is.null(upgraded_users$users[[1]]$upgraded_at))
  
  # Backup should exist
  backup_dir <- file.path(temp_dir, "backups")
  expect_true(dir.exists(backup_dir))
  backups <- list.files(backup_dir, pattern = "users\\.yaml\\.backup_.*")
  expect_true(length(backups) > 0)
})

test_that("auth_upgrade_hashes handles already hashed passwords", {
  skip_if_not_installed("sodium")
  
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  users_file <- file.path(temp_dir, "users.yaml")
  
  # Create test users with already hashed password
  modern_hash <- as.character(sodium::password_store(charToRaw("test123")))
  
  users <- list(
    users = list(
      list(username = "admin", role = "admin", password_hash = modern_hash)
    )
  )
  yaml::write_yaml(users, users_file)
  
  # Run upgrade
  result <- auth_upgrade_hashes(users_file, backup = TRUE)
  expect_true(result)
  
  # Check that modern hash wasn't changed
  upgraded_users <- yaml::read_yaml(users_file)
  expect_equal(upgraded_users$users[[1]]$password_hash, modern_hash)
  
  # No upgrade metadata should be added for already modern hashes
  expect_null(upgraded_users$users[[1]]$hash_version)
})

test_that("restore_users_from_backup works correctly", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  users_file <- file.path(temp_dir, "users.yaml")
  
  # Create original file
  original_users <- list(
    users = list(
      list(username = "admin", role = "admin", password = "original")
    )
  )
  yaml::write_yaml(original_users, users_file)
  
  # Create backup
  safe_write_yaml(original_users, users_file, create_backup = TRUE)
  
  # Modify file
  modified_users <- list(
    users = list(
      list(username = "admin", role = "admin", password = "modified")
    )
  )
  yaml::write_yaml(modified_users, users_file)
  
  # Restore from backup
  result <- restore_users_from_backup(users_file, backup_index = 1)
  expect_true(result)
  
  # Check restored content
  restored <- yaml::read_yaml(users_file)
  expect_equal(restored$users[[1]]$password, "original")
})

test_that("batch_upgrade_user_files processes multiple files", {
  skip_if_not_installed("sodium")
  
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create multiple user files
  for (i in 1:3) {
    file_path <- file.path(temp_dir, sprintf("users_%d.yaml", i))
    users <- list(
      users = list(
        list(username = sprintf("user%d", i), 
             role = "viewer", 
             password = sprintf("pass%d", i))
      )
    )
    yaml::write_yaml(users, file_path)
  }
  
  # Run batch upgrade
  results <- batch_upgrade_user_files("users_*.yaml", temp_dir)
  
  expect_equal(length(results), 3)
  
  # Check all were successful
  for (r in results) {
    expect_true(r$success)
  }
  
  # Verify all files were upgraded
  for (i in 1:3) {
    file_path <- file.path(temp_dir, sprintf("users_%d.yaml", i))
    users <- yaml::read_yaml(file_path)
    expect_false(is.null(users$users[[1]]$password_hash))
    expect_null(users$users[[1]]$password)
  }
})

test_that("safe I/O handles permission errors gracefully", {
  skip_on_windows()  # File permissions work differently on Windows
  
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  test_file <- file.path(temp_dir, "readonly.txt")
  writeLines("content", test_file)
  
  # Make file read-only
  Sys.chmod(test_file, mode = "0444")
  
  # Try to write - should fail with clear error
  expect_error(
    safe_write_file("new content", test_file),
    "No write permission"
  )
  
  # Restore permissions for cleanup
  Sys.chmod(test_file, mode = "0644")
})

test_that("safe I/O recovers from corrupted YAML", {
  temp_dir <- create_test_dir()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  primary <- file.path(temp_dir, "corrupted.yaml")
  fallback <- file.path(temp_dir, "good.yaml")
  
  # Write corrupted YAML
  writeLines("{ invalid: yaml: content }", primary)
  
  # Write valid fallback
  yaml::write_yaml(list(valid = TRUE), fallback)
  
  # Should recover from fallback
  content <- safe_read_yaml(primary, fallback_paths = fallback)
  
  expect_false(is.null(content))
  expect_true(content$valid)
})