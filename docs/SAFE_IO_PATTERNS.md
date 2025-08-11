# Safe I/O Patterns Documentation

## Overview

The Safe I/O module (`R/safe_io.R`) provides robust file operations that prevent data loss through atomic writes, automatic backups, and comprehensive error handling. This documentation describes the patterns and best practices for using these functions throughout the TDMx system.

## Core Principles

### 1. Atomic Writes
All write operations use a temp-file + rename strategy to ensure atomicity:
- Write to temporary file first
- Validate the written content
- Atomically rename to target location
- No partial writes or corrupted files

### 2. Automatic Backups
Before modifying any file:
- Create timestamped backup
- Store in dedicated backup directory
- Maintain configurable number of backups
- Automatic cleanup of old backups

### 3. Fallback Recovery
When reading files:
- Try primary location first
- Fall back to alternative locations
- Attempt recovery from backups
- Return NULL gracefully on total failure

### 4. Validation
After writing:
- Verify file can be read back
- Validate structure/content
- Rollback on validation failure

## Configuration

### Global Settings

```r
# Configure safe I/O behavior
configure_safe_io(
  backup_dir = "backups",        # Where to store backups
  max_backups = 10,              # Maximum backups per file
  validate_after_write = TRUE,   # Verify after writing
  use_atomic_writes = TRUE       # Use atomic operations
)

# Get current configuration
config <- get_safe_io_config()
```

### Environment-Specific Settings

```r
# Development: More backups, always validate
if (Sys.getenv("R_ENV") == "development") {
  configure_safe_io(
    max_backups = 50,
    validate_after_write = TRUE
  )
}

# Production: Fewer backups, atomic writes
if (Sys.getenv("R_ENV") == "production") {
  configure_safe_io(
    max_backups = 10,
    use_atomic_writes = TRUE
  )
}
```

## Usage Patterns

### Pattern 1: Simple File Operations

```r
# Read with automatic fallback
content <- safe_read_file(
  "config/settings.txt",
  fallback_paths = c(
    "config/settings.txt.default",
    "config/settings.txt.example"
  )
)

# Write with automatic backup
safe_write_file(
  content = "new settings",
  path = "config/settings.txt",
  create_backup = TRUE
)
```

### Pattern 2: YAML Configuration Files

```r
# Read configuration with fallbacks
config <- safe_read_yaml(
  "config/app.yaml",
  fallback_paths = c(
    "config/app.yaml.default",
    "/etc/tdmx/app.yaml"
  )
)

# Update configuration safely
config$updated_at <- Sys.time()
safe_write_yaml(
  data = config,
  path = "config/app.yaml",
  create_backup = TRUE
)
```

### Pattern 3: CSV Data Files

```r
# Read CSV with recovery
data <- safe_read_csv(
  "data/measurements.csv",
  fallback_paths = "data/measurements.csv.backup"
)

# Append to CSV safely
new_data <- data.frame(
  timestamp = Sys.time(),
  value = 42
)

safe_write_csv(
  data = new_data,
  path = "data/measurements.csv",
  append = TRUE,
  create_backup = TRUE
)
```

### Pattern 4: Binary Data (RDS)

```r
# Save model safely
safe_write_rds(
  data = model_object,
  path = "models/fitted_model.rds",
  compress = TRUE,
  create_backup = TRUE
)

# Load with fallback to previous version
model <- safe_read_rds(
  "models/fitted_model.rds",
  fallback_paths = "models/fitted_model_previous.rds"
)
```

### Pattern 5: Custom Validation

```r
# Write with custom validation
safe_write_file(
  content = user_data,
  path = "config/users.yaml",
  writer_fn = yaml::write_yaml,
  validate_fn = function(temp_path) {
    # Custom validation logic
    data <- yaml::read_yaml(temp_path)
    
    # Check structure
    if (!all(c("users", "roles") %in% names(data))) {
      return(list(valid = FALSE, error = "Missing required sections"))
    }
    
    # Check each user has required fields
    for (user in data$users) {
      if (!all(c("username", "role") %in% names(user))) {
        return(list(valid = FALSE, error = "Invalid user structure"))
      }
    }
    
    return(list(valid = TRUE))
  }
)
```

## Error Handling

### Handling Read Failures

```r
# Always check for NULL returns
data <- safe_read_yaml("config/settings.yaml")

if (is.null(data)) {
  # Use defaults or handle gracefully
  warning("Could not load settings, using defaults")
  data <- list(
    timeout = 30,
    retries = 3
  )
}
```

### Handling Write Failures

```r
# Check write success
success <- safe_write_yaml(config, "config/app.yaml")

if (!success) {
  # Log error and notify
  log_error("Failed to save configuration")
  notify_admin("Configuration save failed")
  
  # Potentially use alternative storage
  saveRDS(config, "config/app_emergency.rds")
}
```

### Recovery from Backups

```r
# Restore from backup if current file is corrupted
if (file_corrupted("config/users.yaml")) {
  restore_users_from_backup(
    users_file = "config/users.yaml",
    backup_index = 1  # Most recent backup
  )
}

# List available backups
list_backups <- function(file_path) {
  backup_dir <- file.path(dirname(file_path), "backups")
  pattern <- sprintf("%s\\.backup_.*", basename(file_path))
  backups <- list.files(backup_dir, pattern = pattern)
  return(backups)
}
```

## Migration Guide

### Migrating Existing Code

Replace unsafe I/O operations with safe equivalents:

#### Before (Unsafe):
```r
# Direct write - can corrupt on failure
users <- yaml::read_yaml("config/users.yaml")
users$updated <- Sys.time()
yaml::write_yaml(users, "config/users.yaml")
```

#### After (Safe):
```r
# Safe write with backup and validation
users <- safe_read_yaml("config/users.yaml")
if (!is.null(users)) {
  users$updated <- Sys.time()
  safe_write_yaml(users, "config/users.yaml")
}
```

### Common Replacements

| Unsafe Operation | Safe Replacement |
|-----------------|------------------|
| `readLines()` | `safe_read_file()` |
| `writeLines()` | `safe_write_file()` |
| `yaml::read_yaml()` | `safe_read_yaml()` |
| `yaml::write_yaml()` | `safe_write_yaml()` |
| `read.csv()` / `readr::read_csv()` | `safe_read_csv()` |
| `write.csv()` / `readr::write_csv()` | `safe_write_csv()` |
| `readRDS()` | `safe_read_rds()` |
| `saveRDS()` | `safe_write_rds()` |

## Best Practices

### 1. Always Use Safe I/O for Critical Files
- User configurations
- Authentication data
- Audit logs
- Database configurations
- API keys and secrets

### 2. Configure Appropriate Backup Retention
```r
# Critical files: Keep many backups
configure_safe_io(max_backups = 50)
safe_write_yaml(critical_config, "config/critical.yaml")

# Temporary files: Keep few backups
configure_safe_io(max_backups = 3)
safe_write_file(temp_data, "temp/data.txt")
```

### 3. Implement Validation for Complex Structures
```r
validate_database_config <- function(path) {
  config <- yaml::read_yaml(path)
  
  # Check required fields
  required <- c("host", "port", "database", "user")
  if (!all(required %in% names(config))) {
    return(list(valid = FALSE, error = "Missing database fields"))
  }
  
  # Validate port number
  if (!is.numeric(config$port) || config$port < 1 || config$port > 65535) {
    return(list(valid = FALSE, error = "Invalid port number"))
  }
  
  return(list(valid = TRUE))
}

safe_write_yaml(
  db_config, 
  "config/database.yaml",
  validate_fn = validate_database_config
)
```

### 4. Use Fallback Chains for Resilience
```r
# Priority order: user -> system -> default
config <- safe_read_yaml(
  "~/.tdmx/config.yaml",
  fallback_paths = c(
    "/etc/tdmx/config.yaml",
    "/usr/share/tdmx/config.default.yaml",
    system.file("config", "default.yaml", package = "tdmx")
  )
)
```

### 5. Monitor Backup Storage
```r
# Check backup directory size
check_backup_usage <- function(base_dir = ".") {
  backup_dirs <- list.dirs(base_dir, pattern = "backups$", 
                          recursive = TRUE, full.names = TRUE)
  
  for (dir in backup_dirs) {
    size_mb <- sum(file.info(list.files(dir, full.names = TRUE))$size) / 1024^2
    if (size_mb > 100) {
      warning(sprintf("Backup directory %s is large: %.1f MB", dir, size_mb))
    }
  }
}

# Run periodically
check_backup_usage()
```

## Testing

### Unit Testing Safe I/O

```r
test_that("critical file operations are safe", {
  # Test atomic writes
  test_file <- tempfile()
  writeLines("original", test_file)
  
  # Simulate failure during write
  with_mock(
    file.rename = function(...) stop("Simulated failure"),
    {
      expect_error(
        safe_write_file("new", test_file, create_backup = TRUE)
      )
    }
  )
  
  # Original should be intact
  expect_equal(readLines(test_file), "original")
})
```

### Integration Testing

```r
# Test full workflow
test_that("auth upgrade workflow is safe", {
  users_file <- tempfile(fileext = ".yaml")
  
  # Create test users
  users <- list(
    users = list(
      list(username = "test", password = "plaintext")
    )
  )
  yaml::write_yaml(users, users_file)
  
  # Run upgrade
  result <- auth_upgrade_hashes(users_file)
  expect_true(result)
  
  # Verify upgrade
  upgraded <- yaml::read_yaml(users_file)
  expect_null(upgraded$users[[1]]$password)
  expect_false(is.null(upgraded$users[[1]]$password_hash))
  
  # Verify backup exists
  backups <- list.files(
    file.path(dirname(users_file), "backups"),
    pattern = basename(users_file)
  )
  expect_true(length(backups) > 0)
})
```

## Troubleshooting

### Common Issues and Solutions

#### Issue: "No write permission" error
**Solution:** Check file and directory permissions
```r
# Check permissions
file.access("config/file.yaml", mode = 2)  # 0 = success, -1 = no permission

# Fix permissions (Unix/Linux)
system("chmod 664 config/file.yaml")
system("chmod 775 config/")
```

#### Issue: Backup directory grows too large
**Solution:** Reduce max_backups or implement rotation
```r
# Reduce retention
configure_safe_io(max_backups = 5)

# Manual cleanup of old backups
cleanup_old_backups_manual <- function(dir, days_old = 30) {
  cutoff <- Sys.time() - (days_old * 24 * 60 * 60)
  files <- list.files(dir, full.names = TRUE)
  
  for (file in files) {
    if (file.info(file)$mtime < cutoff) {
      file.remove(file)
      message(sprintf("Removed old backup: %s", file))
    }
  }
}
```

#### Issue: Atomic writes fail on network drives
**Solution:** Disable atomic writes for network paths
```r
# Detect network path and adjust
is_network_path <- function(path) {
  startsWith(path, "//") || startsWith(path, "\\\\") ||
  grepl("^[a-zA-Z]:[\\\\/]", path)  # Windows network drive
}

if (is_network_path(file_path)) {
  configure_safe_io(use_atomic_writes = FALSE)
}
```

## Performance Considerations

### 1. Backup Creation Overhead
- Each write creates a backup copy
- Consider disabling for high-frequency writes
- Use batch operations where possible

### 2. Validation Cost
- Validation requires reading file back
- Can be disabled for trusted operations
- Consider sampling for large files

### 3. Optimization Strategies

```r
# Batch writes to reduce overhead
batch_write_configs <- function(configs) {
  # Disable per-write validation
  configure_safe_io(validate_after_write = FALSE)
  
  results <- list()
  for (name in names(configs)) {
    path <- sprintf("config/%s.yaml", name)
    results[[name]] <- safe_write_yaml(configs[[name]], path)
  }
  
  # Re-enable validation
  configure_safe_io(validate_after_write = TRUE)
  
  return(results)
}

# Use memory cache for frequently read files
cached_read_yaml <- memoise::memoise(
  safe_read_yaml,
  cache = cachem::cache_mem(max_size = 100 * 1024^2)  # 100 MB cache
)
```

## Summary

The Safe I/O module provides essential protection against data loss in critical file operations. By following these patterns and best practices, the TDMx system can maintain data integrity even in the face of system failures, permission issues, or corrupted files. Always prefer safe I/O operations for any file that would cause system issues if lost or corrupted.