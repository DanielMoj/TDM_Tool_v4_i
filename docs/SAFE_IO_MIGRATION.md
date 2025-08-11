# Safe I/O Migration Guide

## Overview

This guide helps migrate existing file I/O operations in the TDMx system to use the new Safe I/O module. The migration ensures data integrity through atomic writes, automatic backups, and proper error handling.

## Pre-Migration Checklist

- [ ] Backup all configuration files
- [ ] Document current file locations
- [ ] Test in development environment first
- [ ] Plan maintenance window for production
- [ ] Prepare rollback procedure

## Step-by-Step Migration

### Step 1: Install Safe I/O Module

```r
# Copy safe_io.R to your R directory
file.copy("R/safe_io.R", "R/", overwrite = FALSE)

# Source the module
source("R/safe_io.R")

# Verify installation
stopifnot(exists("safe_read_file"))
stopifnot(exists("safe_write_file"))
```

### Step 2: Identify Files to Migrate

Critical files that MUST use safe I/O:
- `config/users.yaml` - User authentication
- `config/database.yaml` - Database configuration
- `config/api_keys.yaml` - API credentials
- `audit/*.csv` - Audit logs
- `priors/*.json` - Prior distributions
- `cache/*.rds` - Cached model results

### Step 3: Update Authentication Module

#### Original Code (R/auth.R):
```r
# UNSAFE - Direct file operations
auth_upgrade_hashes <- function() {
  users <- yaml::read_yaml("config/users.yaml")  # Can fail!
  # ... upgrade logic ...
  yaml::write_yaml(users, "config/users.yaml")   # Can corrupt!
}
```

#### Migrated Code:
```r
# SAFE - Using safe I/O
source("R/safe_io.R")

auth_upgrade_hashes <- function() {
  users <- safe_read_yaml(
    "config/users.yaml",
    fallback_paths = c(
      "config/users.yaml.backup",
      "config/users.yaml.default"
    )
  )
  
  if (is.null(users)) {
    stop("Cannot read users configuration")
  }
  
  # ... upgrade logic ...
  
  safe_write_yaml(users, "config/users.yaml", create_backup = TRUE)
}
```

### Step 4: Update Database Module

#### Original Code (R/db.R):
```r
# UNSAFE
load_db_config <- function() {
  config <- yaml::read_yaml("config/database.yaml")
  return(config)
}

save_db_config <- function(config) {
  yaml::write_yaml(config, "config/database.yaml")
}
```

#### Migrated Code:
```r
# SAFE
load_db_config <- function() {
  config <- safe_read_yaml(
    "config/database.yaml",
    fallback_paths = c(
      Sys.getenv("TDMX_DB_CONFIG", ""),
      "config/database.yaml.default"
    )
  )
  
  if (is.null(config)) {
    warning("Using default database configuration")
    config <- list(
      host = "localhost",
      port = 5432,
      database = "tdmx",
      user = "tdmx_user"
    )
  }
  
  return(config)
}

save_db_config <- function(config) {
  # Validate before saving
  validate_fn <- function(path) {
    test_config <- yaml::read_yaml(path)
    required <- c("host", "port", "database", "user")
    
    if (!all(required %in% names(test_config))) {
      return(list(valid = FALSE, error = "Missing required fields"))
    }
    
    return(list(valid = TRUE))
  }
  
  safe_write_yaml(
    config, 
    "config/database.yaml",
    create_backup = TRUE,
    validate_fn = validate_fn
  )
}
```

### Step 5: Update Audit Module

#### Original Code (R/audit.R):
```r
# UNSAFE - No atomic writes
audit_append <- function(entry) {
  log_file <- "audit/audit_log.csv"
  write.table(
    entry, 
    log_file, 
    append = TRUE,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(log_file)
  )
}
```

#### Migrated Code:
```r
# SAFE - Atomic append with backup
audit_append <- function(entry) {
  log_file <- "audit/audit_log.csv"
  
  # Read existing log
  existing <- safe_read_csv(log_file)
  
  if (is.null(existing)) {
    # First entry
    new_log <- entry
  } else {
    # Append entry
    new_log <- dplyr::bind_rows(existing, entry)
  }
  
  # Write atomically
  safe_write_csv(
    new_log,
    log_file,
    create_backup = TRUE
  )
}
```

### Step 6: Update Cache Module

#### Original Code (R/cache.R):
```r
# UNSAFE
cache_save <- function(object, key) {
  path <- sprintf("cache/%s.rds", key)
  saveRDS(object, path)
}

cache_load <- function(key) {
  path <- sprintf("cache/%s.rds", key)
  if (file.exists(path)) {
    readRDS(path)
  } else {
    NULL
  }
}
```

#### Migrated Code:
```r
# SAFE
cache_save <- function(object, key) {
  path <- sprintf("cache/%s.rds", key)
  safe_write_rds(
    object, 
    path,
    create_backup = FALSE,  # Cache files don't need backups
    compress = TRUE
  )
}

cache_load <- function(key) {
  path <- sprintf("cache/%s.rds", key)
  safe_read_rds(path)  # Returns NULL on failure
}
```

## Migration Patterns

### Pattern 1: Simple Read/Write

```r
# Before
data <- read.csv("data.csv")
write.csv(data, "data.csv", row.names = FALSE)

# After
data <- safe_read_csv("data.csv")
safe_write_csv(data, "data.csv")
```

### Pattern 2: Configuration with Defaults

```r
# Before
config <- tryCatch(
  yaml::read_yaml("config.yaml"),
  error = function(e) {
    list(default = TRUE)
  }
)

# After
config <- safe_read_yaml(
  "config.yaml",
  fallback_paths = "config.yaml.default"
)

if (is.null(config)) {
  config <- list(default = TRUE)
}
```

### Pattern 3: Append Operations

```r
# Before
cat("New line\n", file = "log.txt", append = TRUE)

# After
existing <- safe_read_file("log.txt")
if (is.null(existing)) existing <- character()
new_content <- c(existing, "New line")
safe_write_file(new_content, "log.txt")
```

## Testing Migration

### Create Test Suite

```r
# tests/testthat/test-safe-io-migration.R

test_that("migrated auth functions work correctly", {
  # Setup test environment
  test_dir <- tempdir()
  test_file <- file.path(test_dir, "users.yaml")
  
  # Create test data
  users <- list(
    users = list(
      list(username = "test", password = "test123")
    )
  )
  yaml::write_yaml(users, test_file)
  
  # Test migrated function
  result <- auth_upgrade_hashes(test_file)
  expect_true(result)
  
  # Verify backup was created
  backup_dir <- file.path(test_dir, "backups")
  expect_true(dir.exists(backup_dir))
})

test_that("migrated cache functions handle missing files", {
  # Test loading non-existent cache
  result <- cache_load("nonexistent_key")
  expect_null(result)
  
  # Test saving and loading
  test_data <- list(a = 1, b = 2)
  cache_save(test_data, "test_key")
  
  loaded <- cache_load("test_key")
  expect_equal(loaded, test_data)
})
```

### Performance Testing

```r
# Benchmark old vs new
library(microbenchmark)

# Create test file
test_file <- tempfile(fileext = ".yaml")
test_data <- list(test = TRUE)

benchmark_results <- microbenchmark(
  old = {
    yaml::write_yaml(test_data, test_file)
    yaml::read_yaml(test_file)
  },
  new = {
    safe_write_yaml(test_data, test_file, create_backup = FALSE)
    safe_read_yaml(test_file)
  },
  times = 100
)

print(benchmark_results)
```

## Rollback Procedure

If issues occur after migration:

### 1. Restore Original Code
```bash
# Restore from version control
git checkout HEAD~1 -- R/auth.R R/db.R R/cache.R R/audit.R
```

### 2. Restore Data from Backups
```r
# List all backups
find_all_backups <- function(base_dir = ".") {
  backup_dirs <- list.dirs(
    base_dir, 
    pattern = "backups$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  all_backups <- list()
  for (dir in backup_dirs) {
    files <- list.files(dir, full.names = TRUE)
    all_backups[[dir]] <- files
  }
  
  return(all_backups)
}

# Restore specific file
restore_from_backup <- function(original_path, backup_path) {
  if (file.exists(backup_path)) {
    file.copy(backup_path, original_path, overwrite = TRUE)
    message(sprintf("Restored %s from %s", original_path, backup_path))
  } else {
    warning(sprintf("Backup not found: %s", backup_path))
  }
}
```

### 3. Verify System State
```r
# Health check after rollback
system_health_check <- function() {
  checks <- list(
    users_file = file.exists("config/users.yaml"),
    db_config = file.exists("config/database.yaml"),
    audit_log = file.exists("audit/audit_log.csv"),
    cache_dir = dir.exists("cache")
  )
  
  failed <- names(checks)[!unlist(checks)]
  
  if (length(failed) > 0) {
    warning(sprintf("Health check failed for: %s", 
                   paste(failed, collapse = ", ")))
    return(FALSE)
  }
  
  message("System health check passed")
  return(TRUE)
}

system_health_check()
```

## Monitoring After Migration

### 1. Check Backup Creation
```r
# Monitor backup directory growth
monitor_backups <- function() {
  backup_dirs <- list.dirs(".", pattern = "backups$", recursive = TRUE)
  
  for (dir in backup_dirs) {
    count <- length(list.files(dir))
    size_mb <- sum(file.info(list.files(dir, full.names = TRUE))$size) / 1024^2
    
    message(sprintf("%s: %d files, %.2f MB", dir, count, size_mb))
    
    if (size_mb > 100) {
      warning(sprintf("Large backup directory: %s", dir))
    }
  }
}

# Run daily
monitor_backups()
```

### 2. Verify File Integrity
```r
# Check critical files are readable
verify_critical_files <- function() {
  critical_files <- c(
    "config/users.yaml",
    "config/database.yaml",
    "config/api_keys.yaml"
  )
  
  for (file in critical_files) {
    content <- safe_read_yaml(file)
    if (is.null(content)) {
      warning(sprintf("Cannot read critical file: %s", file))
    } else {
      message(sprintf("✓ %s is readable", file))
    }
  }
}

verify_critical_files()
```

### 3. Performance Metrics
```r
# Track I/O operation times
.io_metrics <- new.env()

track_io_performance <- function() {
  # Wrap safe I/O functions with timing
  original_read <- safe_read_file
  
  safe_read_file <<- function(...) {
    start <- Sys.time()
    result <- original_read(...)
    duration <- as.numeric(Sys.time() - start)
    
    .io_metrics$read_times <- c(.io_metrics$read_times, duration)
    
    if (duration > 1) {
      warning(sprintf("Slow read operation: %.2f seconds", duration))
    }
    
    return(result)
  }
}

# Get performance summary
get_io_metrics <- function() {
  list(
    read_count = length(.io_metrics$read_times),
    read_mean = mean(.io_metrics$read_times, na.rm = TRUE),
    read_max = max(.io_metrics$read_times, na.rm = TRUE)
  )
}
```

## Common Migration Issues

### Issue 1: Permission Denied
```r
# Fix: Check and update permissions
fix_permissions <- function(path) {
  if (Sys.info()["sysname"] != "Windows") {
    system(sprintf("chmod 664 %s", path))
    system(sprintf("chmod 775 %s", dirname(path)))
  }
}
```

### Issue 2: Disk Space for Backups
```r
# Fix: Clean old backups before migration
pre_migration_cleanup <- function() {
  # Remove backups older than 30 days
  cutoff <- Sys.time() - (30 * 24 * 60 * 60)
  
  backup_dirs <- list.dirs(".", pattern = "backups$", recursive = TRUE)
  
  for (dir in backup_dirs) {
    files <- list.files(dir, full.names = TRUE)
    for (file in files) {
      if (file.info(file)$mtime < cutoff) {
        file.remove(file)
        message(sprintf("Removed old backup: %s", file))
      }
    }
  }
}
```

### Issue 3: Network Drive Compatibility
```r
# Fix: Disable atomic writes for network paths
configure_for_network <- function(path) {
  if (startsWith(path, "//") || startsWith(path, "\\\\")) {
    configure_safe_io(use_atomic_writes = FALSE)
    message("Configured for network drive (atomic writes disabled)")
  }
}
```

## Success Criteria

Migration is complete when:

- [ ] All critical files use safe I/O functions
- [ ] Test suite passes 100%
- [ ] No data loss incidents in 7 days
- [ ] Backup system functioning properly
- [ ] Performance metrics acceptable (< 10% overhead)
- [ ] Documentation updated
- [ ] Team trained on new patterns

## Support

For migration assistance:
1. Check this guide first
2. Review test files in `tests/testthat/test-safe-io.R`
3. Contact the development team
4. File issues in the project repository

## Appendix: Function Mapping

| Old Function | New Function | Notes |
|-------------|--------------|-------|
| `readLines()` | `safe_read_file()` | Returns NULL on failure |
| `writeLines()` | `safe_write_file()` | Creates backup by default |
| `read.csv()` | `safe_read_csv()` | Uses readr internally |
| `write.csv()` | `safe_write_csv()` | No row.names by default |
| `yaml::read_yaml()` | `safe_read_yaml()` | Supports fallback paths |
| `yaml::write_yaml()` | `safe_write_yaml()` | Validates YAML structure |
| `readRDS()` | `safe_read_rds()` | Handles corrupted files |
| `saveRDS()` | `safe_write_rds()` | Compression enabled |
| `jsonlite::read_json()` | `safe_read_file()` with `jsonlite::fromJSON` | Custom reader |
| `jsonlite::write_json()` | `safe_write_file()` with `jsonlite::toJSON` | Custom writer |