# R/safe_io.R
# Safe I/O operations with automatic backup and recovery
# Requires utils.R to be loaded first for %||% operator

# Configuration environment
.io_config <- new.env(parent = emptyenv())
.io_config$backup_dir <- "backups"
.io_config$max_backups <- 10
.io_config$validate_after_write <- TRUE
.io_config$use_atomic_writes <- TRUE

#' Safe file writing with backup and validation
#' 
#' @param content Content to write
#' @param path File path
#' @param writer_fn Function to write content (default: writeLines)
#' @param create_backup Create backup before overwriting
#' @param validate_fn Optional validation function
#' @return TRUE on success, FALSE on failure
safe_write_file <- function(content, path, 
                          writer_fn = writeLines,
                          create_backup = TRUE,
                          validate_fn = NULL) {
  
  # Normalize path
  path <- normalizePath(path, mustWork = FALSE)
  dir_path <- dirname(path)
  
  # Ensure directory exists
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create backup if file exists
  if (create_backup && file.exists(path)) {
    backup_path <- create_file_backup(path)
    if (is.null(backup_path)) {
      warning("Could not create backup for: ", path)
    }
  }
  
  # Write to temporary file first (atomic write)
  temp_path <- tempfile(tmpdir = dir_path, fileext = paste0(".", tools::file_ext(path)))
  
  # Attempt write
  write_success <- tryCatch({
    if (is.function(writer_fn)) {
      writer_fn(content, temp_path)
    } else {
      writeLines(as.character(content), temp_path)
    }
    TRUE
  }, error = function(e) {
    message("Write failed: ", e$message)
    FALSE
  })
  
  if (!write_success) {
    unlink(temp_path, force = TRUE)
    return(FALSE)
  }
  
  # Validate if function provided
  if (!is.null(validate_fn) && is.function(validate_fn)) {
    validation <- validate_fn(temp_path)
    if (!isTRUE(validation$valid)) {
      message("Validation failed: ", validation$error %||% "Unknown error")
      unlink(temp_path, force = TRUE)
      return(FALSE)
    }
  }
  
  # Atomic move to final location
  move_success <- tryCatch({
    file.rename(temp_path, path)
  }, error = function(e) {
    # Fallback to copy+delete if rename fails (cross-device)
    tryCatch({
      file.copy(temp_path, path, overwrite = TRUE)
      unlink(temp_path)
      TRUE
    }, error = function(e2) {
      FALSE
    })
  })
  
  if (!move_success) {
    message("Could not move file to final location")
    unlink(temp_path, force = TRUE)
    return(FALSE)
  }
  
  # Clean old backups
  if (create_backup) {
    clean_old_backups(path)
  }
  
  return(TRUE)
}

#' Safe file reading with fallback
#' 
#' @param path File path
#' @param reader_fn Function to read content (default: readLines)
#' @param fallback_value Value to return on failure
#' @param use_backup Try backup if main file fails
#' @return File content or fallback value
safe_read_file <- function(path, 
                         reader_fn = readLines,
                         fallback_value = NULL,
                         use_backup = TRUE) {
  
  # Try primary file
  content <- tryCatch({
    if (file.exists(path)) {
      if (is.function(reader_fn)) {
        reader_fn(path)
      } else {
        readLines(path, warn = FALSE)
      }
    } else {
      NULL
    }
  }, error = function(e) {
    message("Read failed: ", e$message)
    NULL
  })
  
  # If failed and backup enabled, try latest backup
  if (is.null(content) && use_backup) {
    backup_path <- get_latest_backup(path)
    if (!is.null(backup_path)) {
      message("Attempting to read from backup: ", backup_path)
      content <- tryCatch({
        if (is.function(reader_fn)) {
          reader_fn(backup_path)
        } else {
          readLines(backup_path, warn = FALSE)
        }
      }, error = function(e) {
        message("Backup read also failed: ", e$message)
        NULL
      })
      
      if (!is.null(content)) {
        message("Successfully recovered from backup")
        # Optionally restore the backup
        file.copy(backup_path, path, overwrite = TRUE)
      }
    }
  }
  
  # Return content or fallback
  if (is.null(content)) {
    return(fallback_value)
  }
  
  return(content)
}

#' Create backup of file
#' 
#' @param path Original file path
#' @return Backup file path or NULL on failure
create_file_backup <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  
  # Create backup directory
  backup_dir <- file.path(dirname(path), .io_config$backup_dir)
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Generate backup filename with timestamp
  base_name <- basename(path)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_name <- sprintf("%s.%s.bak", base_name, timestamp)
  backup_path <- file.path(backup_dir, backup_name)
  
  # Copy to backup
  success <- tryCatch({
    file.copy(path, backup_path, overwrite = FALSE)
  }, error = function(e) {
    FALSE
  })
  
  if (success) {
    return(backup_path)
  } else {
    return(NULL)
  }
}

#' Get latest backup for a file
#' 
#' @param path Original file path
#' @return Path to latest backup or NULL
get_latest_backup <- function(path) {
  backup_dir <- file.path(dirname(path), .io_config$backup_dir)
  
  if (!dir.exists(backup_dir)) {
    return(NULL)
  }
  
  base_name <- basename(path)
  pattern <- sprintf("^%s\\..*\\.bak$", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base_name))
  
  backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
  
  if (length(backups) == 0) {
    return(NULL)
  }
  
  # Get file info and sort by modification time
  info <- file.info(backups)
  latest <- backups[which.max(info$mtime)]
  
  return(latest)
}

#' Clean old backup files
#' 
#' @param path Original file path
#' @param max_backups Maximum number of backups to keep
clean_old_backups <- function(path, max_backups = NULL) {
  max_backups <- max_backups %||% .io_config$max_backups
  
  backup_dir <- file.path(dirname(path), .io_config$backup_dir)
  
  if (!dir.exists(backup_dir)) {
    return()
  }
  
  base_name <- basename(path)
  pattern <- sprintf("^%s\\..*\\.bak$", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base_name))
  
  backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
  
  if (length(backups) <= max_backups) {
    return()
  }
  
  # Get file info and sort by modification time
  info <- file.info(backups)
  sorted_backups <- backups[order(info$mtime, decreasing = TRUE)]
  
  # Remove old backups
  to_remove <- sorted_backups[(max_backups + 1):length(sorted_backups)]
  unlink(to_remove)
  
  message(sprintf("Cleaned %d old backup(s)", length(to_remove)))
}

#' List all backups for a file
#' 
#' @param path Original file path
#' @return Data frame with backup information
list_backups <- function(path) {
  backup_dir <- file.path(dirname(path), .io_config$backup_dir)
  
  if (!dir.exists(backup_dir)) {
    return(data.frame(
      file = character(),
      size = numeric(),
      modified = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  base_name <- basename(path)
  pattern <- sprintf("^%s\\..*\\.bak$", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base_name))
  
  backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
  
  if (length(backups) == 0) {
    return(data.frame(
      file = character(),
      size = numeric(),
      modified = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  info <- file.info(backups)
  
  data.frame(
    file = basename(backups),
    size = info$size,
    modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

#' Restore file from backup
#' 
#' @param path Original file path
#' @param backup_file Specific backup file name or NULL for latest
#' @return TRUE on success
restore_from_backup <- function(path, backup_file = NULL) {
  if (is.null(backup_file)) {
    backup_path <- get_latest_backup(path)
    if (is.null(backup_path)) {
      message("No backups found for: ", path)
      return(FALSE)
    }
  } else {
    backup_dir <- file.path(dirname(path), .io_config$backup_dir)
    backup_path <- file.path(backup_dir, backup_file)
    
    if (!file.exists(backup_path)) {
      message("Backup file not found: ", backup_path)
      return(FALSE)
    }
  }
  
  # Create backup of current file before restoring
  if (file.exists(path)) {
    create_file_backup(path)
  }
  
  # Restore from backup
  success <- tryCatch({
    file.copy(backup_path, path, overwrite = TRUE)
  }, error = function(e) {
    message("Restore failed: ", e$message)
    FALSE
  })
  
  if (success) {
    message("Successfully restored from: ", basename(backup_path))
  }
  
  return(success)
}

# Specialized safe I/O functions

#' Safe YAML read/write
safe_read_yaml <- function(path, fallback_value = NULL) {
  safe_read_file(
    path = path,
    reader_fn = yaml::read_yaml,
    fallback_value = fallback_value
  )
}

safe_write_yaml <- function(data, path, create_backup = TRUE) {
  safe_write_file(
    content = data,
    path = path,
    writer_fn = function(d, p) yaml::write_yaml(d, p),
    create_backup = create_backup,
    validate_fn = function(temp_path) {
      test_read <- tryCatch({
        yaml::read_yaml(temp_path)
      }, error = function(e) NULL)
      
      if (is.null(test_read)) {
        return(list(valid = FALSE, error = "Cannot parse YAML"))
      }
      
      return(list(valid = TRUE))
    }
  )
}

#' Safe CSV read/write
safe_read_csv <- function(path, fallback_value = NULL, ...) {
  safe_read_file(
    path = path,
    reader_fn = function(p) read.csv(p, stringsAsFactors = FALSE, ...),
    fallback_value = fallback_value
  )
}

safe_write_csv <- function(data, path, create_backup = TRUE, ...) {
  safe_write_file(
    content = data,
    path = path,
    writer_fn = function(d, p) write.csv(d, p, row.names = FALSE, ...),
    create_backup = create_backup
  )
}

#' Safe RDS read/write
safe_read_rds <- function(path, fallback_value = NULL) {
  safe_read_file(
    path = path,
    reader_fn = readRDS,
    fallback_value = fallback_value
  )
}

safe_write_rds <- function(data, path, create_backup = TRUE, compress = TRUE) {
  safe_write_file(
    content = data,
    path = path,
    writer_fn = function(d, p) saveRDS(d, p, compress = compress),
    create_backup = create_backup,
    validate_fn = function(temp_path) {
      test_read <- tryCatch({
        readRDS(temp_path)
      }, error = function(e) NULL)
      
      if (is.null(test_read)) {
        return(list(valid = FALSE, error = "Cannot read back RDS file"))
      }
      
      return(list(valid = TRUE))
    }
  )
}

#' Configure safe I/O settings
#' @param backup_dir Directory for backups
#' @param max_backups Maximum number of backups to keep
#' @param validate_after_write Whether to validate after writing
#' @param use_atomic_writes Whether to use atomic write operations
configure_safe_io <- function(backup_dir = NULL, max_backups = NULL,
                             validate_after_write = NULL, use_atomic_writes = NULL) {
  if (!is.null(backup_dir)) {
    .io_config$backup_dir <<- backup_dir
  }
  if (!is.null(max_backups)) {
    .io_config$max_backups <<- max_backups
  }
  if (!is.null(validate_after_write)) {
    .io_config$validate_after_write <<- validate_after_write
  }
  if (!is.null(use_atomic_writes)) {
    .io_config$use_atomic_writes <<- use_atomic_writes
  }
  
  invisible(.io_config)
}

#' Get current safe I/O configuration
get_safe_io_config <- function() {
  .io_config
}