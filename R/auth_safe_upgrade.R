# R/auth_safe_upgrade.R - Enhanced auth functions with safe I/O
# This file contains the enhanced version of auth_upgrade_hashes that uses safe I/O

# Source the safe I/O module
source("R/safe_io.R")

#' Check if a password hash needs upgrade
#' @param hash The password hash to check
#' @return Logical indicating if upgrade is needed
needs_hash_upgrade <- function(hash) {
  if (is.null(hash) || length(hash) == 0) {
    return(FALSE)
  }
  
  # Check if it's a plaintext password (no $ signs typical of hashes)
  if (!grepl("\\$", hash)) {
    return(TRUE)
  }
  
  # Check for old hash formats (MD5, SHA1, etc.)
  if (grepl("^[a-f0-9]{32}$", hash, ignore.case = TRUE)) {  # MD5
    return(TRUE)
  }
  if (grepl("^[a-f0-9]{40}$", hash, ignore.case = TRUE)) {  # SHA1
    return(TRUE)
  }
  
  # Check hash version if stored
  # Modern argon2 hashes start with $argon2
  if (!grepl("^\\$argon2", hash)) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' Upgrade a password hash to modern format
#' @param old_hash The old hash or plaintext password
#' @return New secure hash
upgrade_password_hash <- function(old_hash) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("Package 'sodium' is required for password hashing")
  }
  
  # If it's plaintext or old format, treat it as the password
  if (!grepl("^\\$argon2", old_hash)) {
    # Create new hash using sodium/argon2
    new_hash <- sodium::password_store(charToRaw(old_hash))
    return(as.character(new_hash))
  }
  
  # Already modern format
  return(old_hash)
}

#' Validate user structure
#' @param users The users list structure
#' @return List with valid status and error message
validate_user_structure <- function(users) {
  if (!is.list(users)) {
    return(list(valid = FALSE, error = "Users must be a list"))
  }
  
  # Check for users section
  if (is.null(users$users)) {
    return(list(valid = FALSE, error = "Missing 'users' section"))
  }
  
  required_fields <- c("username", "role")
  valid_roles <- c("admin", "clinician", "viewer", "user")
  
  for (i in seq_along(users$users)) {
    user_data <- users$users[[i]]
    
    if (!is.list(user_data)) {
      return(list(valid = FALSE, 
                 error = sprintf("User at index %d must be a list", i)))
    }
    
    # Check required fields
    missing_fields <- setdiff(required_fields, names(user_data))
    if (length(missing_fields) > 0) {
      username <- user_data$username %||% sprintf("index_%d", i)
      return(list(valid = FALSE,
                 error = sprintf("User %s missing fields: %s", 
                               username, paste(missing_fields, collapse = ", "))))
    }
    
    # Validate role
    if (!user_data$role %in% valid_roles) {
      return(list(valid = FALSE,
                 error = sprintf("User %s has invalid role: %s", 
                               user_data$username, user_data$role)))
    }
    
    # Check that either password or password_hash exists
    if (is.null(user_data$password) && is.null(user_data$password_hash)) {
      return(list(valid = FALSE,
                 error = sprintf("User %s has neither password nor password_hash", 
                               user_data$username)))
    }
  }
  
  return(list(valid = TRUE))
}

#' Safely upgrade password hashes with full error recovery
#' @param users_file Path to users YAML file
#' @param backup Enable backup creation
#' @return Logical indicating success
auth_upgrade_hashes <- function(users_file = "config/users.yaml", backup = TRUE) {
  
  # Source audit functions if available
  audit_available <- file.exists("R/audit.R")
  if (audit_available) {
    source("R/audit.R")
  }
  
  # Define audit wrapper function
  audit_event <- function(event, data) {
    if (audit_available && exists("audit_append_hashchain")) {
      tryCatch({
        audit_append_hashchain(
          actor = "system",
          action = event,
          payload = data
        )
      }, error = function(e) {
        warning(sprintf("Failed to write audit: %s", e$message))
      })
    }
  }
  
  # Use safe read with fallbacks
  users <- safe_read_yaml(
    users_file,
    fallback_paths = c(
      paste0(users_file, ".backup"),
      paste0(users_file, ".bak"),
      paste0(users_file, ".default"),
      "config/users.yaml.example"
    )
  )
  
  if (is.null(users)) {
    stop("Cannot read users configuration from any source")
  }
  
  # Validate structure before processing
  pre_validation <- validate_user_structure(users)
  if (!pre_validation$valid) {
    stop(sprintf("Invalid user structure: %s", pre_validation$error))
  }
  
  # Track changes
  upgraded_users <- character()
  original_users <- users  # Keep original for comparison
  changed <- FALSE
  
  tryCatch({
    # Process each user
    for (i in seq_along(users$users)) {
      user_data <- users$users[[i]]
      username <- user_data$username
      
      # Check if upgrade is needed
      needs_upgrade <- FALSE
      
      # Check for plaintext password
      if (!is.null(user_data$password) && is.null(user_data$password_hash)) {
        message(sprintf("User %s has plaintext password, upgrading...", username))
        
        # Hash the password
        user_data$password_hash <- as.character(
          sodium::password_store(charToRaw(user_data$password))
        )
        
        # Remove plaintext password
        user_data$password <- NULL
        needs_upgrade <- TRUE
        
      } else if (!is.null(user_data$password_hash)) {
        # Check if existing hash needs upgrade
        if (needs_hash_upgrade(user_data$password_hash)) {
          message(sprintf("User %s has old hash format, upgrading...", username))
          
          old_hash <- user_data$password_hash
          new_hash <- upgrade_password_hash(old_hash)
          
          user_data$password_hash <- new_hash
          needs_upgrade <- TRUE
        }
      }
      
      if (needs_upgrade) {
        # Add metadata
        user_data$hash_version <- "2.0"
        user_data$upgraded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        # Update in list
        users$users[[i]] <- user_data
        upgraded_users <- c(upgraded_users, username)
        changed <- TRUE
        
        message(sprintf("✓ Upgraded hash for user: %s", username))
      }
    }
    
    if (changed) {
      # Validate before writing
      post_validation <- validate_user_structure(users)
      if (!post_validation$valid) {
        stop(sprintf("User structure validation failed after upgrade: %s", 
                    post_validation$error))
      }
      
      # Write with safe I/O
      write_success <- safe_write_yaml(
        users,
        users_file,
        create_backup = backup
      )
      
      if (write_success) {
        # Audit the upgrade
        audit_event("auth_hashes_upgraded", list(
          users_upgraded = upgraded_users,
          count = length(upgraded_users),
          timestamp = Sys.time(),
          file = users_file
        ))
        
        message(sprintf("\n✓ Successfully upgraded %d user hashes", 
                       length(upgraded_users)))
        
        # Verify the written file
        verify_users <- safe_read_yaml(users_file)
        if (!is.null(verify_users)) {
          message("✓ Verified written file is readable")
        } else {
          warning("⚠ Could not verify written file, but backup exists")
        }
        
      } else {
        stop("Failed to write upgraded users file")
      }
      
    } else {
      message("✓ No users need hash upgrade")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    # Log error
    audit_event("auth_upgrade_failed", list(
      error = e$message,
      users_affected = upgraded_users,
      timestamp = Sys.time()
    ))
    
    warning(sprintf("Hash upgrade failed: %s", e$message))
    
    # Attempt to verify file integrity
    current_content <- safe_read_yaml(users_file)
    if (!identical(current_content, original_users)) {
      warning("Users file may have been modified, check backups in config/backups/")
    }
    
    return(FALSE)
  })
}

#' Batch upgrade multiple user files
#' @param file_pattern Pattern to match user files
#' @param base_dir Base directory to search
#' @return List of results
batch_upgrade_user_files <- function(file_pattern = "users*.yaml", 
                                   base_dir = "config") {
  files <- list.files(base_dir, pattern = file_pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    warning(sprintf("No files matching pattern '%s' in %s", file_pattern, base_dir))
    return(list())
  }
  
  results <- list()
  
  for (file in files) {
    message(sprintf("\nProcessing: %s", file))
    message(paste(rep("-", 50), collapse = ""))
    
    result <- tryCatch({
      auth_upgrade_hashes(file, backup = TRUE)
      list(file = file, success = TRUE, error = NULL)
    }, error = function(e) {
      list(file = file, success = FALSE, error = e$message)
    })
    
    results[[file]] <- result
  }
  
  # Summary
  message("\n", paste(rep("=", 50), collapse = ""))
  message("Batch Upgrade Summary:")
  success_count <- sum(sapply(results, function(r) r$success))
  fail_count <- length(results) - success_count
  
  message(sprintf("✓ Successful: %d", success_count))
  message(sprintf("✗ Failed: %d", fail_count))
  
  if (fail_count > 0) {
    message("\nFailed files:")
    for (r in results) {
      if (!r$success) {
        message(sprintf("  - %s: %s", r$file, r$error))
      }
    }
  }
  
  return(results)
}

#' Restore user file from backup
#' @param users_file Path to users file
#' @param backup_index Which backup to restore (1 = most recent)
#' @return Logical indicating success
restore_users_from_backup <- function(users_file = "config/users.yaml", 
                                    backup_index = 1) {
  backup_dir <- file.path(dirname(users_file), .io_config$backup_dir)
  
  if (!dir.exists(backup_dir)) {
    stop(sprintf("Backup directory does not exist: %s", backup_dir))
  }
  
  pattern <- sprintf("%s\\.backup_.*", basename(users_file))
  backups <- list.files(backup_dir, pattern = pattern, full.names = TRUE)
  
  if (length(backups) == 0) {
    stop("No backups found")
  }
  
  # Sort by modification time, newest first
  backups <- backups[order(file.info(backups)$mtime, decreasing = TRUE)]
  
  if (backup_index > length(backups)) {
    stop(sprintf("Only %d backups available, requested index %d", 
                length(backups), backup_index))
  }
  
  backup_file <- backups[backup_index]
  message(sprintf("Restoring from: %s", backup_file))
  
  # Create backup of current file before restoring
  if (file.exists(users_file)) {
    pre_restore_backup <- sprintf("%s.pre_restore_%s", 
                                 users_file,
                                 format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(users_file, pre_restore_backup)
    message(sprintf("Current file backed up to: %s", pre_restore_backup))
  }
  
  # Restore
  success <- file.copy(backup_file, users_file, overwrite = TRUE)
  
  if (success) {
    message("✓ Successfully restored from backup")
    
    # Verify restored file
    users <- safe_read_yaml(users_file)
    if (!is.null(users)) {
      validation <- validate_user_structure(users)
      if (validation$valid) {
        message("✓ Restored file is valid")
      } else {
        warning(sprintf("⚠ Restored file has issues: %s", validation$error))
      }
    }
  } else {
    stop("Failed to restore from backup")
  }
  
  return(success)
}