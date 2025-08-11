#!/usr/bin/env Rscript
# scripts/fix_filename_case_sensitivity.R
# Migration script to fix inconsistent filename casing for Linux compatibility
# Run this script to rename .r files to .R and update all references

cat("========================================\n")
cat("File Case Sensitivity Fix for Linux\n")
cat("========================================\n\n")

# Function to check if running on case-sensitive filesystem
is_case_sensitive <- function() {
  temp_dir <- tempdir()
  test_file_lower <- file.path(temp_dir, "test.r")
  test_file_upper <- file.path(temp_dir, "test.R")
  
  # Create file with lowercase extension
  writeLines("test", test_file_lower)
  
  # Try to access with uppercase - on case-insensitive systems this works
  result <- !file.exists(test_file_upper)
  
  # Cleanup
  unlink(test_file_lower)
  
  return(result)
}

# Check current system
cat("Step 1: System Check\n")
cat("--------------------\n")
if (is_case_sensitive()) {
  cat("✓ Running on case-sensitive filesystem (Linux/Unix)\n")
} else {
  cat("ℹ Running on case-insensitive filesystem (Windows/macOS)\n")
}
cat("\n")

# Find all R files with lowercase extension
cat("Step 2: Scanning for inconsistencies\n")
cat("------------------------------------\n")

r_dir <- "R"
if (!dir.exists(r_dir)) {
  stop("R directory not found. Please run from project root.")
}

# Find files with .r extension (lowercase)
lowercase_files <- list.files(r_dir, pattern = "\\.r$", full.names = TRUE)

if (length(lowercase_files) == 0) {
  cat("✓ No files with .r extension found - all files already use .R\n")
} else {
  cat(sprintf("⚠ Found %d file(s) with .r extension:\n", length(lowercase_files)))
  for (file in lowercase_files) {
    cat(sprintf("  - %s\n", file))
  }
}
cat("\n")

# Find all source() calls in R files
cat("Step 3: Checking source() references\n")
cat("------------------------------------\n")

all_r_files <- list.files(r_dir, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
inconsistent_refs <- list()

for (file in all_r_files) {
  content <- readLines(file, warn = FALSE)
  
  # Find source() calls
  source_pattern <- 'source\\s*\\(["\']([^"\']+)["\']'
  
  for (i in seq_along(content)) {
    line <- content[i]
    
    # Check for source() calls
    if (grepl(source_pattern, line)) {
      matches <- gregexpr(source_pattern, line, perl = TRUE)
      match_text <- regmatches(line, matches)[[1]]
      
      for (match in match_text) {
        # Extract the file path from source()
        file_ref <- sub(source_pattern, "\\1", match, perl = TRUE)
        
        # Check if it references a .r file (lowercase)
        if (grepl("\\.r$", file_ref)) {
          inconsistent_refs[[length(inconsistent_refs) + 1]] <- list(
            file = file,
            line = i,
            reference = file_ref,
            full_line = line
          )
        }
      }
    }
  }
}

if (length(inconsistent_refs) > 0) {
  cat(sprintf("⚠ Found %d reference(s) to .r files:\n", length(inconsistent_refs)))
  for (ref in inconsistent_refs) {
    cat(sprintf("  File: %s (line %d)\n", ref$file, ref$line))
    cat(sprintf("    Reference: %s\n", ref$reference))
  }
} else {
  cat("✓ No references to .r files found\n")
}
cat("\n")

# Fix the issues
if (length(lowercase_files) > 0 || length(inconsistent_refs) > 0) {
  cat("Step 4: Apply fixes?\n")
  cat("-------------------\n")
  cat("This will:\n")
  cat("  1. Rename all .r files to .R\n")
  cat("  2. Update all source() references\n")
  cat("\n")
  
  # In non-interactive mode, proceed automatically
  if (!interactive()) {
    response <- "y"
    cat("Non-interactive mode - proceeding with fixes\n")
  } else {
    response <- readline("Proceed with fixes? (y/n): ")
  }
  
  if (tolower(response) == "y") {
    cat("\nApplying fixes...\n")
    
    # First, rename the files
    if (length(lowercase_files) > 0) {
      cat("\nRenaming files:\n")
      for (old_file in lowercase_files) {
        new_file <- sub("\\.r$", ".R", old_file)
        
        # Check if target already exists (shouldn't on case-sensitive systems)
        if (file.exists(new_file) && old_file != new_file) {
          cat(sprintf("  ✗ Cannot rename %s - %s already exists\n", 
                     basename(old_file), basename(new_file)))
        } else {
          success <- file.rename(old_file, new_file)
          if (success) {
            cat(sprintf("  ✓ Renamed %s to %s\n", 
                       basename(old_file), basename(new_file)))
          } else {
            cat(sprintf("  ✗ Failed to rename %s\n", basename(old_file)))
          }
        }
      }
    }
    
    # Then update references
    if (length(inconsistent_refs) > 0) {
      cat("\nUpdating references:\n")
      
      # Group by file
      files_to_update <- unique(sapply(inconsistent_refs, function(x) x$file))
      
      for (file in files_to_update) {
        content <- readLines(file, warn = FALSE)
        modified <- FALSE
        
        # Get all refs for this file
        file_refs <- Filter(function(x) x$file == file, inconsistent_refs)
        
        for (ref in file_refs) {
          # Replace .r with .R in the line
          old_line <- content[ref$line]
          new_line <- gsub("\\.r([\"'])", ".R\\1", old_line)
          
          if (old_line != new_line) {
            content[ref$line] <- new_line
            modified <- TRUE
            cat(sprintf("  ✓ Updated %s (line %d)\n", basename(file), ref$line))
          }
        }
        
        # Write back if modified
        if (modified) {
          writeLines(content, file)
        }
      }
    }
    
    cat("\n✅ All fixes applied successfully!\n")
    
  } else {
    cat("\nNo changes made.\n")
  }
} else {
  cat("Step 4: Summary\n")
  cat("---------------\n")
  cat("✅ No fixes needed - all files already follow conventions!\n")
}

# Final verification
cat("\nStep 5: Final Verification\n")
cat("--------------------------\n")

# Check again for .r files
remaining_lowercase <- list.files(r_dir, pattern = "\\.r$", full.names = TRUE)
if (length(remaining_lowercase) == 0) {
  cat("✓ All R files use .R extension\n")
} else {
  cat(sprintf("⚠ Still found %d files with .r extension\n", length(remaining_lowercase)))
}

# Test that critical files can be sourced
critical_files <- c("R/utils.R", "R/db.R", "R/load_all.R")
cat("\nTesting critical files can be loaded:\n")

for (file in critical_files) {
  if (file.exists(file)) {
    # Try to parse (not execute) the file
    tryCatch({
      parse(file)
      cat(sprintf("  ✓ %s - OK\n", file))
    }, error = function(e) {
      cat(sprintf("  ✗ %s - Parse error: %s\n", file, e$message))
    })
  } else {
    cat(sprintf("  ⚠ %s - Not found\n", file))
  }
}

cat("\n========================================\n")
cat("Migration complete!\n")
cat("========================================\n")
cat("\nRecommended next steps:\n")
cat("1. Run tests to ensure everything works: testthat::test_dir('tests')\n")
cat("2. Commit these changes with message: 'fix: standardize R file extensions for Linux compatibility'\n")
cat("3. Test on a Linux system if possible\n")