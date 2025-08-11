# tests/testthat/test-security.R
# Security Tests for PK/PD Shiny Application

library(testthat)
library(DBI)
library(RSQLite)
library(mockery)
library(sodium)

# Helper function for test database setup
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Create test tables
  dbExecute(con, "
    CREATE TABLE audit_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT,
      user TEXT,
      action TEXT,
      details TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password_hash TEXT,
      role TEXT,
      created_at TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE antibiogram_data (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      organism TEXT,
      antibiotic TEXT,
      mic_value REAL,
      resistance_rate REAL
    )
  ")
  
  return(con)
}

test_that("SQL queries are properly parameterized against injection", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Insert test data
  dbExecute(con, 
    "INSERT INTO antibiogram_data (organism, antibiotic, mic_value) VALUES (?, ?, ?)",
    params = list("E. coli", "Ampicillin", 2.0)
  )
  
  # Test various injection attempts
  malicious_inputs <- c(
    "'; DROP TABLE audit_log; --",
    "1' OR '1'='1",
    "admin'--",
    "'; DELETE FROM users; --",
    "' UNION SELECT * FROM users--"
  )
  
  for (malicious_input in malicious_inputs) {
    # Function should safely handle malicious input
    expect_no_error({
      result <- dbGetQuery(con, 
        "SELECT * FROM antibiogram_data WHERE organism = ?",
        params = list(malicious_input)
      )
    })
    
    # Verify tables still exist and are intact
    expect_true("audit_log" %in% dbListTables(con))
    expect_true("users" %in% dbListTables(con))
    expect_true("antibiogram_data" %in% dbListTables(con))
  }
})

test_that("No plaintext passwords are stored or accepted", {
  # Mock password hashing functions
  password_hash <- function(password) {
    sodium::password_store(password)
  }
  
  password_verify <- function(password, hash) {
    sodium::password_verify(hash, password)
  }
  
  # Test password hashing
  test_password <- "TestP@ssw0rd123"
  hash <- password_hash(test_password)
  
  # Hash should not contain the plaintext password
  expect_false(grepl(test_password, hash, fixed = TRUE))
  
  # Verification should work with correct password
  expect_true(password_verify(test_password, hash))
  
  # Verification should fail with wrong password
  expect_false(password_verify("WrongPassword", hash))
  
  # Test that plaintext comparison fails
  expect_false(test_password == hash)
})

test_that("Authentication requires proper credentials", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Create test user with hashed password
  test_user <- "testuser"
  test_pass <- "SecureP@ss123"
  pass_hash <- sodium::password_store(test_pass)
  
  dbExecute(con,
    "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)",
    params = list(test_user, pass_hash, "user")
  )
  
  # Mock authentication function
  authenticate_user <- function(con, username, password) {
    user <- dbGetQuery(con,
      "SELECT password_hash FROM users WHERE username = ?",
      params = list(username)
    )
    
    if (nrow(user) == 0) return(FALSE)
    
    return(sodium::password_verify(user$password_hash[1], password))
  }
  
  # Test successful authentication
  expect_true(authenticate_user(con, test_user, test_pass))
  
  # Test failed authentication scenarios
  expect_false(authenticate_user(con, test_user, "wrongpass"))
  expect_false(authenticate_user(con, "nonexistent", test_pass))
  expect_false(authenticate_user(con, "", ""))
  expect_false(authenticate_user(con, test_user, ""))
})

test_that("Input validation prevents XSS attacks", {
  # Mock input sanitization function
  sanitize_input <- function(input) {
    # Remove HTML tags and script elements
    input <- gsub("<script.*?</script>", "", input, ignore.case = TRUE)
    input <- gsub("<.*?>", "", input)
    # Escape special HTML characters
    input <- gsub("&", "&amp;", input)
    input <- gsub("<", "&lt;", input)
    input <- gsub(">", "&gt;", input)
    input <- gsub('"', "&quot;", input)
    input <- gsub("'", "&#39;", input)
    return(input)
  }
  
  # Test XSS prevention
  xss_attempts <- c(
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')",
    "<iframe src='malicious.com'></iframe>",
    "<body onload=alert('XSS')>"
  )
  
  for (xss in xss_attempts) {
    sanitized <- sanitize_input(xss)
    expect_false(grepl("<script", sanitized, ignore.case = TRUE))
    expect_false(grepl("javascript:", sanitized, ignore.case = TRUE))
    expect_false(grepl("onerror", sanitized, ignore.case = TRUE))
    expect_false(grepl("onload", sanitized, ignore.case = TRUE))
  }
})

test_that("File upload validation prevents malicious files", {
  # Mock file validation function
  validate_upload <- function(file_path, allowed_types = c("csv", "xlsx", "rds")) {
    if (!file.exists(file_path)) return(FALSE)
    
    # Check file extension
    ext <- tolower(tools::file_ext(file_path))
    if (!ext %in% allowed_types) return(FALSE)
    
    # Check file size (max 10MB)
    if (file.info(file_path)$size > 10 * 1024 * 1024) return(FALSE)
    
    # Check for suspicious content in CSV files
    if (ext == "csv") {
      lines <- readLines(file_path, n = 10, warn = FALSE)
      suspicious_patterns <- c("=SYSTEM", "=CMD", "@SUM", "+SUM")
      for (pattern in suspicious_patterns) {
        if (any(grepl(pattern, lines, fixed = TRUE))) return(FALSE)
      }
    }
    
    return(TRUE)
  }
  
  # Create test files
  temp_dir <- tempdir()
  
  # Valid CSV file
  valid_csv <- file.path(temp_dir, "valid.csv")
  write.csv(data.frame(a = 1:3, b = 4:6), valid_csv, row.names = FALSE)
  expect_true(validate_upload(valid_csv))
  
  # Invalid file type
  invalid_file <- file.path(temp_dir, "malicious.exe")
  writeLines("malicious content", invalid_file)
  expect_false(validate_upload(invalid_file))
  
  # CSV with formula injection attempt
  formula_csv <- file.path(temp_dir, "formula.csv")
  writeLines(c("name,value", "test,=SYSTEM('cmd.exe')"), formula_csv)
  expect_false(validate_upload(formula_csv))
  
  # Clean up
  unlink(c(valid_csv, invalid_file, formula_csv))
})

test_that("Session management is secure", {
  # Mock session functions
  create_session <- function(user_id) {
    session_id <- sodium::random(32)
    session_token <- sodium::bin2hex(session_id)
    # In real app, store in secure session store
    return(list(
      token = session_token,
      created_at = Sys.time(),
      expires_at = Sys.time() + 3600,  # 1 hour expiry
      user_id = user_id
    ))
  }
  
  validate_session <- function(session) {
    # Check if session exists and is not expired
    if (is.null(session)) return(FALSE)
    if (Sys.time() > session$expires_at) return(FALSE)
    if (nchar(session$token) != 64) return(FALSE)  # 32 bytes = 64 hex chars
    return(TRUE)
  }
  
  # Test session creation
  session <- create_session("user123")
  expect_true(!is.null(session$token))
  expect_equal(nchar(session$token), 64)
  expect_true(validate_session(session))
  
  # Test expired session
  expired_session <- session
  expired_session$expires_at <- Sys.time() - 1
  expect_false(validate_session(expired_session))
  
  # Test invalid session
  expect_false(validate_session(NULL))
  expect_false(validate_session(list(token = "short")))
})

test_that("Rate limiting prevents brute force attacks", {
  # Mock rate limiter
  rate_limiter <- new.env()
  
  check_rate_limit <- function(ip, max_attempts = 5, window_seconds = 60) {
    current_time <- as.numeric(Sys.time())
    
    if (!exists(ip, envir = rate_limiter)) {
      rate_limiter[[ip]] <- list(attempts = 1, first_attempt = current_time)
      return(TRUE)
    }
    
    attempts_data <- rate_limiter[[ip]]
    time_elapsed <- current_time - attempts_data$first_attempt
    
    if (time_elapsed > window_seconds) {
      # Reset window
      rate_limiter[[ip]] <- list(attempts = 1, first_attempt = current_time)
      return(TRUE)
    }
    
    if (attempts_data$attempts >= max_attempts) {
      return(FALSE)  # Rate limit exceeded
    }
    
    attempts_data$attempts <- attempts_data$attempts + 1
    rate_limiter[[ip]] <- attempts_data
    return(TRUE)
  }
  
  # Test rate limiting
  test_ip <- "192.168.1.1"
  
  # First 5 attempts should succeed
  for (i in 1:5) {
    expect_true(check_rate_limit(test_ip))
  }
  
  # 6th attempt should fail
  expect_false(check_rate_limit(test_ip))
  
  # Different IP should work
  expect_true(check_rate_limit("192.168.1.2"))
})

test_that("Audit logging captures security events", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Mock audit log function
  log_security_event <- function(con, user, action, details) {
    dbExecute(con,
      "INSERT INTO audit_log (timestamp, user, action, details) VALUES (?, ?, ?, ?)",
      params = list(
        as.character(Sys.time()),
        user,
        action,
        jsonlite::toJSON(details, auto_unbox = TRUE)
      )
    )
  }
  
  # Test logging various security events
  log_security_event(con, "admin", "login_success", list(ip = "192.168.1.1"))
  log_security_event(con, "unknown", "login_failure", list(ip = "10.0.0.1", reason = "invalid_password"))
  log_security_event(con, "user1", "data_export", list(table = "antibiogram_data", rows = 100))
  
  # Verify logs were created
  logs <- dbGetQuery(con, "SELECT * FROM audit_log")
  expect_equal(nrow(logs), 3)
  expect_true("login_success" %in% logs$action)
  expect_true("login_failure" %in% logs$action)
  expect_true("data_export" %in% logs$action)
})

test_that("Sensitive data is properly encrypted", {
  # Test encryption of sensitive configuration
  encrypt_config <- function(config, key) {
    serialized <- serialize(config, NULL)
    nonce <- sodium::random(24)
    encrypted <- sodium::data_encrypt(serialized, key, nonce)
    return(list(data = encrypted, nonce = nonce))
  }
  
  decrypt_config <- function(encrypted_config, key) {
    decrypted <- sodium::data_decrypt(
      encrypted_config$data, 
      key, 
      encrypted_config$nonce
    )
    return(unserialize(decrypted))
  }
  
  # Generate encryption key
  key <- sodium::keygen()
  
  # Test config with sensitive data
  sensitive_config <- list(
    db_password = "SuperSecret123",
    api_key = "sk-1234567890abcdef",
    admin_email = "admin@example.com"
  )
  
  # Encrypt
  encrypted <- encrypt_config(sensitive_config, key)
  
  # Encrypted data should not contain plaintext
  encrypted_str <- rawToChar(encrypted$data, multiple = TRUE)
  expect_false(any(grepl("SuperSecret123", encrypted_str)))
  expect_false(any(grepl("sk-1234567890", encrypted_str)))
  
  # Decrypt and verify
  decrypted <- decrypt_config(encrypted, key)
  expect_equal(decrypted, sensitive_config)
  
  # Wrong key should fail
  wrong_key <- sodium::keygen()
  expect_error(decrypt_config(encrypted, wrong_key))
})