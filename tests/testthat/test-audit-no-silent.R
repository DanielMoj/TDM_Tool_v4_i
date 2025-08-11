# test-audit-no-silent.R - Unit Tests für Audit und Auth System ohne Silent Failures

library(testthat)
library(mockery)
library(DBI)
library(RSQLite)

# Source der zu testenden Dateien
source("../../R/audit.r")
source("../../R/audit_fallback.R")
source("../../api/plumber_auth.R")

# Helper-Funktionen für Tests
setup_test_environment <- function() {
  # Erstelle temporäre Verzeichnisse
  test_dirs <- c("test_audit", "test_log", "test_audit/emergency", "test_audit/backup")
  for (dir in test_dirs) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Setze Test-Pfade
  assign("AUDIT_CSV_PATH", "test_audit/audit_log.csv", envir = .GlobalEnv)
  assign("AUDIT_ERROR_LOG", "test_log/audit_errors.log", envir = .GlobalEnv)
  assign("AUDIT_FALLBACK_PATH", "test_audit/audit_fallback.csv", envir = .GlobalEnv)
}

cleanup_test_environment <- function() {
  # Lösche Test-Dateien
  unlink("test_audit", recursive = TRUE)
  unlink("test_log", recursive = TRUE)
}

# Test Suite 1: Audit System Error Handling
describe("Audit System Error Handling", {
  
  test_that("audit_append_hashchain logs DB errors properly", {
    setup_test_environment()
    
    # Mock fehlschlagende DB-Verbindung
    mock_db_write <- mock(stop("Connection refused"))
    stub(audit_append_hashchain, ".audit_write_to_db", mock_db_write)
    
    # Sollte Warning ausgeben, NICHT silent fehlschlagen
    expect_warning(
      result <- audit_append_hashchain(
        file = AUDIT_CSV_PATH,
        actor = "test_user",
        action = "test_action",
        payload = list(test = TRUE)
      ),
      regexp = "Audit DB write failed"
    )
    
    # Prüfe dass Funktion ein Result zurückgibt
    expect_type(result, "list")
    expect_false(result$db_synced)
    expect_true(result$success)  # CSV sollte trotzdem funktionieren
    
    # Error log sollte existieren und Fehlermeldung enthalten
    expect_true(file.exists(AUDIT_ERROR_LOG))
    log_content <- readLines(AUDIT_ERROR_LOG)
    expect_true(any(grepl("Connection refused", log_content)))
    expect_true(any(grepl("AUDIT_DB_FAIL", log_content)))
    
    cleanup_test_environment()
  })
  
  test_that("audit creates CSV fallback when DB fails", {
    setup_test_environment()
    
    # Mock fehlschlagende DB
    mock_db_write <- mock(stop("Database unavailable"))
    stub(audit_append_hashchain, ".audit_write_to_db", mock_db_write)
    
    # Füge mehrere Einträge hinzu
    for (i in 1:3) {
      expect_warning(
        audit_append_hashchain(
          file = AUDIT_CSV_PATH,
          actor = sprintf("user_%d", i),
          action = sprintf("action_%d", i),
          payload = list(index = i)
        )
      )
    }
    
    # CSV sollte existieren und Einträge enthalten
    expect_true(file.exists(AUDIT_CSV_PATH))
    csv_data <- read.csv(AUDIT_CSV_PATH, stringsAsFactors = FALSE)
    expect_equal(nrow(csv_data), 3)
    expect_equal(csv_data$db_sync_status, rep("failed", 3))
    expect_true(all(nchar(csv_data$db_sync_error) > 0))
    
    cleanup_test_environment()
  })
  
  test_that("audit fallback cascade works correctly", {
    setup_test_environment()
    
    event_data <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      actor = "test_user",
      action = "critical_action",
      payload = '{"critical": true}',
      hash = "testhash123",
      prev_hash = "prevhash456"
    )
    
    # Mock alle DB-Verbindungen als fehlschlagend
    mock_db <- mock(stop("All databases down"))
    stub(audit_fallback_cascade, ".fallback_level1_db_retry", 
         list(success = FALSE, error = "DB retry failed"))
    
    result <- audit_fallback_cascade(event_data)
    
    expect_true(result$success)
    expect_true(result$fallback_level %in% c(2, 3))
    
    cleanup_test_environment()
  })
  
  test_that("hash chain verification detects tampering", {
    setup_test_environment()
    
    # Erstelle valide Chain
    df <- data.frame(
      timestamp = c("2024-01-01 10:00:00", "2024-01-01 10:01:00"),
      actor = c("user1", "user2"),
      action = c("login", "logout"),
      payload = c('{"ip":"1.2.3.4"}', '{"ip":"5.6.7.8"}'),
      hash = c("hash1", "hash2"),
      prev_hash = c("GENESIS", "hash1"),
      db_sync_status = c("success", "success"),
      db_sync_error = c("", ""),
      stringsAsFactors = FALSE
    )
    
    # Berechne korrekte Hashes
    df$hash[1] <- digest::digest(
      paste(df$timestamp[1], df$actor[1], df$action[1], 
            df$payload[1], df$prev_hash[1], sep = "|"),
      algo = "sha256"
    )
    df$prev_hash[2] <- df$hash[1]
    df$hash[2] <- digest::digest(
      paste(df$timestamp[2], df$actor[2], df$action[2], 
            df$payload[2], df$prev_hash[2], sep = "|"),
      algo = "sha256"
    )
    
    write.csv(df, AUDIT_CSV_PATH, row.names = FALSE)
    
    # Sollte als valide erkannt werden
    expect_true(audit_verify_hashchain(AUDIT_CSV_PATH))
    
    # Manipuliere einen Eintrag
    df$action[1] <- "malicious_change"
    write.csv(df, AUDIT_CSV_PATH, row.names = FALSE)
    
    # Sollte als invalide erkannt werden
    expect_false(audit_verify_hashchain(AUDIT_CSV_PATH))
    
    cleanup_test_environment()
  })
})

# Test Suite 2: Authentication System
describe("Authentication System Error Handling", {
  
  test_that("rate limiting blocks after max attempts", {
    # Reset rate limit environment
    rm(list = ls(.auth_attempts), envir = .auth_attempts)
    
    username <- "test_user"
    ip <- "192.168.1.100"
    
    # Erste AUTH_MAX_ATTEMPTS - 1 Versuche sollten durchgehen
    for (i in 1:(AUTH_MAX_ATTEMPTS - 1)) {
      expect_true(check_rate_limit(username, ip))
    }
    
    # Nächster Versuch sollte fehlschlagen
    expect_error(
      check_rate_limit(username, ip),
      regexp = "Too many login attempts"
    )
    
    # IP sollte jetzt geblockt sein
    expect_true(.is_ip_blocked(ip))
  })
  
  test_that("auth_check handles DB errors gracefully", {
    # Mock fehlschlagende DB-Verbindung
    mock_db_connect <- mock(stop("Database connection failed"))
    stub(auth_check, "DBI::dbConnect", mock_db_connect)
    
    result <- auth_check("test_user", "test_password")
    
    expect_false(result$success)
    expect_equal(result$reason, "auth_system_error")
    
    # Fehler sollte geloggt worden sein
    expect_true(file.exists(AUTH_ERROR_LOG_PATH))
  })
  
  test_that("successful login resets rate limit", {
    username <- "valid_user"
    ip <- "192.168.1.101"
    
    # Füge einige fehlgeschlagene Versuche hinzu
    key <- paste0(username, "_", ip)
    .auth_attempts[[key]] <- list(Sys.time(), Sys.time())
    
    # Reset sollte den Key entfernen
    .reset_rate_limit(username, ip)
    
    expect_false(exists(key, envir = .auth_attempts))
  })
  
  test_that("JWT token generation and verification works", {
    user_id <- 123
    username <- "test_user"
    role <- "admin"
    
    # Generiere Token
    token <- .generate_jwt_token(user_id, username, role)
    
    expect_type(token, "character")
    expect_true(nchar(token) > 0)
    
    # Verifiziere Token
    payload <- jose::jwt_decode_hmac(token, secret = JWT_SECRET)
    
    expect_equal(payload$sub, as.character(user_id))
    expect_equal(payload$username, username)
    expect_equal(payload$role, role)
    expect_true(payload$exp > as.numeric(Sys.time()))
  })
})

# Test Suite 3: Fallback System
describe("Fallback System", {
  
  test_that("SQLite fallback works when primary DB fails", {
    setup_test_environment()
    
    event_data <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      actor = "test_user",
      action = "test_action",
      payload = '{"test": true}',
      hash = "hash789",
      prev_hash = "prevhash"
    )
    
    # Führe Level 2 Fallback aus
    result <- .fallback_level2_local_storage(event_data)
    
    expect_true(result$success)
    expect_equal(result$storage, "sqlite")
    
    # Prüfe ob Daten in SQLite gespeichert wurden
    sqlite_path <- file.path(FALLBACK_CONFIG$backup_dir, "audit_fallback.sqlite")
    con <- dbConnect(SQLite(), sqlite_path)
    
    data <- dbGetQuery(con, "SELECT * FROM audit_fallback WHERE hash = ?", 
                      params = list(event_data$hash))
    
    expect_equal(nrow(data), 1)
    expect_equal(data$actor[1], event_data$actor)
    
    dbDisconnect(con)
    cleanup_test_environment()
  })
  
  test_that("emergency file is created as last resort", {
    setup_test_environment()
    
    event_data <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      actor = "emergency_user",
      action = "critical_action",
      payload = '{"emergency": true}',
      hash = "emergency_hash",
      prev_hash = "prev_emergency"
    )
    
    result <- .fallback_level3_emergency_file(event_data)
    
    expect_true(result$success)
    expect_true(file.exists(result$file))
    
    # Prüfe Inhalt
    content <- readLines(result$file)
    expect_true(any(grepl("EMERGENCY AUDIT ENTRY", content)))
    expect_true(any(grepl(event_data$actor, content)))
    expect_true(any(grepl(event_data$hash, content)))
    
    cleanup_test_environment()
  })
  
  test_that("fallback sync recovers unsynced entries", {
    setup_test_environment()
    
    # Erstelle SQLite mit ungesyncten Einträgen
    sqlite_path <- file.path(FALLBACK_CONFIG$backup_dir, "audit_fallback.sqlite")
    con <- dbConnect(SQLite(), sqlite_path)
    
    dbExecute(con, "
      CREATE TABLE audit_fallback (
        id INTEGER PRIMARY KEY,
        timestamp TEXT,
        actor TEXT,
        action TEXT,
        payload TEXT,
        hash TEXT,
        prev_hash TEXT,
        synced BOOLEAN DEFAULT FALSE
      )
    ")
    
    dbExecute(con, "
      INSERT INTO audit_fallback (timestamp, actor, action, payload, hash, prev_hash)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(
      "2024-01-01 12:00:00",
      "sync_test_user",
      "sync_test_action",
      '{"sync_test": true}',
      "sync_hash",
      "prev_sync"
    ))
    
    dbDisconnect(con)
    
    # Mock erfolgreiche Hauptdatenbank
    mock_db_success <- mock(TRUE)
    stub(sync_fallback_entries, ".sync_sqlite_fallback", 
         list(synced = 1, failed = 0))
    
    result <- sync_fallback_entries()
    
    expect_equal(result$synced, 1)
    expect_equal(result$failed, 0)
    
    cleanup_test_environment()
  })
})

# Test Suite 4: Integration Tests
describe("Integration Tests", {
  
  test_that("full audit cycle with DB failure and recovery", {
    setup_test_environment()
    
    # Phase 1: DB ist down
    mock_db_fail <- mock(stop("Database down"), cycle = TRUE)
    stub(audit_append_hashchain, ".audit_write_to_db", mock_db_fail)
    
    # Schreibe Events während DB down ist
    for (i in 1:3) {
      expect_warning(
        audit_append_hashchain(
          actor = sprintf("user_%d", i),
          action = "action_during_outage",
          payload = list(index = i)
        )
      )
    }
    
    # Prüfe dass Events in CSV sind
    csv_data <- read.csv(AUDIT_CSV_PATH, stringsAsFactors = FALSE)
    expect_equal(nrow(csv_data), 3)
    expect_true(all(csv_data$db_sync_status == "failed"))
    
    # Phase 2: DB ist wieder verfügbar
    mock_db_success <- mock(TRUE, cycle = TRUE)
    stub(audit_sync_failed_entries, ".audit_write_to_db", mock_db_success)
    
    # Sync fehlgeschlagene Einträge
    sync_result <- audit_sync_failed_entries(AUDIT_CSV_PATH)
    expect_true(sync_result)
    
    # Prüfe dass Status aktualisiert wurde
    csv_data_after <- read.csv(AUDIT_CSV_PATH, stringsAsFactors = FALSE)
    expect_true(all(csv_data_after$db_sync_status == "success"))
    
    cleanup_test_environment()
  })
  
  test_that("auth system prevents brute force attacks", {
    # Simuliere Brute-Force-Angriff
    attacker_ip <- "10.0.0.1"
    target_username <- "admin"
    
    # Reset environments
    rm(list = ls(.auth_attempts), envir = .auth_attempts)
    rm(list = ls(.blocked_ips), envir = .blocked_ips)
    
    # Versuche Brute-Force
    for (i in 1:10) {
      result <- tryCatch(
        check_rate_limit(target_username, attacker_ip),
        error = function(e) e$message
      )
      
      if (i < AUTH_MAX_ATTEMPTS) {
        expect_true(is.logical(result) && result)
      } else {
        expect_true(is.character(result))
        expect_true(grepl("blocked", result))
        break
      }
    }
    
    # IP sollte geblockt sein
    expect_true(.is_ip_blocked(attacker_ip))
    
    # Weitere Versuche sollten sofort abgelehnt werden
    expect_error(
      check_rate_limit(target_username, attacker_ip),
      regexp = "IP blocked"
    )
  })
})

# Test Suite 5: Performance Tests
describe("Performance Tests", {
  
  test_that("audit system handles high load", {
    setup_test_environment()
    
    # Mock erfolgreiche DB für Performance-Test
    mock_db_fast <- mock(TRUE, cycle = TRUE)
    stub(audit_append_hashchain, ".audit_write_to_db", mock_db_fast)
    
    start_time <- Sys.time()
    
    # Schreibe 100 Audit-Events
    for (i in 1:100) {
      audit_append_hashchain(
        actor = sprintf("perf_user_%d", i %% 10),
        action = "performance_test",
        payload = list(
          index = i,
          timestamp = Sys.time(),
          data = paste(sample(letters, 50, replace = TRUE), collapse = "")
        )
      )
    }
    
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Sollte in angemessener Zeit abschließen (< 10 Sekunden für 100 Events)
    expect_lt(duration, 10)
    
    # Verifiziere Hash-Chain-Integrität
    expect_true(audit_verify_hashchain(AUDIT_CSV_PATH))
    
    # Prüfe dass alle Events geschrieben wurden
    csv_data <- read.csv(AUDIT_CSV_PATH, stringsAsFactors = FALSE)
    expect_equal(nrow(csv_data), 100)
    
    cleanup_test_environment()
  })
  
  test_that("fallback system handles cascade efficiently", {
    setup_test_environment()
    
    # Mock alle Fallback-Level
    stub(audit_fallback_cascade, ".fallback_level1_db_retry",
         list(success = FALSE))
    
    start_time <- Sys.time()
    
    # Teste Fallback-Cascade 10 mal
    for (i in 1:10) {
      event_data <- list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        actor = sprintf("cascade_user_%d", i),
        action = "cascade_test",
        payload = sprintf('{"test":%d}', i),
        hash = sprintf("hash_%d", i),
        prev_hash = sprintf("prev_%d", i)
      )
      
      result <- audit_fallback_cascade(event_data)
      expect_true(result$success)
    }
    
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Cascade sollte schnell sein (< 5 Sekunden für 10 Events)
    expect_lt(duration, 5)
    
    cleanup_test_environment()
  })
})

# Führe alle Tests aus
test_results <- test_dir(".", reporter = "summary")

# Coverage Report (wenn covr installiert ist)
if (requireNamespace("covr", quietly = TRUE)) {
  coverage <- covr::file_coverage(
    source_files = c("../../R/audit.r", 
                     "../../R/audit_fallback.R", 
                     "../../api/plumber_auth.R"),
    test_files = "test-audit-no-silent.R"
  )
  
  print(coverage)
  
  # Prüfe Coverage > 90%
  coverage_percent <- covr::percent_coverage(coverage)
  cat(sprintf("\nCode Coverage: %.1f%%\n", coverage_percent))
  
  if (coverage_percent < 90) {
    warning("Code coverage is below 90%!")
  }
}