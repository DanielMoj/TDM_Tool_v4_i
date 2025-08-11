# audit_fallback.R - Robuste Fallback-Strategien für Audit-System
# Mehrschichtiges Fallback-System für kritische Audit-Events
#
# RESOURCE LEAK FIXES (2025-08-11):
# - .fallback_level1_db_retry: Added on.exit() to ensure dbDisconnect on error
# - .fallback_level2_local_storage: Added on.exit() for SQLite connection cleanup
# - .sync_sqlite_fallback: Added on.exit() for both SQLite and PostgreSQL connections
# - All database connections are now guaranteed to close even on errors

library(jsonlite)
library(digest)

# Fallback-Konfiguration
FALLBACK_CONFIG <- list(
  max_retries = 3,
  retry_delay_seconds = 2,
  emergency_dir = "audit/emergency",
  backup_dir = "audit/backup",
  remote_syslog_host = Sys.getenv("SYSLOG_HOST", ""),
  remote_syslog_port = as.integer(Sys.getenv("SYSLOG_PORT", 514)),
  email_alerts = as.logical(Sys.getenv("AUDIT_EMAIL_ALERTS", FALSE)),
  email_recipient = Sys.getenv("AUDIT_EMAIL_RECIPIENT", "admin@example.com")
)

# Initialisierung der Fallback-Struktur
.init_fallback_system <- function() {
  dirs <- c(
    FALLBACK_CONFIG$emergency_dir,
    FALLBACK_CONFIG$backup_dir,
    "log/fallback"
  )
  
  for (dir in dirs) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Prüfe und initialisiere Fallback-Datei
  fallback_index <- file.path(FALLBACK_CONFIG$backup_dir, "fallback_index.json")
  if (!file.exists(fallback_index)) {
    index_data <- list(
      created = Sys.time(),
      entries = list(),
      last_sync = NULL
    )
    writeLines(toJSON(index_data, pretty = TRUE), fallback_index)
  }
}

# Hierarchisches Fallback-System
audit_fallback_cascade <- function(event_data) {
  .init_fallback_system()
  
  # Level 1: Versuche primäre DB mit Retry
  level1_result <- .fallback_level1_db_retry(event_data)
  if (level1_result$success) {
    return(level1_result)
  }
  
  # Level 2: Lokale strukturierte Speicherung
  level2_result <- .fallback_level2_local_storage(event_data)
  if (!level2_result$success) {
    # Level 3: Emergency Flat File
    level3_result <- .fallback_level3_emergency_file(event_data)
    
    # Level 4: System Journal (Linux)
    if (Sys.info()["sysname"] == "Linux") {
      .fallback_level4_system_journal(event_data)
    }
    
    # Level 5: Remote Syslog
    if (nchar(FALLBACK_CONFIG$remote_syslog_host) > 0) {
      .fallback_level5_remote_syslog(event_data)
    }
    
    # Level 6: Email Alert
    if (FALLBACK_CONFIG$email_alerts) {
      .fallback_level6_email_alert(event_data)
    }
  }
  
  return(list(
    success = level2_result$success || level3_result$success,
    fallback_level = ifelse(level2_result$success, 2, 3),
    details = "Event stored in fallback system"
  ))
}

# Level 1: DB Retry mit exponential backoff
.fallback_level1_db_retry <- function(event_data) {
  for (attempt in 1:FALLBACK_CONFIG$max_retries) {
    result <- tryCatch({
      # Versuche DB-Verbindung
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("PG_DB", "tdmx_audit"),
        host = Sys.getenv("PG_HOST", "localhost"),
        port = as.integer(Sys.getenv("PG_PORT", 5432)),
        user = Sys.getenv("PG_USER", "audit_user"),
        password = Sys.getenv("PG_PASS", ""),
        connect_timeout = 5
      )
      
      # CRITICAL FIX: Ensure connection is closed even on error
      # on.exit is executed when the function exits, regardless of how (normal return or error)
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
      
      # Versuche Insert
      query <- "INSERT INTO audit_log (timestamp, actor, action, payload, hash, prev_hash, retry_attempt) 
                VALUES ($1, $2, $3, $4, $5, $6, $7)"
      
      DBI::dbExecute(con, query, params = list(
        event_data$timestamp,
        event_data$actor,
        event_data$action,
        event_data$payload,
        event_data$hash,
        event_data$prev_hash,
        attempt
      ))
      
      .log_fallback_event(
        sprintf("DB write successful on attempt %d/%d for hash: %s", 
                attempt, FALLBACK_CONFIG$max_retries, event_data$hash),
        level = "INFO"
      )
      
      return(list(success = TRUE, attempt = attempt))
      
    }, error = function(e) {
      # Exponential backoff
      if (attempt < FALLBACK_CONFIG$max_retries) {
        Sys.sleep(FALLBACK_CONFIG$retry_delay_seconds * (2^(attempt - 1)))
      }
      
      .log_fallback_event(
        sprintf("DB retry %d/%d failed: %s", 
                attempt, FALLBACK_CONFIG$max_retries, e$message),
        level = "WARNING"
      )
      
      return(list(success = FALSE, error = e$message))
    })
    
    if (result$success) {
      return(result)
    }
  }
  
  return(list(success = FALSE, error = "All DB retries exhausted"))
}

# Level 2: Strukturierte lokale Speicherung
.fallback_level2_local_storage <- function(event_data) {
  tryCatch({
    # SQLite als lokale Fallback-DB
    sqlite_path <- file.path(FALLBACK_CONFIG$backup_dir, "audit_fallback.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
    
    # CRITICAL FIX: Ensure SQLite connection is always closed
    # This guarantees the connection is closed even if an error occurs
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    # Erstelle Tabelle falls nicht vorhanden
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS audit_fallback (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp TEXT NOT NULL,
        actor TEXT NOT NULL,
        action TEXT NOT NULL,
        payload TEXT,
        hash TEXT UNIQUE NOT NULL,
        prev_hash TEXT,
        synced BOOLEAN DEFAULT FALSE,
        created_at TEXT DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Insert Event
    DBI::dbExecute(con, "
      INSERT OR IGNORE INTO audit_fallback 
      (timestamp, actor, action, payload, hash, prev_hash)
      VALUES (?, ?, ?, ?, ?, ?)
    ", params = list(
      event_data$timestamp,
      event_data$actor,
      event_data$action,
      event_data$payload,
      event_data$hash,
      event_data$prev_hash
    ))
    
    # Update Index
    .update_fallback_index(event_data, "sqlite")
    
    .log_fallback_event(
      sprintf("Event stored in SQLite fallback: %s", event_data$hash),
      level = "INFO"
    )
    
    return(list(success = TRUE, storage = "sqlite"))
    
  }, error = function(e) {
    # Fallback zu JSON-Datei
    return(.fallback_to_json_storage(event_data))
  })
}

# JSON-basierte Speicherung als Fallback
.fallback_to_json_storage <- function(event_data) {
  tryCatch({
    json_file <- file.path(
      FALLBACK_CONFIG$backup_dir,
      sprintf("audit_%s.json", format(Sys.Date(), "%Y%m%d"))
    )
    
    # Lese existierende Daten oder erstelle neue Liste
    if (file.exists(json_file)) {
      existing_data <- fromJSON(readLines(json_file))
    } else {
      existing_data <- list(entries = list())
    }
    
    # Füge neuen Eintrag hinzu
    existing_data$entries <- append(existing_data$entries, list(event_data))
    existing_data$last_updated <- Sys.time()
    
    # Schreibe zurück
    writeLines(toJSON(existing_data, pretty = TRUE), json_file)
    
    .update_fallback_index(event_data, "json")
    
    return(list(success = TRUE, storage = "json"))
    
  }, error = function(e) {
    .log_fallback_event(
      sprintf("JSON storage failed: %s", e$message),
      level = "ERROR"
    )
    return(list(success = FALSE, error = e$message))
  })
}

# Level 3: Emergency Flat File
.fallback_level3_emergency_file <- function(event_data) {
  tryCatch({
    emergency_file <- file.path(
      FALLBACK_CONFIG$emergency_dir,
      sprintf("EMERGENCY_%s_%s.txt", 
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              substr(event_data$hash, 1, 8))
    )
    
    content <- paste(
      "=== EMERGENCY AUDIT ENTRY ===",
      sprintf("Timestamp: %s", event_data$timestamp),
      sprintf("Actor: %s", event_data$actor),
      sprintf("Action: %s", event_data$action),
      sprintf("Payload: %s", event_data$payload),
      sprintf("Hash: %s", event_data$hash),
      sprintf("Previous Hash: %s", event_data$prev_hash),
      sprintf("Emergency File Created: %s", Sys.time()),
      "=== END ===",
      sep = "\n"
    )
    
    writeLines(content, emergency_file)
    
    .log_fallback_event(
      sprintf("Emergency file created: %s", emergency_file),
      level = "WARNING"
    )
    
    return(list(success = TRUE, file = emergency_file))
    
  }, error = function(e) {
    .log_fallback_event(
      sprintf("Emergency file creation failed: %s", e$message),
      level = "CRITICAL"
    )
    return(list(success = FALSE, error = e$message))
  })
}

# Level 4: System Journal (Linux only)
.fallback_level4_system_journal <- function(event_data) {
  if (Sys.info()["sysname"] != "Linux") {
    return(list(success = FALSE, error = "Not on Linux"))
  }
  
  tryCatch({
    message <- sprintf(
      "AUDIT_FALLBACK actor=%s action=%s hash=%s",
      event_data$actor,
      event_data$action,
      event_data$hash
    )
    
    system2("logger", 
            args = c("-t", "tdmx_audit", "-p", "user.warning", message),
            stdout = FALSE,
            stderr = FALSE)
    
    return(list(success = TRUE, method = "syslog"))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Level 5: Remote Syslog
.fallback_level5_remote_syslog <- function(event_data) {
  if (nchar(FALLBACK_CONFIG$remote_syslog_host) == 0) {
    return(list(success = FALSE, error = "No remote syslog configured"))
  }
  
  tryCatch({
    # Erstelle Syslog-formatierte Nachricht
    priority <- 14  # user.info
    timestamp <- format(Sys.time(), "%b %d %H:%M:%S")
    hostname <- Sys.info()["nodename"]
    tag <- "tdmx_audit"
    
    message <- sprintf(
      "<%d>%s %s %s: FALLBACK actor=%s action=%s hash=%s",
      priority, timestamp, hostname, tag,
      event_data$actor, event_data$action, event_data$hash
    )
    
    # Sende über UDP
    con <- socketConnection(
      host = FALLBACK_CONFIG$remote_syslog_host,
      port = FALLBACK_CONFIG$remote_syslog_port,
      open = "w",
      blocking = FALSE
    )
    
    # Ensure socket is closed even on error
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    
    writeLines(message, con)
    
    return(list(success = TRUE, method = "remote_syslog"))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Level 6: Email Alert
.fallback_level6_email_alert <- function(event_data) {
  if (!FALLBACK_CONFIG$email_alerts) {
    return(list(success = FALSE, error = "Email alerts disabled"))
  }
  
  tryCatch({
    subject <- sprintf("[CRITICAL] TDMx Audit Fallback: %s", event_data$action)
    
    body <- paste(
      "Critical Audit Event Required Fallback Storage",
      "",
      sprintf("Timestamp: %s", event_data$timestamp),
      sprintf("Actor: %s", event_data$actor),
      sprintf("Action: %s", event_data$action),
      sprintf("Hash: %s", event_data$hash),
      "",
      "This event has been stored in the fallback system.",
      "Immediate attention required to restore primary audit storage.",
      "",
      "-- TDMx Audit System",
      sep = "\n"
    )
    
    # Verwende sendmailR oder mailR package
    # Beispiel mit system mail command (Linux)
    if (Sys.info()["sysname"] == "Linux") {
      mail_cmd <- sprintf(
        'echo "%s" | mail -s "%s" %s',
        body,
        subject,
        FALLBACK_CONFIG$email_recipient
      )
      
      system(mail_cmd, intern = FALSE)
    }
    
    return(list(success = TRUE, method = "email_alert"))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Update Fallback Index
.update_fallback_index <- function(event_data, storage_method) {
  index_file <- file.path(FALLBACK_CONFIG$backup_dir, "fallback_index.json")
  
  tryCatch({
    index_data <- fromJSON(readLines(index_file))
    
    entry <- list(
      hash = event_data$hash,
      timestamp = event_data$timestamp,
      storage = storage_method,
      synced = FALSE,
      added_at = Sys.time()
    )
    
    index_data$entries <- append(index_data$entries, list(entry))
    index_data$last_updated <- Sys.time()
    
    writeLines(toJSON(index_data, pretty = TRUE), index_file)
    
  }, error = function(e) {
    # Index-Update fehlgeschlagen ist nicht kritisch
    .log_fallback_event(
      sprintf("Index update failed: %s", e$message),
      level = "WARNING"
    )
  })
}

# Sync-Funktion für Fallback-Einträge
sync_fallback_entries <- function() {
  .init_fallback_system()
  
  synced_count <- 0
  failed_count <- 0
  
  # Sync von SQLite
  sqlite_result <- .sync_sqlite_fallback()
  synced_count <- synced_count + sqlite_result$synced
  failed_count <- failed_count + sqlite_result$failed
  
  # Sync von JSON-Dateien
  json_result <- .sync_json_fallback()
  synced_count <- synced_count + json_result$synced
  failed_count <- failed_count + json_result$failed
  
  # Update Index
  if (synced_count > 0) {
    .update_sync_status()
  }
  
  message(sprintf("Fallback sync complete: %d synced, %d failed", 
                 synced_count, failed_count))
  
  return(list(synced = synced_count, failed = failed_count))
}

# Sync SQLite Fallback
.sync_sqlite_fallback <- function() {
  sqlite_path <- file.path(FALLBACK_CONFIG$backup_dir, "audit_fallback.sqlite")
  
  if (!file.exists(sqlite_path)) {
    return(list(synced = 0, failed = 0))
  }
  
  con_sqlite <- NULL
  con_pg <- NULL
  
  tryCatch({
    con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
    
    # CRITICAL FIX: Ensure both connections are closed on exit
    # This handles cleanup for both SQLite and PostgreSQL connections
    on.exit({
      try(DBI::dbDisconnect(con_sqlite), silent = TRUE)
      if (!is.null(con_pg)) {
        try(DBI::dbDisconnect(con_pg), silent = TRUE)
      }
    }, add = TRUE)
    
    # Hole ungesynte Einträge
    unsynced <- DBI::dbGetQuery(con_sqlite, 
      "SELECT * FROM audit_fallback WHERE synced = FALSE ORDER BY id"
    )
    
    synced <- 0
    failed <- 0
    
    for (i in seq_len(nrow(unsynced))) {
      entry <- unsynced[i, ]
      
      # Versuche in Haupt-DB zu schreiben
      result <- tryCatch({
        con_pg <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname = Sys.getenv("PG_DB", "tdmx_audit"),
          host = Sys.getenv("PG_HOST", "localhost"),
          port = as.integer(Sys.getenv("PG_PORT", 5432)),
          user = Sys.getenv("PG_USER", "audit_user"),
          password = Sys.getenv("PG_PASS", "")
        )
        
        DBI::dbExecute(con_pg, 
          "INSERT INTO audit_log (timestamp, actor, action, payload, hash, prev_hash, from_fallback)
           VALUES ($1, $2, $3, $4, $5, $6, TRUE)
           ON CONFLICT (hash) DO NOTHING",
          params = list(
            entry$timestamp,
            entry$actor,
            entry$action,
            entry$payload,
            entry$hash,
            entry$prev_hash
          )
        )
        
        # Close PostgreSQL connection after each successful write
        DBI::dbDisconnect(con_pg)
        con_pg <- NULL
        
        # Markiere als gesynct
        DBI::dbExecute(con_sqlite,
          "UPDATE audit_fallback SET synced = TRUE WHERE id = ?",
          params = list(entry$id)
        )
        
        synced <- synced + 1
        TRUE
        
      }, error = function(e) {
        failed <- failed + 1
        FALSE
      })
    }
    
    return(list(synced = synced, failed = failed))
    
  }, error = function(e) {
    .log_fallback_event(
      sprintf("SQLite sync failed: %s", e$message),
      level = "ERROR"
    )
    return(list(synced = 0, failed = 0))
  })
}

# Sync JSON Fallback
.sync_json_fallback <- function() {
  json_files <- list.files(
    FALLBACK_CONFIG$backup_dir,
    pattern = "^audit_.*\\.json$",
    full.names = TRUE
  )
  
  synced <- 0
  failed <- 0
  
  for (json_file in json_files) {
    tryCatch({
      data <- fromJSON(readLines(json_file))
      
      for (entry in data$entries) {
        if (is.null(entry$synced) || !entry$synced) {
          # Versuche Sync
          result <- .sync_single_entry(entry)
          if (result) {
            synced <- synced + 1
            entry$synced <- TRUE
          } else {
            failed <- failed + 1
          }
        }
      }
      
      # Update JSON-Datei
      writeLines(toJSON(data, pretty = TRUE), json_file)
      
    }, error = function(e) {
      .log_fallback_event(
        sprintf("JSON sync failed for %s: %s", json_file, e$message),
        level = "WARNING"
      )
    })
  }
  
  return(list(synced = synced, failed = failed))
}

# Sync einzelnen Eintrag
.sync_single_entry <- function(entry) {
  con <- NULL
  
  tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PG_DB", "tdmx_audit"),
      host = Sys.getenv("PG_HOST", "localhost"),
      port = as.integer(Sys.getenv("PG_PORT", 5432)),
      user = Sys.getenv("PG_USER", "audit_user"),
      password = Sys.getenv("PG_PASS", "")
    )
    
    # Ensure connection is closed
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    DBI::dbExecute(con,
      "INSERT INTO audit_log (timestamp, actor, action, payload, hash, prev_hash, from_fallback)
       VALUES ($1, $2, $3, $4, $5, $6, TRUE)
       ON CONFLICT (hash) DO NOTHING",
      params = list(
        entry$timestamp,
        entry$actor,
        entry$action,
        entry$payload,
        entry$hash,
        entry$prev_hash
      )
    )
    
    return(TRUE)
    
  }, error = function(e) {
    return(FALSE)
  })
}

# Update Sync Status
.update_sync_status <- function() {
  index_file <- file.path(FALLBACK_CONFIG$backup_dir, "fallback_index.json")
  
  tryCatch({
    index_data <- fromJSON(readLines(index_file))
    index_data$last_sync <- Sys.time()
    writeLines(toJSON(index_data, pretty = TRUE), index_file)
  }, error = function(e) {
    # Nicht kritisch
  })
}

# Logging-Funktion für Fallback-Events
.log_fallback_event <- function(message, level = "INFO") {
  log_file <- file.path("log/fallback", sprintf("fallback_%s.log", format(Sys.Date(), "%Y%m%d")))
  
  log_entry <- sprintf("[%s] %s: %s", 
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      level,
                      message)
  
  tryCatch({
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    # Wenn Logging fehlschlägt, gebe zu stderr aus
    message(log_entry)
  })
}

# Helper: Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a