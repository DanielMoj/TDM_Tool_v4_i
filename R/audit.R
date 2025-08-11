# audit.r - Audit System mit verbessertem Error Handling
# Keine Silent Failures mehr - alle Fehler werden geloggt und behandelt

library(digest)
library(jsonlite)

# Globale Konfiguration
AUDIT_CSV_PATH <- "audit/audit_log.csv"
AUDIT_ERROR_LOG <- "log/audit_errors.log"
AUDIT_FALLBACK_PATH <- "audit/audit_fallback.csv"

# Initialisiere Audit-System
.audit_init <- function() {
  # Stelle sicher, dass alle Verzeichnisse existieren
  dir.create("audit", showWarnings = FALSE, recursive = TRUE)
  dir.create("log", showWarnings = FALSE, recursive = TRUE)
  
  # Initialisiere CSV falls nicht vorhanden
  if (!file.exists(AUDIT_CSV_PATH)) {
    df <- data.frame(
      timestamp = character(),
      actor = character(),
      action = character(),
      payload = character(),
      hash = character(),
      prev_hash = character(),
      db_sync_status = character(),
      db_sync_error = character(),
      stringsAsFactors = FALSE
    )
    write.csv(df, AUDIT_CSV_PATH, row.names = FALSE)
  }
}

# Verbesserte DB-Schreibfunktion mit Error Handling
.audit_write_to_db <- function(ts, actor, action, payload_json, prev_hash, hash) {
  # Simulierte DB-Funktion - ersetzen Sie mit Ihrer echten PostgreSQL-Verbindung
  con <- NULL
  
  tryCatch({
    # Verbindung zur PostgreSQL
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PG_DB", "tdmx_audit"),
      host = Sys.getenv("PG_HOST", "localhost"),
      port = as.integer(Sys.getenv("PG_PORT", 5432)),
      user = Sys.getenv("PG_USER", "audit_user"),
      password = Sys.getenv("PG_PASS", "")
    )
    
    # SQL Insert
    query <- "INSERT INTO audit_log (timestamp, actor, action, payload, hash, prev_hash) 
              VALUES ($1, $2, $3, $4, $5, $6)"
    
    DBI::dbExecute(con, query, params = list(ts, actor, action, payload_json, hash, prev_hash))
    
    return(TRUE)
    
  }, error = function(e) {
    # Logge den Fehler, aber werfe ihn weiter
    .log_audit_error(
      sprintf("DB Write Failed - Actor: %s, Action: %s, Error: %s", 
              actor, action, e$message)
    )
    stop(e)
  }, finally = {
    if (!is.null(con)) {
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  })
}

# Zentralisierte Error-Logging-Funktion
.log_audit_error <- function(message, severity = "ERROR") {
  error_msg <- sprintf("[%s] %s | %s\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      severity,
                      message)
  
  # Schreibe in Error-Log
  cat(error_msg, file = AUDIT_ERROR_LOG, append = TRUE)
  
  # Bei kritischen Fehlern auch System-Journal nutzen (Linux)
  if (severity == "CRITICAL" && Sys.info()["sysname"] == "Linux") {
    try({
      system2("logger", 
              args = c("-p", "user.err", "-t", "tdmx-audit", message),
              stdout = FALSE, stderr = FALSE)
    }, silent = TRUE)
  }
}

# Hauptfunktion zum Anhängen an die Hash-Chain
audit_append_hashchain <- function(file = AUDIT_CSV_PATH, 
                                  actor, 
                                  action, 
                                  payload = list(),
                                  use_fallback = TRUE) {
  
  # Initialisierung
  .audit_init()
  
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  payload_json <- toJSON(payload, auto_unbox = TRUE)
  
  # Lese bestehende Audit-Daten
  df <- NULL
  prev_hash <- "GENESIS"
  
  if (file.exists(file) && file.info(file)$size > 0) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    if (nrow(df) > 0) {
      prev_hash <- df$hash[nrow(df)]
    }
  }
  
  # Berechne Hash
  h <- digest(paste(ts, actor, action, payload_json, prev_hash, sep = "|"), algo = "sha256")
  
  # Versuche DB-Schreibvorgang mit Error Handling
  db_success <- FALSE
  db_error <- ""
  
  db_result <- tryCatch({
    .audit_write_to_db(ts, actor, action, payload_json, prev_hash, h)
    db_success <- TRUE
    TRUE
  }, error = function(e) {
    # Detailliertes Error Logging
    error_msg <- sprintf("AUDIT_DB_FAIL | Actor: %s | Action: %s | Error: %s",
                        actor, action, e$message)
    .log_audit_error(error_msg, severity = "ERROR")
    
    # System Warning für Monitoring
    warning(sprintf("Audit DB write failed for action '%s': %s", action, e$message))
    
    db_error <- substr(e$message, 1, 100)  # Truncate für CSV
    
    # Aktiviere Fallback-Mechanismus
    if (use_fallback) {
      .audit_fallback_write(ts, actor, action, payload_json, prev_hash, h)
    }
    
    FALSE
  })
  
  # Schreibe immer in CSV (primärer Fallback)
  new_row <- data.frame(
    timestamp = ts,
    actor = actor,
    action = action,
    payload = as.character(payload_json),
    hash = h,
    prev_hash = prev_hash,
    db_sync_status = ifelse(db_success, "success", "failed"),
    db_sync_error = db_error,
    stringsAsFactors = FALSE
  )
  
  if (is.null(df)) {
    df <- new_row
  } else {
    df <- rbind(df, new_row)
  }
  
  # Schreibe CSV mit Error Handling
  csv_result <- tryCatch({
    write.csv(df, file, row.names = FALSE)
    TRUE
  }, error = function(e) {
    .log_audit_error(
      sprintf("CRITICAL - CSV Write Failed - Actor: %s, Action: %s, Error: %s",
              actor, action, e$message),
      severity = "CRITICAL"
    )
    
    # Letzter Fallback: Schreibe in separates Fallback-File
    .audit_emergency_write(ts, actor, action, payload_json, h, prev_hash)
    
    FALSE
  })
  
  # Verifiziere Hash-Chain-Integrität
  if (csv_result) {
    verify_result <- tryCatch({
      audit_verify_hashchain(file)
    }, error = function(e) {
      .log_audit_error(
        sprintf("Hash chain verification failed: %s", e$message),
        severity = "WARNING"
      )
      FALSE
    })
    
    if (!verify_result) {
      .log_audit_error("Hash chain integrity check failed", severity = "WARNING")
    }
  }
  
  # Rückgabe mit Status-Information
  return(list(
    success = csv_result,
    db_synced = db_success,
    hash = h,
    timestamp = ts
  ))
}

# Fallback-Mechanismus für kritische Audit-Events
.audit_fallback_write <- function(ts, actor, action, payload_json, prev_hash, hash) {
  fallback_entry <- sprintf("%s|%s|%s|%s|%s|%s\n",
                           ts, actor, action, 
                           gsub("\n", " ", payload_json),  # Remove newlines
                           hash, prev_hash)
  
  tryCatch({
    cat(fallback_entry, file = AUDIT_FALLBACK_PATH, append = TRUE)
    .log_audit_error(
      sprintf("Audit entry written to fallback: %s - %s", actor, action),
      severity = "WARNING"
    )
  }, error = function(e) {
    .log_audit_error(
      sprintf("Fallback write failed: %s", e$message),
      severity = "CRITICAL"
    )
  })
}

# Emergency Write - Letzter Ausweg
.audit_emergency_write <- function(ts, actor, action, payload_json, hash, prev_hash) {
  emergency_file <- sprintf("audit/emergency_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  emergency_content <- paste(
    "EMERGENCY AUDIT ENTRY",
    paste("Timestamp:", ts),
    paste("Actor:", actor),
    paste("Action:", action),
    paste("Payload:", payload_json),
    paste("Hash:", hash),
    paste("Previous Hash:", prev_hash),
    sep = "\n"
  )
  
  tryCatch({
    writeLines(emergency_content, emergency_file)
    .log_audit_error(
      sprintf("EMERGENCY: Audit written to %s", emergency_file),
      severity = "CRITICAL"
    )
  }, error = function(e) {
    # Absolute letzter Versuch: stdout
    cat("EMERGENCY AUDIT:", emergency_content, "\n")
  })
}

# Hash-Chain-Verifikation
audit_verify_hashchain <- function(file = AUDIT_CSV_PATH) {
  if (!file.exists(file)) {
    stop("Audit file does not exist")
  }
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  if (nrow(df) == 0) {
    return(TRUE)  # Leere Chain ist valide
  }
  
  # Verifiziere ersten Eintrag
  if (df$prev_hash[1] != "GENESIS") {
    return(FALSE)
  }
  
  # Verifiziere Chain
  for (i in 1:nrow(df)) {
    expected_hash <- digest(
      paste(df$timestamp[i], df$actor[i], df$action[i], 
            df$payload[i], df$prev_hash[i], sep = "|"), 
      algo = "sha256"
    )
    
    if (expected_hash != df$hash[i]) {
      .log_audit_error(
        sprintf("Hash mismatch at row %d: expected %s, got %s", 
                i, expected_hash, df$hash[i]),
        severity = "ERROR"
      )
      return(FALSE)
    }
    
    # Verifiziere Chain-Verbindung
    if (i < nrow(df) && df$prev_hash[i + 1] != df$hash[i]) {
      .log_audit_error(
        sprintf("Chain broken between rows %d and %d", i, i + 1),
        severity = "ERROR"
      )
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Sync-Funktion für fehlgeschlagene DB-Writes
audit_sync_failed_entries <- function(file = AUDIT_CSV_PATH) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  failed_entries <- df[df$db_sync_status == "failed", ]
  
  if (nrow(failed_entries) == 0) {
    message("No failed entries to sync")
    return(TRUE)
  }
  
  synced_count <- 0
  
  for (i in 1:nrow(failed_entries)) {
    entry <- failed_entries[i, ]
    
    result <- tryCatch({
      .audit_write_to_db(
        entry$timestamp, 
        entry$actor, 
        entry$action, 
        entry$payload, 
        entry$prev_hash, 
        entry$hash
      )
      
      # Update Status in DataFrame
      df[df$hash == entry$hash, "db_sync_status"] <- "success"
      df[df$hash == entry$hash, "db_sync_error"] <- ""
      synced_count <- synced_count + 1
      
      TRUE
    }, error = function(e) {
      .log_audit_error(
        sprintf("Sync failed for entry %s: %s", entry$hash, e$message),
        severity = "WARNING"
      )
      FALSE
    })
  }
  
  # Speichere aktualisierten DataFrame
  if (synced_count > 0) {
    write.csv(df, file, row.names = FALSE)
    message(sprintf("Successfully synced %d of %d failed entries", 
                   synced_count, nrow(failed_entries)))
  }
  
  return(synced_count == nrow(failed_entries))
}

# Export-Funktionen
audit_export_json <- function(file = AUDIT_CSV_PATH, output_file = NULL) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  if (is.null(output_file)) {
    output_file <- sprintf("audit/export_%s.json", 
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  # Konvertiere zu JSON mit voller Struktur
  audit_list <- lapply(1:nrow(df), function(i) {
    list(
      timestamp = df$timestamp[i],
      actor = df$actor[i],
      action = df$action[i],
      payload = fromJSON(df$payload[i]),
      hash = df$hash[i],
      prev_hash = df$prev_hash[i],
      db_sync_status = df$db_sync_status[i],
      verified = TRUE  # Nur exportieren wenn verifiziert
    )
  })
  
  # Verifiziere vor Export
  if (!audit_verify_hashchain(file)) {
    warning("Hash chain verification failed - export may contain compromised data")
    for (i in 1:length(audit_list)) {
      audit_list[[i]]$verified <- FALSE
    }
  }
  
  writeLines(toJSON(audit_list, pretty = TRUE), output_file)
  message(sprintf("Audit log exported to %s", output_file))
  
  return(output_file)
}