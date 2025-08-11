# plumber_auth.R - Authentication API mit Rate Limiting und Error Handling
# Keine Silent Failures mehr - alle Auth-Versuche werden geloggt

library(plumber)
library(jose)
library(digest)
library(jsonlite)

# Globale Environments für Rate Limiting und Session Management
.auth_attempts <- new.env(parent = emptyenv())
.active_sessions <- new.env(parent = emptyenv())
.blocked_ips <- new.env(parent = emptyenv())

# Konfiguration
AUTH_MAX_ATTEMPTS <- as.integer(Sys.getenv("AUTH_MAX_ATTEMPTS", 5))
AUTH_WINDOW_MINUTES <- as.integer(Sys.getenv("AUTH_WINDOW_MINUTES", 15))
AUTH_BLOCK_DURATION_MINUTES <- as.integer(Sys.getenv("AUTH_BLOCK_DURATION", 60))
AUTH_LOG_PATH <- "log/auth.log"
AUTH_ERROR_LOG_PATH <- "log/auth_errors.log"
JWT_SECRET <- Sys.getenv("JWT_SECRET", "CHANGE_THIS_IN_PRODUCTION")

# Initialisierung
.auth_init <- function() {
  dir.create("log", showWarnings = FALSE, recursive = TRUE)
  
  # Initialisiere Log-Dateien
  if (!file.exists(AUTH_LOG_PATH)) {
    cat("timestamp,username,ip,action,success,details\n", file = AUTH_LOG_PATH)
  }
}

# Zentralisierte Auth-Logging-Funktion
.log_auth_event <- function(username, ip, action, success, details = "") {
  log_entry <- sprintf("%s,%s,%s,%s,%s,%s\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      username,
                      ip,
                      action,
                      tolower(as.character(success)),
                      gsub(",", ";", details))  # Escape commas
  
  tryCatch({
    cat(log_entry, file = AUTH_LOG_PATH, append = TRUE)
  }, error = function(e) {
    # Fallback zu stderr wenn Log-Datei nicht schreibbar
    message(sprintf("Auth log write failed: %s", e$message))
    cat(log_entry, file = stderr())
  })
}

# Error-Logging für Auth-System
.log_auth_error <- function(message, severity = "ERROR") {
  error_msg <- sprintf("[%s] %s | %s\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      severity,
                      message)
  
  tryCatch({
    cat(error_msg, file = AUTH_ERROR_LOG_PATH, append = TRUE)
  }, error = function(e) {
    cat(error_msg, file = stderr())
  })
  
  # Bei kritischen Fehlern System-Journal nutzen
  if (severity == "CRITICAL" && Sys.info()["sysname"] == "Linux") {
    try({
      system2("logger", 
              args = c("-p", "auth.err", "-t", "tdmx-auth", message),
              stdout = FALSE, stderr = FALSE)
    }, silent = TRUE)
  }
}

# IP-Blocking-Funktionen
.is_ip_blocked <- function(ip) {
  blocked_until <- .blocked_ips[[ip]]
  
  if (is.null(blocked_until)) {
    return(FALSE)
  }
  
  if (Sys.time() > blocked_until) {
    # Blockierung abgelaufen
    rm(list = ip, envir = .blocked_ips)
    return(FALSE)
  }
  
  return(TRUE)
}

.block_ip <- function(ip, duration_minutes = AUTH_BLOCK_DURATION_MINUTES) {
  blocked_until <- Sys.time() + (duration_minutes * 60)
  .blocked_ips[[ip]] <- blocked_until
  
  .log_auth_error(
    sprintf("IP blocked: %s until %s", ip, format(blocked_until, "%Y-%m-%d %H:%M:%S")),
    severity = "WARNING"
  )
}

# Verbesserte Rate-Limiting-Funktion
check_rate_limit <- function(username, ip) {
  # Prüfe IP-Blockierung zuerst
  if (.is_ip_blocked(ip)) {
    blocked_until <- .blocked_ips[[ip]]
    remaining_minutes <- as.numeric(difftime(blocked_until, Sys.time(), units = "mins"))
    
    .log_auth_event(username, ip, "login_attempt", FALSE, "ip_blocked")
    
    stop(sprintf("IP blocked. Try again in %.0f minutes.", ceiling(remaining_minutes)))
  }
  
  key <- paste0(username, "_", ip)
  current_time <- Sys.time()
  
  # Hole bisherige Attempts
  attempts <- .auth_attempts[[key]]
  if (is.null(attempts)) {
    attempts <- list()
  }
  
  # Entferne alte Attempts (älter als AUTH_WINDOW_MINUTES)
  attempts <- Filter(
    function(x) difftime(current_time, x, units = "mins") < AUTH_WINDOW_MINUTES, 
    attempts
  )
  
  # Prüfe Anzahl der Versuche
  if (length(attempts) >= AUTH_MAX_ATTEMPTS) {
    # Blockiere IP bei zu vielen Versuchen
    .block_ip(ip)
    
    .log_auth_event(username, ip, "rate_limit_exceeded", FALSE, 
                   sprintf("attempts=%d", length(attempts)))
    
    stop(sprintf("Too many login attempts. IP blocked for %d minutes.", 
                AUTH_BLOCK_DURATION_MINUTES))
  }
  
  # Füge neuen Attempt hinzu
  attempts <- c(attempts, list(current_time))
  .auth_attempts[[key]] <- attempts
  
  # Warne bei hoher Anzahl von Versuchen
  if (length(attempts) >= (AUTH_MAX_ATTEMPTS - 1)) {
    .log_auth_error(
      sprintf("Warning: User %s from IP %s has %d/%d attempts", 
              username, ip, length(attempts), AUTH_MAX_ATTEMPTS),
      severity = "WARNING"
    )
  }
  
  return(TRUE)
}

# Reset Rate Limit nach erfolgreichem Login
.reset_rate_limit <- function(username, ip) {
  key <- paste0(username, "_", ip)
  if (exists(key, envir = .auth_attempts)) {
    rm(list = key, envir = .auth_attempts)
  }
}

# Verbesserte Auth-Check-Funktion
auth_check <- function(username, password) {
  # Hier würde normalerweise die echte Authentifizierung stattfinden
  # z.B. LDAP, Active Directory, oder Datenbank-Check
  
  tryCatch({
    # Simulierte Auth-Prüfung (ersetzen Sie mit echter Implementierung)
    # Beispiel: Prüfung gegen Datenbank
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PG_DB", "tdmx_users"),
      host = Sys.getenv("PG_HOST", "localhost"),
      port = as.integer(Sys.getenv("PG_PORT", 5432)),
      user = Sys.getenv("PG_USER", "auth_user"),
      password = Sys.getenv("PG_PASS", "")
    )
    
    # Prepared Statement gegen SQL Injection
    query <- "SELECT user_id, password_hash, salt, role, active 
              FROM users 
              WHERE username = $1"
    
    result <- DBI::dbGetQuery(con, query, params = list(username))
    DBI::dbDisconnect(con)
    
    if (nrow(result) == 0) {
      return(list(success = FALSE, reason = "user_not_found"))
    }
    
    if (!result$active[1]) {
      return(list(success = FALSE, reason = "account_disabled"))
    }
    
    # Verifiziere Passwort
    password_hash <- digest(paste0(password, result$salt[1]), algo = "sha256")
    
    if (password_hash != result$password_hash[1]) {
      return(list(success = FALSE, reason = "invalid_password"))
    }
    
    return(list(
      success = TRUE, 
      user_id = result$user_id[1],
      role = result$role[1]
    ))
    
  }, error = function(e) {
    .log_auth_error(
      sprintf("Auth check failed for user %s: %s", username, e$message),
      severity = "ERROR"
    )
    
    # Wichtig: Nicht den echten Fehler an Client weitergeben
    return(list(success = FALSE, reason = "auth_system_error"))
  })
}

# JWT Token Generation
.generate_jwt_token <- function(user_id, username, role) {
  # Token-Payload
  payload <- list(
    sub = as.character(user_id),
    username = username,
    role = role,
    iat = as.numeric(Sys.time()),
    exp = as.numeric(Sys.time() + (60 * 60 * 8))  # 8 Stunden Gültigkeit
  )
  
  # Generiere Token
  token <- jwt_encode_hmac(
    claim = payload,
    secret = JWT_SECRET,
    algo = "HS256"
  )
  
  return(token)
}

# Session Management
.create_session <- function(user_id, username, ip) {
  session_id <- digest(paste(user_id, username, ip, Sys.time(), runif(1)), algo = "sha256")
  
  session_data <- list(
    user_id = user_id,
    username = username,
    ip = ip,
    created_at = Sys.time(),
    last_activity = Sys.time()
  )
  
  .active_sessions[[session_id]] <- session_data
  
  return(session_id)
}

# Plumber API Endpoints
#* @apiTitle TDMx Authentication API
#* @apiDescription Secure authentication system with rate limiting

#* Health Check
#* @get /health
function() {
  list(
    status = "healthy",
    timestamp = Sys.time(),
    version = "1.0.0"
  )
}

#* Login Endpoint
#* @post /auth/token
#* @param username:str Username
#* @param password:str Password
function(req, res, username = "", password = "") {
  # Initialisierung
  .auth_init()
  
  # Extrahiere IP-Adresse
  ip <- req$HTTP_X_FORWARDED_FOR
  if (is.null(ip)) {
    ip <- req$REMOTE_ADDR
  }
  if (is.null(ip)) {
    ip <- "unknown"
  }
  
  # Input-Validierung
  if (nchar(username) == 0 || nchar(password) == 0) {
    .log_auth_event(username, ip, "login_attempt", FALSE, "missing_credentials")
    res$status <- 400
    return(list(error = "Username and password required"))
  }
  
  # Username-Validierung (Schutz vor Injection)
  if (!grepl("^[a-zA-Z0-9._-]+$", username)) {
    .log_auth_event(username, ip, "login_attempt", FALSE, "invalid_username_format")
    res$status <- 400
    return(list(error = "Invalid username format"))
  }
  
  # Rate Limiting Check mit Error Handling
  rate_limit_result <- tryCatch({
    check_rate_limit(username, ip)
  }, error = function(e) {
    res$status <- 429  # Too Many Requests
    return(list(error = e$message))
  })
  
  if (!is.logical(rate_limit_result)) {
    return(rate_limit_result)  # Fehler von Rate Limiting
  }
  
  # Auth Check mit verbessertem Error Handling
  auth_result <- tryCatch({
    auth_check(username, password)
  }, error = function(e) {
    # Log detaillierten Fehler intern
    .log_auth_error(
      sprintf("Authentication error for %s from %s: %s", username, ip, e$message),
      severity = "ERROR"
    )
    
    # Log Event
    .log_auth_event(username, ip, "login_attempt", FALSE, "system_error")
    
    # Gebe generischen Fehler an Client
    res$status <- 500
    return(list(error = "Authentication system error"))
  })
  
  # Prüfe Auth-Ergebnis
  if (!auth_result$success) {
    # Log fehlgeschlagenen Versuch
    .log_auth_event(username, ip, "login_attempt", FALSE, auth_result$reason)
    
    # Spezifische Fehlerbehandlung
    if (auth_result$reason == "account_disabled") {
      res$status <- 403
      return(list(error = "Account disabled"))
    }
    
    # Generische Fehlermeldung für Sicherheit
    res$status <- 401
    return(list(error = "Invalid credentials"))
  }
  
  # Erfolgreiche Authentifizierung
  .reset_rate_limit(username, ip)
  
  # Generiere JWT Token
  token <- tryCatch({
    .generate_jwt_token(auth_result$user_id, username, auth_result$role)
  }, error = function(e) {
    .log_auth_error(
      sprintf("Token generation failed for %s: %s", username, e$message),
      severity = "CRITICAL"
    )
    res$status <- 500
    return(list(error = "Token generation failed"))
  })
  
  # Erstelle Session
  session_id <- .create_session(auth_result$user_id, username, ip)
  
  # Log erfolgreichen Login
  .log_auth_event(username, ip, "login_success", TRUE, 
                 sprintf("role=%s,session=%s", auth_result$role, substr(session_id, 1, 8)))
  
  # Audit-Log für erfolgreichen Login
  source("R/audit.r")
  audit_append_hashchain(
    actor = username,
    action = "user_login",
    payload = list(
      ip = ip,
      role = auth_result$role,
      session_id = substr(session_id, 1, 8)  # Nur Prefix loggen
    )
  )
  
  return(list(
    token = token,
    expires_in = 28800,  # 8 Stunden
    token_type = "Bearer",
    role = auth_result$role
  ))
}

#* Logout Endpoint
#* @post /auth/logout
#* @param token:str JWT Token
function(req, res, token = "") {
  if (nchar(token) == 0) {
    # Prüfe Authorization Header
    auth_header <- req$HTTP_AUTHORIZATION
    if (!is.null(auth_header) && grepl("^Bearer ", auth_header)) {
      token <- sub("^Bearer ", "", auth_header)
    }
  }
  
  if (nchar(token) == 0) {
    res$status <- 400
    return(list(error = "Token required"))
  }
  
  # Verifiziere und decode Token
  payload <- tryCatch({
    jwt_decode_hmac(token, secret = JWT_SECRET)
  }, error = function(e) {
    res$status <- 401
    return(list(error = "Invalid token"))
  })
  
  if (!is.list(payload)) {
    return(payload)  # Fehler
  }
  
  # Log Logout
  ip <- req$HTTP_X_FORWARDED_FOR
  if (is.null(ip)) ip <- req$REMOTE_ADDR
  if (is.null(ip)) ip <- "unknown"
  
  .log_auth_event(payload$username, ip, "logout", TRUE, "")
  
  # Audit-Log
  source("R/audit.r")
  audit_append_hashchain(
    actor = payload$username,
    action = "user_logout",
    payload = list(ip = ip)
  )
  
  return(list(message = "Logout successful"))
}

#* Verify Token Endpoint
#* @get /auth/verify
function(req, res) {
  auth_header <- req$HTTP_AUTHORIZATION
  
  if (is.null(auth_header) || !grepl("^Bearer ", auth_header)) {
    res$status <- 401
    return(list(error = "Authorization header required"))
  }
  
  token <- sub("^Bearer ", "", auth_header)
  
  # Verifiziere Token
  payload <- tryCatch({
    jwt_decode_hmac(token, secret = JWT_SECRET)
  }, error = function(e) {
    .log_auth_error(
      sprintf("Token verification failed: %s", e$message),
      severity = "WARNING"
    )
    res$status <- 401
    return(list(error = "Invalid token"))
  })
  
  if (!is.list(payload)) {
    return(payload)
  }
  
  # Prüfe Ablaufzeit
  if (payload$exp < as.numeric(Sys.time())) {
    res$status <- 401
    return(list(error = "Token expired"))
  }
  
  return(list(
    valid = TRUE,
    username = payload$username,
    role = payload$role,
    expires_at = as.POSIXct(payload$exp, origin = "1970-01-01")
  ))
}