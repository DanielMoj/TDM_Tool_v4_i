# TDMx Error Monitoring & System Health - Betriebshandbuch

## 📋 Übersicht

Dieses Handbuch beschreibt das umfassende Error Monitoring und Health Check System für die TDMx PK/PD Anwendung. Das System ermöglicht proaktive Fehlererkennung, automatische Alerts und schnelle Fehlerbehebung (MTTR < 5 Minuten).

## 🏗️ Systemarchitektur

### Komponenten

1. **Error Monitor** (`R/error_monitor.R`)
   - Zentrales Error Tracking
   - Automatische Alert-Generierung
   - Error Pattern Detection
   - Metriken-Aggregation

2. **Health Checks** (`R/health_checks.R`)
   - Komponenten-Überwachung
   - Performance-Metriken
   - Ressourcen-Monitoring
   - Verfügbarkeitsprüfungen

3. **Monitoring Dashboard** (`R/modules/mod_monitoring.R`)
   - Echtzeit-Visualisierung
   - Error Trends
   - System-Status
   - Alert-Management

4. **Integration Tests** (`tests/testthat/test-error-handling-integration.R`)
   - Umfassende Test-Suite
   - Error Recovery Tests
   - Performance Tests
   - Coverage > 95%

## 🚀 Installation & Setup

### 1. Systemvoraussetzungen

```r
# Benötigte Packages
required_packages <- c(
  "DBI", "RPostgres", "pool",
  "httr", "jsonlite", 
  "digest", "lubridate",
  "later", "shiny", "shinydashboard",
  "plotly", "DT"
)

install.packages(required_packages)
```

### 2. Initialisierung

```r
# Error Monitor initialisieren
source("R/error_monitor.R")

# Mit Basis-Konfiguration
init_error_monitor(
  config = list(
    max_errors_in_memory = 1000,
    alert_threshold_critical = 5,
    alert_threshold_warning = 20,
    alert_window_minutes = 5
  )
)

# Mit Datenbank-Persistenz
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("PG_HOST"),
  port = Sys.getenv("PG_PORT"),
  dbname = Sys.getenv("PG_DB"),
  user = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS")
)

init_error_monitor(
  db_connection = conn,
  config = list(
    email_alerts = TRUE,
    slack_webhook = Sys.getenv("SLACK_WEBHOOK")
  )
)
```

### 3. Health Checks aktivieren

```r
source("R/health_checks.R")

# Manuelle Ausführung
results <- run_health_checks(verbose = TRUE)

# Automatische Ausführung (alle 60 Sekunden)
schedule_health_checks(interval_seconds = 60)
```

### 4. Dashboard einbinden

```r
# In der Shiny App (app.R)
ui <- dashboardPage(
  dashboardHeader(title = "TDMx System"),
  dashboardSidebar(
    menuItem("Monitoring", tabName = "monitoring", icon = icon("chart-line"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "monitoring",
        mod_monitoring_ui("monitoring")
      )
    )
  )
)

server <- function(input, output, session) {
  # Monitoring Modul
  monitoring <- mod_monitoring_server("monitoring")
}
```

## 📊 Monitoring Metriken

### Kern-Metriken

| Metrik | Zielwert | Alert-Schwelle | Beschreibung |
|--------|----------|----------------|--------------|
| Error Rate | < 0.1% | > 1% | Fehler pro Minute |
| MTTR | < 5 min | > 15 min | Mean Time To Recovery |
| Uptime | > 99.9% | < 99% | System-Verfügbarkeit |
| Response Time | < 100ms | > 500ms | API/DB Response |
| Memory Usage | < 500MB | > 1GB | R Session Memory |
| Cache Hit Rate | > 80% | < 50% | Cache-Effizienz |

### Component Health Status

- **Healthy** (✓): Alle Checks bestanden
- **Warning** (⚠): Minor Issues, System operational
- **Degraded** (⚡): Performance-Probleme
- **Critical** (✗): Komponente nicht verfügbar

## 🔔 Alert-Konfiguration

### Alert-Level

1. **INFO**: Informative Events (keine Aktion erforderlich)
2. **WARNING**: Potenzielle Probleme (Beobachtung empfohlen)
3. **ERROR**: Fehler aufgetreten (Untersuchung erforderlich)
4. **CRITICAL**: System-kritisch (Sofortige Aktion!)

### Alert-Kanäle

#### Email-Alerts

```r
# Email-Konfiguration (in .Renviron)
SMTP_SERVER=smtp.example.com
SMTP_PORT=587
SMTP_USER=monitoring@example.com
SMTP_PASS=secure_password
ALERT_EMAIL=admin@example.com
```

#### Slack-Integration

```r
# Slack Webhook (in .Renviron)
SLACK_WEBHOOK=https://hooks.slack.com/services/XXX/YYY/ZZZ

# Test Slack Alert
send_slack_alert(list(
  level = "INFO",
  subject = "Test Alert",
  message = "Monitoring system active"
))
```

### Alert-Regeln

```r
# Konfigurierbare Alert-Regeln
alert_rules <- list(
  critical_errors = list(
    condition = "error_count > 5 AND time_window < 5min",
    action = "email + slack",
    escalation = "immediate"
  ),
  
  high_memory = list(
    condition = "memory_usage > 800MB",
    action = "email",
    escalation = "after 3 occurrences"
  ),
  
  db_unavailable = list(
    condition = "db_status == 'critical'",
    action = "email + slack + sms",
    escalation = "immediate"
  )
)
```

## 🔍 Fehleranalyse

### Error Log Struktur

```r
# Error Record Format
error_record <- list(
  id = "ERR_20250810120000_abc123",
  timestamp = "2025-08-10 12:00:00",
  error_type = "DB_CONNECTION_FAILED",
  message = "Unable to connect to database",
  severity = "CRITICAL",
  module = "db",
  context = list(
    host = "localhost",
    port = 5432,
    attempts = 3
  ),
  stack_trace = c("db_connect()", "get_connection()", "init_app()"),
  session_id = "session_123",
  user = "admin",
  environment = "production"
)
```

### Häufige Fehlertypen

| Error Type | Module | Typische Ursache | Lösung |
|------------|--------|------------------|--------|
| DB_CONNECTION_FAILED | db | DB Server down | DB neustarten, Connection Pool prüfen |
| FHIR_TIMEOUT | fhir | Netzwerk/Server | Timeout erhöhen, Server-Status prüfen |
| AUTH_FAILED | auth | Ungültige Credentials | User-Config prüfen, Passwort reset |
| CACHE_FULL | cache | Memory Limit | Cache leeren, Limit erhöhen |
| MODEL_CONVERGENCE | bayes | Numerische Probleme | Priors anpassen, Daten prüfen |

### Error Pattern Detection

```r
# Automatische Erkennung von Error-Mustern
patterns <- detect_error_patterns(
  time_window = minutes(15),
  min_occurrences = 5
)

# Beispiel-Output
list(
  repeated_errors = list(
    "DB_TIMEOUT" = 12,  # 12x in 15 Minuten
    "AUTH_FAILED" = 8   # 8x in 15 Minuten
  ),
  
  affected_modules = list(
    "db" = 15,
    "auth" = 8
  ),
  
  error_correlation = list(
    "DB_TIMEOUT -> API_ERROR" = 0.85  # 85% Korrelation
  )
)
```

## 🛠️ Operatives Management

### Tägliche Aufgaben

```r
# Morning Health Check
daily_health_check <- function() {
  # 1. System Health
  health <- run_health_checks(verbose = TRUE)
  
  # 2. Error Summary (letzte 24h)
  errors <- get_error_stats(time_window = hours(24))
  
  # 3. Performance Metrics
  perf <- get_performance_metrics()
  
  # 4. Cleanup alte Logs
  clear_error_history(older_than = days(7))
  
  # 5. Report generieren
  generate_daily_report(health, errors, perf)
}
```

### Wöchentliche Wartung

```r
# Weekly Maintenance
weekly_maintenance <- function() {
  # 1. Backup Audit Logs
  backup_audit_logs()
  
  # 2. Cache Optimization
  optimize_cache()
  
  # 3. Database Vacuum
  vacuum_database()
  
  # 4. Update Monitoring Thresholds
  update_alert_thresholds()
  
  # 5. Test Disaster Recovery
  test_recovery_procedures()
}
```

### Incident Response

#### Level 1: Warning
1. Dashboard prüfen
2. Betroffene Komponente identifizieren
3. In Monitoring-Log dokumentieren
4. Bei Verschlechterung eskalieren

#### Level 2: Degraded
1. Alert-Empfänger informieren
2. Root Cause Analysis starten
3. Workaround implementieren
4. Fix planen

#### Level 3: Critical
1. **Sofort** Incident Team aktivieren
2. Betroffene Services identifizieren
3. Rollback oder Hotfix
4. Post-Mortem nach Resolution

### Recovery Procedures

```r
# Automatische Recovery-Prozeduren
recovery_procedures <- list(
  
  db_failure = function() {
    # 1. Fallback auf Read-Replica
    switch_to_replica()
    
    # 2. Cache-Mode aktivieren
    enable_cache_only_mode()
    
    # 3. Alert senden
    send_critical_alert("DB Failed - Cache Mode Active")
    
    # 4. Auto-Reconnect starten
    schedule_reconnect(interval = 30)
  },
  
  memory_overflow = function() {
    # 1. Garbage Collection
    gc()
    
    # 2. Cache leeren
    clear_cache(keep_critical = TRUE)
    
    # 3. Non-critical Services stoppen
    stop_background_jobs()
    
    # 4. Memory-Report
    report_memory_usage()
  },
  
  api_overload = function() {
    # 1. Rate Limiting aktivieren
    enable_rate_limiting(requests_per_minute = 100)
    
    # 2. Queue aktivieren
    enable_request_queue()
    
    # 3. Auto-Scaling (falls verfügbar)
    trigger_auto_scaling()
  }
)
```

## 📈 Performance Optimization

### Cache-Strategie

```r
# Cache-Konfiguration für optimale Performance
configure_cache <- function() {
  list(
    ttl = 300,              # 5 Minuten
    max_size = 100,         # Max 100 Einträge
    compression = TRUE,     # Komprimierung aktiv
    strategy = "LRU",       # Least Recently Used
    
    # Cache-Prioritäten
    priorities = list(
      fhir_metadata = "high",
      user_sessions = "high", 
      model_results = "medium",
      static_config = "low"
    )
  )
}
```

### Query-Optimierung

```r
# Datenbank-Query Monitoring
monitor_slow_queries <- function() {
  queries <- get_slow_queries(threshold_ms = 100)
  
  for (query in queries) {
    log_error(
      error_type = "SLOW_QUERY",
      message = sprintf("Query took %dms", query$duration),
      severity = "WARNING",
      module = "db",
      context = list(
        query = query$sql,
        duration = query$duration,
        rows = query$rows_affected
      )
    )
  }
}
```

## 🔐 Sicherheit

### Audit-Log Integrität

```r
# Hash-Chain Verification
verify_audit_integrity <- function() {
  audit_log <- read.csv("audit/audit_log.csv")
  
  valid <- TRUE
  for (i in 2:nrow(audit_log)) {
    if (audit_log$prev_hash[i] != audit_log$hash[i-1]) {
      valid <- FALSE
      log_error(
        error_type = "AUDIT_CHAIN_BROKEN",
        message = sprintf("Hash chain broken at row %d", i),
        severity = "CRITICAL",
        module = "audit"
      )
      break
    }
  }
  
  return(valid)
}

# Tägliche Überprüfung
schedule_daily_audit_check()
```

### Sensitive Data Protection

```r
# Keine sensitiven Daten in Logs
sanitize_error_context <- function(context) {
  sensitive_fields <- c("password", "token", "api_key", "secret")
  
  for (field in sensitive_fields) {
    if (field %in% names(context)) {
      context[[field]] <- "***REDACTED***"
    }
  }
  
  return(context)
}
```

## 📊 KPIs und SLAs

### Service Level Agreements

| Service | Verfügbarkeit | Response Time | Error Rate |
|---------|---------------|---------------|------------|
| Web UI | 99.9% | < 200ms | < 0.1% |
| API | 99.95% | < 100ms | < 0.05% |
| Database | 99.99% | < 50ms | < 0.01% |
| FHIR | 99% | < 500ms | < 1% |

### Key Performance Indicators

```r
# KPI Dashboard Metriken
calculate_kpis <- function(period = "month") {
  list(
    availability = calculate_uptime(period),
    mttr = calculate_mttr(period),
    error_rate = calculate_error_rate(period),
    
    # Business KPIs
    successful_calculations = count_successful_calculations(period),
    avg_calculation_time = mean_calculation_time(period),
    user_sessions = count_unique_sessions(period),
    
    # Technical KPIs
    api_response_time_p50 = quantile(api_times, 0.50),
    api_response_time_p95 = quantile(api_times, 0.95),
    api_response_time_p99 = quantile(api_times, 0.99),
    
    cache_hit_rate = calculate_cache_hit_rate(period),
    db_connection_pool_usage = get_pool_usage()
  )
}
```

## 🚨 Troubleshooting Guide

### Problem: Hohe Error Rate

**Symptome**: Error Rate > 1% für > 5 Minuten

**Diagnose**:
```r
# 1. Error-Typen analysieren
errors <- get_error_stats(time_window = minutes(10))
table(sapply(errors$recent_errors, function(e) e$error_type))

# 2. Betroffene Module identifizieren
table(sapply(errors$recent_errors, function(e) e$module))

# 3. Error Pattern suchen
patterns <- detect_error_patterns()
```

**Lösungen**:
1. Bei DB-Errors: Connection Pool prüfen, DB-Last reduzieren
2. Bei API-Errors: Rate Limiting prüfen, externe Services checken
3. Bei Memory-Errors: GC triggern, Cache leeren

### Problem: Memory Leak

**Symptome**: Stetig steigender Memory-Verbrauch

**Diagnose**:
```r
# Memory Profiling
library(profmem)
prof <- profmem({
  # Verdächtiger Code
})
print(prof)

# Object Sizes
sort(sapply(ls(), function(x) object.size(get(x))), decreasing = TRUE)[1:10]
```

**Lösungen**:
1. Garbage Collection: `gc()`
2. Clear unused objects: `rm(list = setdiff(ls(), keep_objects))`
3. Restart R session if necessary

### Problem: Slow Response Times

**Symptome**: Response Time > 500ms

**Diagnose**:
```r
# Performance Profiling
library(profvis)
profvis({
  # Langsamer Code
})

# Query Analysis
slow_queries <- get_slow_queries()
analyze_query_plans(slow_queries)
```

**Lösungen**:
1. Query-Optimierung (Indizes, Query-Plan)
2. Caching erhöhen
3. Parallelisierung wo möglich

## 📝 Changelog & Updates

### Version 1.0.0 (2025-08-10)
- Initial Release
- Error Monitor implementiert
- Health Checks für alle Komponenten
- Monitoring Dashboard
- Integration Tests > 95% Coverage

### Geplante Features
- [ ] Machine Learning für Anomalie-Erkennung
- [ ] Predictive Alerting
- [ ] Auto-Healing für bekannte Probleme
- [ ] GraphQL Monitoring Interface
- [ ] Distributed Tracing Support

## 📞 Support & Kontakt

### Bei kritischen Problemen:

1. **Stufe 1**: Dashboard prüfen → Dokumentation konsultieren
2. **Stufe 2**: Dev-Team kontaktieren (dev@tdmx.example.com)
3. **Stufe 3**: Eskalation an System-Admin (admin@tdmx.example.com)

### Weitere Ressourcen:

- [API Dokumentation](API.md)
- [Sicherheitsrichtlinien](SECURITY.md)
- [Deployment Guide](DEPLOYMENT.md)
- [Test Strategy](TEST_STRATEGY.md)

---

**Letztes Update**: 2025-08-10  
**Version**: 1.0.0  
**Maintainer**: TDMx Development Team