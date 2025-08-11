# Audit Error Handling - Dokumentation

## Übersicht

Diese Dokumentation beschreibt die Implementierung eines robusten Error-Handling-Systems für das TDMx PK/PD Audit- und Authentifizierungssystem. Die Hauptziele waren die Eliminierung von "Silent Failures" und die Implementierung mehrschichtiger Fallback-Mechanismen.

## Kritische Probleme (Behoben)

### 1. Silent Failures im Audit-System
- **Problem**: `try(..., silent = TRUE)` unterdrückte alle Datenbankfehler
- **Risiko**: Compliance-relevante Events konnten verloren gehen
- **Lösung**: Explizites Error Handling mit Logging und Fallback-Mechanismen

### 2. Silent Failures in der Authentifizierung
- **Problem**: Auth-Fehler wurden verschluckt
- **Risiko**: Brute-Force-Angriffe blieben unentdeckt
- **Lösung**: Rate Limiting und detailliertes Auth-Logging

## Architektur

### Schichten des Fallback-Systems

```
┌─────────────────────────────────────┐
│     Primär: PostgreSQL Database     │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│   Level 1: DB Retry (3x mit Backoff)│
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│  Level 2: SQLite Lokale Datenbank   │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│    Level 3: JSON-basierte Speicher  │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│   Level 4: Emergency Flat Files     │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│   Level 5: System Journal (Linux)   │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│    Level 6: Remote Syslog Server    │
└────────────┬────────────────────────┘
             │ Fehler
             ▼
┌─────────────────────────────────────┐
│     Level 7: Email Alerts           │
└─────────────────────────────────────┘
```

## Implementierte Komponenten

### 1. R/audit.r - Verbessertes Audit-System

#### Neue Features:
- **Error Logging**: Alle Fehler werden in `log/audit_errors.log` protokolliert
- **DB Sync Status**: CSV enthält `db_sync_status` und `db_sync_error` Spalten
- **Automatisches Fallback**: Bei DB-Ausfall automatischer Wechsel zu Fallback
- **Hash-Chain-Verifikation**: Kontinuierliche Integritätsprüfung

#### Wichtige Funktionen:

```r
# Hauptfunktion mit Error Handling
audit_append_hashchain(
  actor = "username",
  action = "user_action",
  payload = list(data = "value"),
  use_fallback = TRUE  # Aktiviert Fallback-Mechanismen
)

# Sync fehlgeschlagene Einträge
audit_sync_failed_entries()

# Verifiziere Hash-Chain
audit_verify_hashchain()

# Export zu JSON
audit_export_json()
```

### 2. api/plumber_auth.R - Sichere Authentifizierung

#### Neue Features:
- **Rate Limiting**: Max. 5 Versuche in 15 Minuten pro IP/User
- **IP-Blocking**: Automatische Blockierung bei zu vielen Versuchen
- **Detailliertes Logging**: Alle Auth-Events in `log/auth.log`
- **JWT Token**: Sichere Token-basierte Authentifizierung

#### Konfiguration:
```bash
# Umgebungsvariablen
AUTH_MAX_ATTEMPTS=5           # Max. Login-Versuche
AUTH_WINDOW_MINUTES=15        # Zeitfenster für Rate Limiting
AUTH_BLOCK_DURATION=60        # IP-Block-Dauer in Minuten
JWT_SECRET=your-secret-key    # JWT Secret (ÄNDERN!)
```

#### API Endpoints:

```http
POST /auth/token
Content-Type: application/json

{
  "username": "user",
  "password": "pass"
}

Response:
{
  "token": "eyJ...",
  "expires_in": 28800,
  "token_type": "Bearer",
  "role": "admin"
}
```

### 3. R/audit_fallback.R - Fallback-System

#### Hierarchische Fallback-Strategie:

1. **DB Retry**: 3 Versuche mit exponential backoff
2. **SQLite**: Lokale Datenbank als primärer Fallback
3. **JSON Files**: Tagesbasierte JSON-Dateien
4. **Emergency Files**: Einzelne Textdateien pro Event
5. **System Journal**: Linux systemd journal
6. **Remote Syslog**: Netzwerk-basiertes Logging
7. **Email Alerts**: Kritische Benachrichtigungen

#### Verwendung:

```r
# Manueller Fallback-Aufruf
event_data <- list(
  timestamp = Sys.time(),
  actor = "system",
  action = "critical_event",
  payload = '{"important": true}',
  hash = "...",
  prev_hash = "..."
)

result <- audit_fallback_cascade(event_data)

# Sync Fallback-Einträge zurück zur Hauptdatenbank
sync_fallback_entries()

# Cleanup alte Fallback-Dateien
cleanup_old_fallback_files(days_to_keep = 30)
```

## Monitoring und Wartung

### Log-Dateien

| Datei | Zweck | Rotation |
|-------|-------|----------|
| `log/audit_errors.log` | Audit-System-Fehler | Täglich |
| `log/auth.log` | Auth-Events (CSV) | Wöchentlich |
| `log/auth_errors.log` | Auth-System-Fehler | Täglich |
| `log/fallback/fallback_events.log` | Fallback-System-Events | Wöchentlich |

### Monitoring-Empfehlungen

1. **Alerting bei kritischen Fehlern**:
   - Überwache `CRITICAL` Level in Error-Logs
   - Alert bei > 5 fehlgeschlagenen DB-Writes pro Stunde

2. **Rate Limiting Monitoring**:
   - Überwache geblockte IPs
   - Alert bei > 10 Blocks pro Stunde (möglicher Angriff)

3. **Fallback-Status**:
   - Tägliche Prüfung ungesyncte Fallback-Einträge
   - Alert wenn > 100 ungesynct

### Wartungsaufgaben

#### Täglich:
```r
# Sync fehlgeschlagene Audit-Einträge
audit_sync_failed_entries()

# Prüfe Hash-Chain-Integrität
audit_verify_hashchain()
```

#### Wöchentlich:
```r
# Sync Fallback-Datenbank
sync_fallback_entries()

# Export Audit-Log für Archivierung
audit_export_json(output_file = sprintf("archive/audit_%s.json", Sys.Date()))
```

#### Monatlich:
```r
# Cleanup alte Fallback-Dateien
cleanup_old_fallback_files(days_to_keep = 30)

# Rotate Log-Dateien
# (Verwende logrotate auf Linux oder manuelles Archivieren)
```

## Sicherheitsüberlegungen

### 1. Datenschutz
- Keine sensitiven Daten (Passwörter) in Logs
- IP-Adressen werden anonymisiert nach 30 Tagen
- Payload-Daten werden verschlüsselt in Fallback gespeichert

### 2. Compliance
- Vollständige Audit-Trail ohne Lücken
- Hash-Chain garantiert Unveränderlichkeit
- Mehrfache Redundanz verhindert Datenverlust

### 3. Performance
- Asynchrones Logging wo möglich
- Batch-Sync für Fallback-Einträge
- Rate Limiting verhindert DoS

## Testing

### Unit Tests ausführen:
```r
# Führe Test-Suite aus
testthat::test_file("tests/testthat/test-audit-no-silent.R")

# Mit Coverage-Report
covr::file_coverage(
  source_files = c("R/audit.r", "R/audit_fallback.R", "api/plumber_auth.R"),
  test_files = "tests/testthat/test-audit-no-silent.R"
)
```

### Integrationstests:
```r
# Simuliere DB-Ausfall
source("tests/integration/test_db_failure.R")

# Teste Brute-Force-Protection
source("tests/integration/test_brute_force.R")
```

## Migration von alter Version

### Schritt 1: Backup
```bash
# Backup bestehende Audit-Logs
cp -r audit/ audit_backup_$(date +%Y%m%d)/
cp -r log/ log_backup_$(date +%Y%m%d)/
```

### Schritt 2: Update Code
```bash
# Ersetze alte Dateien
cp R/audit.r.new R/audit.r
cp api/plumber_auth.R.new api/plumber_auth.R
cp R/audit_fallback.R R/audit_fallback.R
```

### Schritt 3: Datenbank-Migration
```sql
-- Füge neue Spalten zur audit_log Tabelle hinzu
ALTER TABLE audit_log 
ADD COLUMN retry_attempt INTEGER DEFAULT 0,
ADD COLUMN from_fallback BOOLEAN DEFAULT FALSE;

-- Index für Hash (für ON CONFLICT)
CREATE UNIQUE INDEX IF NOT EXISTS idx_audit_hash ON audit_log(hash);
```

### Schritt 4: Umgebungsvariablen setzen
```bash
export PG_DB="tdmx_audit"
export PG_HOST="localhost"
export PG_PORT="5432"
export PG_USER="audit_user"
export PG_PASS="secure_password"
export JWT_SECRET="your-secret-jwt-key"
export AUTH_MAX_ATTEMPTS="5"
export AUTH_WINDOW_MINUTES="15"
```

### Schritt 5: Initialisierung
```r
# Initialisiere neue Strukturen
source("R/audit.r")
source("R/audit_fallback.R")

.audit_init()
.init_fallback_system()

# Verifiziere bestehende Logs
audit_verify_hashchain()
```

## Troubleshooting

### Problem: "Too many open files" Error
```bash
# Erhöhe File Descriptor Limit
ulimit -n 4096
```

### Problem: SQLite Fallback schlägt fehl
```r
# Prüfe Berechtigungen
file.access("audit/backup", mode = 2)  # Write permission

# Manuell SQLite reparieren
library(RSQLite)
con <- dbConnect(SQLite(), "audit/backup/audit_fallback.sqlite")
dbExecute(con, "VACUUM")
dbDisconnect(con)
```

### Problem: Rate Limiting zu restriktiv
```r
# Temporär Rate Limit zurücksetzen
rm(list = ls(.auth_attempts), envir = .auth_attempts)
rm(list = ls(.blocked_ips), envir = .blocked_ips)
```

## Performance-Optimierung

### 1. Batch-Inserts für Audit
```r
# Sammle Events und schreibe in Batches
audit_batch <- list()
# ... Events sammeln ...
audit_write_batch(audit_batch)
```

### 2. Connection Pooling
```r
# Verwende Connection Pool für DB
library(pool)
pool <- dbPool(
  RPostgres::Postgres(),
  dbname = "tdmx_audit",
  host = "localhost",
  minSize = 1,
  maxSize = 5
)
```

### 3. Async Logging
```r
# Verwende future für asynchrones Logging
library(future)
plan(multisession, workers = 2)

future({
  audit_append_hashchain(...)
})
```

## Kontakt und Support

Bei Fragen oder Problemen:
1. Prüfen Sie zuerst die Log-Dateien
2. Konsultieren Sie diese Dokumentation
3. Erstellen Sie ein Ticket im Issue-Tracker
4. Kontaktieren Sie das Security-Team für kritische Issues

## Changelog

### Version 2.0.0 (2024-01-XX)
- Entfernt alle `silent = TRUE` Vorkommen
- Implementiert hierarchisches Fallback-System
- Fügt Rate Limiting für Authentication hinzu
- Verbessert Error Logging und Monitoring
- Fügt Hash-Chain-Verifikation hinzu
- Implementiert JWT-basierte Authentifizierung

### Version 1.0.0 (Legacy)
- Initiale Implementation mit Silent Failures
- Basis-Audit-System ohne Fallback
- Einfache Passwort-Authentifizierung