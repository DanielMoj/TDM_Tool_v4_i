# Manuelle Bereinigung der verbleibenden Dateien

## 📝 Noch zu bereinigende Dateien

### 1. R/fhir.R
**Zu entfernen** (ungefähr Zeile 583):
```r
# Null-safe operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
```

**Ersetzen durch Kommentar am Dateianfang**:
```r
# R/fhir.R
# FHIR client implementation
# Requires utils.R to be loaded first for %||% operator
```

### 2. R/fhir_auth.R  
**Zu entfernen** (am Ende der Datei):
```r
# Null-safe operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
```

**Ersetzen durch Kommentar am Dateianfang**:
```r
# R/fhir_auth.R
# FHIR authentication module
# Requires utils.R to be loaded first for %||% operator
```

## 🔧 Durchführung

### Schritt 1: Backup erstellen
```bash
cp R/fhir.R R/fhir.R.backup
cp R/fhir_auth.R R/fhir_auth.R.backup
```

### Schritt 2: Definitionen entfernen
```bash
# Mit sed (Linux/Mac)
sed -i '/%||%.*function/,/^}/d' R/fhir.R
sed -i '/%||%.*function/,/^}/d' R/fhir_auth.R

# Oder manuell in RStudio/Editor:
# 1. Öffne R/fhir.R
# 2. Suche nach "%||%"
# 3. Lösche die komplette Funktionsdefinition (3-4 Zeilen)
# 4. Wiederhole für R/fhir_auth.R
```

### Schritt 3: Kommentare hinzufügen
Am Anfang jeder Datei nach den initialen Kommentaren:
```r
# Requires utils.R to be loaded first for %||% operator
```

### Schritt 4: Validierung
```r
# Test, dass alles funktioniert
source("R/load_all.R")

# Prüfe, dass %||% verfügbar ist
exists("%||%")  # sollte TRUE sein

# Teste die FHIR-Module
test_value <- NULL %||% "default"
print(test_value)  # sollte "default" ausgeben
```

## ✅ Abschluss-Checkliste

- [ ] fhir.R: %||% Definition entfernt
- [ ] fhir.R: Abhängigkeits-Kommentar hinzugefügt  
- [ ] fhir_auth.R: %||% Definition entfernt
- [ ] fhir_auth.R: Abhängigkeits-Kommentar hinzugefügt
- [ ] Tests ausgeführt und bestanden
- [ ] Load-Order getestet
- [ ] Keine Fehler beim Laden der Module

## 🚨 Wichtige Hinweise

1. **Load-Order ist kritisch**: utils.R MUSS vor allen anderen Dateien geladen werden
2. **Keine Conditional Definitions**: Vermeiden Sie `if (!exists("%||%"))` Konstrukte
3. **Zentrale Wartung**: Alle Änderungen am Operator nur in R/utils.R

## 📊 Erwartetes Ergebnis

Nach der Bereinigung sollte:
```bash
grep -r "^[\`']%\|\|%[\`'].*<-.*function" R/ --include="*.R"
```

**Nur EINE Zeile** ausgeben:
```
R/utils.R:`%||%` <- function(x, y) {
```

Alle anderen Definitionen sollten entfernt sein.