# CONTRIBUTING

Danke für deinen Beitrag! Bitte beachte folgende Leitlinien, um Qualität und Reproduzierbarkeit zu sichern.

## Branching & Commits
- **main**: stabil, releasbar.
- **feature/***: pro Feature/Hotfix.
- **Conventional Commits**: `feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:`.

## Code-Style
- **R**: klare Funktionsnamen, keine versteckten side-effects, `lintr`-sauber.  
- **Stan**: Parameter klar benennen, Constraints setzen, Prüfen mit kleinen Testdaten.  
- **Shiny**: Reaktive Ketten klein halten; `req()`/`validate()` nutzen.

## Dateinamen-Konventionen
- **R-Dateien**: ALLE R-Dateien verwenden die Endung `.R` (Großbuchstabe), niemals `.r`
  - Korrekt: `utils.R`, `db.R`, `auth.R`
  - Falsch: `utils.r`, `db.r`, `auth.r`
- **Grund**: Linux-Dateisysteme sind case-sensitive. Inkonsistente Schreibweisen führen zu Fehlern.
- **Prüfung**: Vor jedem Commit prüfen mit:
  ```bash
  # Sollte keine Ausgabe haben:
  find R/ -name "*.r"
  
  # Alle R-Dateien sollten .R verwenden:
  ls -la R/*.R
  ```

## Tests
- **Unit**: `testthat` (PK-Funktionen, Fehler-Modelle, PTA/CFR).  
- **UI**: `shinytest2` für Kern-Flows (Fit → Plot → Report).  
- **Sim-Truth**: synthetische Datensätze mit bekannten Parametern.

## Dokumentation
- PRs aktualisieren relevante **docs/*.md**.  
- **CHANGELOG.md** updaten (Kategorie & kurze Beschreibung).

## Daten & Privacy
- Keine PHI/Daten im Repo.  
- Beispiel-/synthetische Daten verwenden.

## Review-Checkliste
- [ ] Läuft lokal (README Quickstart).  
- [ ] Keine Regressionen (Kern-Flows).  
- [ ] Tests grün.  
- [ ] Doku aktualisiert.  
- [ ] Alle R-Dateien verwenden `.R` Endung (Großbuchstabe).
- [ ] Priors/Targets **nicht** mit Fake-Werten für Produktivsysteme verwechselt.

## Issue-Tags (Vorschlag)
- `kind:bug`, `kind:feature`, `kind:docs`, `kind:infra`, `good first issue`, `help wanted`.

Viel Spaß beim Bauen!