# COMPUTATION_STABILITY - Berechnungsstabilität für Bayes'sche Inferenz

## Übersicht

Diese Dokumentation beschreibt die implementierten Mechanismen zur Stabilisierung der Bayes'schen Berechnungen (Stan HMC und JAGS) im TDMx System. Die Implementierung stellt sicher, dass Berechnungsfehler die Anwendung nicht zum Absturz bringen und bietet automatische Recovery-Mechanismen.

## Architektur

### Hauptkomponenten

1. **`R/safe_computation.R`** - Zentrale Wrapper-Funktion für sichere Berechnungen
2. **`R/backend_bayes.R`** - Modifizierte Stan-Implementierung mit Error Handling
3. **`R/run_fit_jags.R`** - Vollständig abgesicherte JAGS-Implementierung

### Schichtenmodell

```
┌─────────────────────────────────┐
│        Shiny UI Layer           │
├─────────────────────────────────┤
│    safe_bayesian_computation    │ ← Wrapper mit Progress & Recovery
├─────────────────────────────────┤
│   Stan HMC │ JAGS │ Laplace     │ ← Backend-Implementierungen
├─────────────────────────────────┤
│     Error Handling Layer        │ ← Diagnostics & Recovery
└─────────────────────────────────┘
```

## Features

### 1. Automatische Error Recovery

#### Stan Divergent Transitions
```r
# Automatische Erhöhung von adapt_delta bei Divergenzen
if (divergences_detected && adapt_delta < 0.95) {
  retry_with_adapt_delta = 0.95
  retry_with_max_treedepth = 15
}
```

#### JAGS Initialisierungsfehler
```r
# Automatische Generierung alternativer Startwerte
if (initialization_failed) {
  generate_custom_initial_values()
  retry_compilation()
}
```

### 2. Memory Management

- **Pre-flight Check**: Verfügbarer Speicher wird vor Berechnungsstart geprüft
- **Adaptive Reduction**: Bei Speicherproblemen automatische Reduktion von:
  - Anzahl der Iterationen
  - Anzahl der Chains
  - Erhöhung des Thinning-Intervalls

### 3. Timeout Protection

- **Stan**: 5 Minuten Timeout (konfigurierbar)
- **JAGS**: 3 Minuten Timeout (konfigurierbar)
- **Graceful Degradation**: Bei Timeout wird auf schnellere Methoden gewechselt

### 4. Progress Tracking

```r
# Echtzeit-Updates für Shiny UI
progress$set(value = 20, message = "Initializing model...")
progress$set(value = 50, message = "Running MCMC chains...")
progress$set(value = 80, message = "Processing results...")
```

### 5. Comprehensive Diagnostics

#### Stan Diagnostics
- Divergent transitions
- Max treedepth hits
- E-BFMI (Energy Bayesian Fraction of Missing Information)
- R-hat und ESS (Effective Sample Size)
- Mean acceptance probability

#### JAGS Diagnostics
- Gelman-Rubin statistic (R-hat)
- Effective sample size
- Heidelberger-Welch test
- Raftery-Lewis diagnostic
- DIC (Deviance Information Criterion)

## Verwendung

### Basic Usage

```r
# Sichere Stan-Berechnung
result <- safe_bayesian_computation(
  computation_type = "stan",
  computation_fn = function(params) {
    # Stan sampling code
  },
  params = list(
    obs = observations,
    regimen = dosing_regimen,
    adapt_delta = 0.8
  ),
  session = shiny::getDefaultReactiveDomain()
)

if (result$success) {
  # Verarbeite Ergebnisse
  draws <- result$result
  diagnostics <- result$diagnostics
} else {
  # Handle Fehler mit informativer Nachricht
  showNotification(result$error, type = "error")
}
```

### Advanced Configuration

```r
# HMC Controls für Stan
options(tdmx_hmc = list(
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  adapt_delta = 0.95,
  max_treedepth = 15,
  parallel_chains = 4
))

# JAGS Configuration
config <- list(
  n_chains = 3,
  n_iter = 10000,
  n_burnin = 2000,
  thin = 2,
  error_model = "combined"
)
```

## Error Messages und Lösungsvorschläge

### Häufige Fehler und automatische Lösungen

| Fehler | Automatische Aktion | Benutzer-Vorschlag |
|--------|-------------------|-------------------|
| Divergent transitions | Retry mit adapt_delta=0.95 | "Consider reparameterizing model" |
| Memory allocation failed | Reduziere chains/iterations | "Close other applications" |
| Initial values invalid | Generiere neue Startwerte | "Check prior specifications" |
| Timeout exceeded | - | "Reduce model complexity" |
| Compilation failed | - | "Check Stan/JAGS installation" |

### Diagnose-Schwellwerte

```r
# Konvergenz-Kriterien
MAX_RHAT = 1.01        # Maximaler R-hat für Konvergenz
MIN_ESS_BULK = 400     # Minimale bulk ESS
MIN_ESS_TAIL = 400     # Minimale tail ESS
MAX_DIVERGENCES = 0    # Idealerweise keine Divergenzen
EBFMI_THRESHOLD = 0.3  # Energy diagnostic
```

## Testing

### Unit Tests

```bash
# Alle Stabilitäts-Tests ausführen
Rscript -e "testthat::test_file('tests/testthat/test-computation-stability.R')"
```

### Integration Tests

```r
# Test mit problematischen Daten
test_problematic_data <- function() {
  data <- create_extreme_data()  # Extreme Werte
  result <- run_fit_with_safety(data)
  expect_true(result$handled_gracefully)
}
```

### Stress Tests

```r
# Memory stress test
test_memory_limits <- function() {
  large_data <- create_large_dataset(n = 100000)
  result <- run_fit_jags(large_data, config = high_iteration_config)
  expect_true("memory" %in% result$warnings || result$success)
}
```

## Performance Considerations

### Caching

- **Stan Model Compilation**: Modelle werden gecacht und wiederverwendet
- **Key**: Basiert auf Dateiname und Änderungszeit
- **Invalidierung**: Automatisch bei Modelländerungen

### Parallelisierung

```r
# Stan parallel chains
parallel_chains = min(parallel::detectCores() - 1, n_chains)

# JAGS (aktuell sequenziell, parallel möglich mit:)
# - rjags::parallel.seeds()
# - doParallel backend
```

### Optimierungen

1. **Warm-Start**: Verwendung vorheriger Ergebnisse als Startwerte
2. **Adaptive Sampling**: Mehr Samples nur bei schlechter Konvergenz
3. **Early Stopping**: Abbruch bei offensichtlichen Problemen

## Monitoring und Logging

### Audit Trail

```r
# Automatisches Logging aller Berechnungen
audit_event(
  action = "stan_computation",
  payload = list(
    success = TRUE,
    duration = 45.3,
    divergences = 0,
    max_rhat = 1.005
  )
)
```

### Metriken

- **Erfolgsrate**: Prozentsatz erfolgreicher Berechnungen
- **Recovery-Rate**: Erfolgreiche Wiederholungen nach Fehler
- **Durchschnittliche Berechnungszeit**
- **Häufigste Fehlertypen**

## Best Practices

### 1. Immer mit Wrapper arbeiten

```r
# ✓ Gut - verwendet safe_bayesian_computation
result <- safe_bayesian_computation(...)

# ✗ Schlecht - direkter Aufruf ohne Schutz
fit <- mod$sample(...)
```

### 2. Diagnostics prüfen

```r
# Immer Diagnostics auswerten
if (result$diagnostics$divergences > 0) {
  warning("Model may be misspecified")
}
```

### 3. Progress Feedback geben

```r
# Benutzer informiert halten
computation_fn <- function(params) {
  updateProgress("Starting warmup...")
  # ... computation ...
  updateProgress("Sampling...")
}
```

### 4. Fallback-Strategien

```r
# Mehrstufige Fallback-Strategie
try_stan_hmc() %||%
try_stan_advi() %||%
try_laplace_approximation()
```

## Troubleshooting

### Debug-Modus aktivieren

```r
# Verbose output für Debugging
options(tdmx_debug = TRUE)
Sys.setenv(STAN_VERBOSE = TRUE)
```

### Häufige Probleme

1. **"Cannot allocate vector of size..."**
   - Lösung: Reduziere chains oder iterations
   - Alternative: Verwende ADVI statt HMC

2. **"Initialization failed after 100 attempts"**
   - Lösung: Prüfe Priors auf Plausibilität
   - Alternative: Manuelle init-Werte setzen

3. **"47 divergent transitions"**
   - Lösung: Erhöhe adapt_delta (automatisch)
   - Alternative: Reparametrisiere Modell

## Weiterentwicklung

### Geplante Verbesserungen

1. **Adaptive Grid Refinement** für JAGS ODE-Solver
2. **Parallel JAGS Chains** mit doParallel
3. **Intelligent Restart** mit gelernten Parametern
4. **GPU-Acceleration** für Stan (OpenCL)
5. **Automatic Model Selection** bei wiederholten Fehlern

### Contribution Guidelines

Bei Erweiterungen der Error-Handling-Mechanismen:

1. Neue Fehlertypen in `check_computation_problems()` ergänzen
2. Recovery-Strategie in `safe_bayesian_computation()` implementieren
3. Tests in `test-computation-stability.R` hinzufügen
4. Dokumentation aktualisieren

## Referenzen

- [Stan Best Practices](https://mc-stan.org/docs/2_28/stan-users-guide/index.html)
- [JAGS User Manual](https://sourceforge.net/projects/mcmc-jags/files/Manuals/)
- [Bayesian Workflow](https://arxiv.org/abs/2011.01808)
- [HMC Diagnostics](https://mc-stan.org/docs/2_28/reference-manual/hmc-chapter.html)