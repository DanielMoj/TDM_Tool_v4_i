# R/db.R
# Database functions for the PK/PD Shiny application
# RENAMED FROM db.r to db.R for Linux compatibility

# Database connection configuration
get_db_config <- function() {
  list(
    host = Sys.getenv("PG_HOST", "localhost"),
    port = as.integer(Sys.getenv("PG_PORT", "5432")),
    database = Sys.getenv("PG_DATABASE", "tdmx"),
    user = Sys.getenv("PG_USER", "tdmx"),
    password = Sys.getenv("PG_PASSWORD", "tdmx")
  )
}

# Connect to PostgreSQL
connect_pg <- function() {
  config <- get_db_config()
  
  tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = config$host,
      port = config$port,
      dbname = config$database,
      user = config$user,
      password = config$password
    )
    con
  }, error = function(e) {
    warning(sprintf("Database connection failed: %s", e$message))
    NULL
  })
}

# Execute with connection management
with_db_connection <- function(expr, transactional = FALSE) {
  con <- connect_pg()
  if (is.null(con)) {
    warning("No database connection available")
    return(NULL)
  }
  
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  if (transactional) {
    DBI::dbBegin(con)
    result <- tryCatch({
      res <- eval(expr)
      DBI::dbCommit(con)
      res
    }, error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    })
  } else {
    result <- eval(expr)
  }
  
  result
}

# Import antibiogram data with versioning
db_import_antibiogram <- function(df, source, version = NULL) {
  # Require digest package
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' required for checksums")
  }
  
  with_db_connection({
    # Normalize per drug (ensure sum(prob)=1)
    df <- dplyr::group_by(df, drug) |> 
          dplyr::mutate(prob = prob / sum(prob)) |> 
          dplyr::ungroup()
    
    # Add source column
    df$source <- source
    
    # Insert rows - dbWriteTable handles parameterization internally
    DBI::dbWriteTable(con, "antibiogram", df, append = TRUE, row.names = FALSE)
    
    # Version entry
    v <- version %||% format(Sys.time(), "%Y%m%d%H%M%S")
    checksum <- digest::digest(df, algo = "sha256")
    meta <- list(
      source = source, 
      rows = nrow(df), 
      drugs = length(unique(df$drug))
    )
    
    # Write version record
    sql <- "INSERT INTO dataset_versions(kind, version, checksum, meta) VALUES ($1,$2,$3,$4)"
    DBI::dbExecute(con, sql, params = list(
      "antibiogram",
      v,
      checksum,
      jsonlite::toJSON(meta, auto_unbox = TRUE)
    ))
    
    message(sprintf("Imported %d antibiogram rows for %d drugs", nrow(df), meta$drugs))
    list(version = v, checksum = checksum, rows = nrow(df))
  }, transactional = TRUE)  # Use transaction for atomicity
}

# SECURITY: Fixed SQL injection - uses parameterized query
db_get_antibiogram <- function(drug = NULL) {
  with_db_connection({
    if (is.null(drug)) {
      df <- DBI::dbGetQuery(con, "SELECT * FROM antibiogram ORDER BY drug, mic")
    } else {
      # SECURITY: Parameterized query prevents SQL injection
      sql <- "SELECT * FROM antibiogram WHERE drug = $1 ORDER BY mic"
      df <- DBI::dbGetQuery(con, sql, params = list(drug))
    }
    df
  })
}

# List available drugs in antibiogram
db_list_antibiogram_drugs <- function() {
  with_db_connection({
    df <- DBI::dbGetQuery(con, "SELECT DISTINCT drug FROM antibiogram ORDER BY drug")
    if (!is.null(df) && nrow(df) > 0) df$drug else character(0)
  })
}

# Get latest dataset version
db_get_latest_version <- function(kind) {
  with_db_connection({
    sql <- "SELECT * FROM dataset_versions WHERE kind = $1 ORDER BY created_at DESC LIMIT 1"
    df <- DBI::dbGetQuery(con, sql, params = list(kind))
    if (nrow(df) > 0) df[1,] else NULL
  })
}

# Check if antibiogram data exists
db_has_antibiogram_data <- function() {
  with_db_connection({
    df <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM antibiogram")
    !is.null(df) && df$n[1] > 0
  })
}

# Clean up old antibiogram data (keep only latest version per drug)
db_cleanup_antibiogram <- function() {
  with_db_connection({
    # Get latest version
    latest <- DBI::dbGetQuery(con, 
      "SELECT MAX(created_at) as latest FROM antibiogram"
    )
    
    if (!is.null(latest) && !is.na(latest$latest[1])) {
      # Delete older entries (keep last 24 hours)
      cutoff <- as.POSIXct(latest$latest[1]) - 86400  # 24 hours
      sql <- "DELETE FROM antibiogram WHERE created_at < $1"
      deleted <- DBI::dbExecute(con, sql, params = list(cutoff))
      message(sprintf("Cleaned up %d old antibiogram entries", deleted))
      deleted
    } else {
      0
    }
  }, transactional = TRUE)
}

# Helper function for testing DB connection
db_test_connection <- function() {
  con <- connect_pg()
  if (is.null(con) || inherits(con, "try-error")) {
    return(FALSE)
  }
  
  # Try a simple query
  result <- tryCatch({
    DBI::dbGetQuery(con, "SELECT 1 as test")
    TRUE
  }, error = function(e) {
    FALSE
  }, finally = {
    try(DBI::dbDisconnect(con), silent = TRUE)
  })
  
  result
}