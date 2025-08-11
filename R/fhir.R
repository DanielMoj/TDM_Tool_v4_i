#' FHIR R Client - Main Functions
#' 
#' @description
#' Production-ready FHIR client with comprehensive error handling,
#' automatic retries, caching, and circuit breaker protection.

# Load dependencies
source("R/fhir_connection.R")
source("R/fhir_auth.R")
source("R/fhir_cache.R")

# Configuration
.fhir_config <- new.env(parent = emptyenv())
.fhir_config$base_url <- Sys.getenv("FHIR_BASE_URL", "")
.fhir_config$default_page_size <- 50
.fhir_config$max_pages <- 10
.fhir_config$timeout <- 30

#' Initialize FHIR client
#' 
#' @param base_url FHIR server base URL
#' @param auth_url OAuth2 token endpoint
#' @param client_id OAuth2 client ID
#' @param client_secret OAuth2 client secret
#' @param cache_enabled Enable response caching
#' @export
init_fhir_client <- function(base_url = NULL,
                            auth_url = NULL,
                            client_id = NULL,
                            client_secret = NULL,
                            cache_enabled = TRUE) {
  
  # Set base URL
  if (!is.null(base_url)) {
    .fhir_config$base_url <- base_url
  } else if (nzchar(Sys.getenv("FHIR_BASE_URL"))) {
    .fhir_config$base_url <- Sys.getenv("FHIR_BASE_URL")
  } else {
    stop("FHIR base URL must be provided or set in FHIR_BASE_URL environment variable")
  }
  
  # Validate URL format
  if (!grepl("^https?://", .fhir_config$base_url)) {
    stop("FHIR base URL must start with http:// or https://")
  }
  
  # Remove trailing slash
  .fhir_config$base_url <- sub("/$", "", .fhir_config$base_url)
  
  # Initialize authentication if credentials provided
  if (!is.null(auth_url) || nzchar(Sys.getenv("FHIR_AUTH_URL"))) {
    init_fhir_auth(
      auth_url = auth_url,
      client_id = client_id,
      client_secret = client_secret
    )
  }
  
  # Configure caching
  if (cache_enabled) {
    configure_fhir_cache(
      default_ttl = 300,
      max_cache_size = 100,
      enable_compression = TRUE
    )
  }
  
  # Test connection
  if (test_fhir_connection()) {
    message(sprintf("FHIR client initialized successfully: %s", .fhir_config$base_url))
    return(TRUE)
  } else {
    warning("FHIR client initialized but connection test failed")
    return(FALSE)
  }
}

#' Test FHIR server connection
#' 
#' @return TRUE if server is reachable, FALSE otherwise
#' @export
test_fhir_connection <- function() {
  tryCatch({
    # Try to fetch server metadata
    response <- fhir_request_with_circuit_breaker(
      request_fn = httr::GET,
      url = sprintf("%s/metadata", .fhir_config$base_url),
      httr::add_headers(
        Accept = "application/fhir+json"
      ),
      max_retries = 1,
      timeout = 10
    )
    
    if (!is.null(response)) {
      content <- httr::content(response, as = "parsed", type = "application/json")
      
      # Verify it's a FHIR server
      if (!is.null(content$resourceType) && content$resourceType == "CapabilityStatement") {
        message(sprintf("Connected to FHIR server: %s", 
                       content$software$name %||% "Unknown"))
        return(TRUE)
      }
    }
    
    return(FALSE)
    
  }, error = function(e) {
    warning(sprintf("FHIR connection test failed: %s", e$message))
    return(FALSE)
  })
}

#' Get observations with full error handling and caching
#' 
#' @param patient_id Patient identifier (required)
#' @param code LOINC or other code for filtering
#' @param date_from Start date for filtering (Date or character)
#' @param date_to End date for filtering (Date or character)
#' @param use_cache Use cached results if available
#' @param include_all Fetch all pages of results
#' @return FHIR Bundle or NULL on error
#' @export
fhir_get_observations <- function(patient_id, 
                                 code = NULL,
                                 date_from = NULL,
                                 date_to = NULL,
                                 use_cache = TRUE,
                                 include_all = FALSE) {
  
  # Input validation
  if (missing(patient_id) || is.null(patient_id) || !nzchar(patient_id)) {
    stop("patient_id is required and must be non-empty")
  }
  
  # Ensure base URL is set
  if (!nzchar(.fhir_config$base_url)) {
    stop("FHIR client not initialized. Run init_fhir_client() first.")
  }
  
  # Build query parameters
  params <- list(
    patient = patient_id,
    `_count` = .fhir_config$default_page_size
  )
  
  if (!is.null(code) && nzchar(code)) {
    params$code <- code
  }
  
  if (!is.null(date_from)) {
    params$date <- sprintf("ge%s", format_fhir_date(date_from))
  }
  
  if (!is.null(date_to)) {
    if (!is.null(date_from)) {
      params$date <- sprintf("%s&date=le%s", params$date, format_fhir_date(date_to))
    } else {
      params$date <- sprintf("le%s", format_fhir_date(date_to))
    }
  }
  
  # Build cache key
  cache_key <- sprintf("obs_%s_%s_%s_%s", 
                      patient_id, 
                      code %||% "all",
                      date_from %||% "any",
                      date_to %||% "any")
  
  # Use cache if enabled
  if (use_cache) {
    return(fhir_cached_request(
      cache_key = cache_key,
      fetch_fn = function() {
        fetch_observations_internal(params, include_all)
      },
      ttl = 300
    ))
  }
  
  # Direct fetch without cache
  return(fetch_observations_internal(params, include_all))
}

#' Internal function to fetch observations
fetch_observations_internal <- function(params, include_all = FALSE) {
  # Build URL with query parameters
  url <- build_fhir_url("/Observation", params)
  
  if (include_all) {
    # Fetch all pages
    return(fhir_fetch_all_pages(url, max_pages = .fhir_config$max_pages))
  } else {
    # Fetch single page
    return(fetch_fhir_bundle(url))
  }
}

#' Get patient demographics
#' 
#' @param patient_id Patient identifier
#' @param use_cache Use cached results if available
#' @return Patient resource or NULL on error
#' @export
fhir_get_patient <- function(patient_id, use_cache = TRUE) {
  
  if (missing(patient_id) || is.null(patient_id) || !nzchar(patient_id)) {
    stop("patient_id is required")
  }
  
  cache_key <- sprintf("patient_%s", patient_id)
  
  if (use_cache) {
    return(fhir_cached_request(
      cache_key = cache_key,
      fetch_fn = function() fetch_patient_internal(patient_id),
      ttl = 3600  # Cache patient data for 1 hour
    ))
  }
  
  return(fetch_patient_internal(patient_id))
}

#' Internal function to fetch patient
fetch_patient_internal <- function(patient_id) {
  url <- sprintf("%s/Patient/%s", .fhir_config$base_url, patient_id)
  
  response <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = url,
    httr::add_headers(
      Authorization = fhir_auth_header(),
      Accept = "application/fhir+json"
    )
  )
  
  if (is.null(response)) {
    return(NULL)
  }
  
  # Parse response
  content <- safe_parse_json(response)
  
  # Validate resource type
  if (!is.null(content) && !is.null(content$resourceType)) {
    if (content$resourceType != "Patient") {
      warning(sprintf("Expected Patient resource, got %s", content$resourceType))
      return(NULL)
    }
  }
  
  return(content)
}

#' Get medication statements
#' 
#' @param patient_id Patient identifier
#' @param status Filter by status (active, completed, etc.)
#' @param use_cache Use cached results if available
#' @return FHIR Bundle or NULL on error
#' @export
fhir_get_medications <- function(patient_id, 
                                status = "active",
                                use_cache = TRUE) {
  
  if (missing(patient_id) || is.null(patient_id)) {
    stop("patient_id is required")
  }
  
  params <- list(
    patient = patient_id,
    `_count` = .fhir_config$default_page_size
  )
  
  if (!is.null(status) && nzchar(status)) {
    params$status <- status
  }
  
  cache_key <- sprintf("meds_%s_%s", patient_id, status %||% "all")
  
  if (use_cache) {
    return(fhir_cached_request(
      cache_key = cache_key,
      fetch_fn = function() fetch_medications_internal(params),
      ttl = 600  # Cache medications for 10 minutes
    ))
  }
  
  return(fetch_medications_internal(params))
}

#' Internal function to fetch medications
fetch_medications_internal <- function(params) {
  url <- build_fhir_url("/MedicationStatement", params)
  return(fetch_fhir_bundle(url))
}

#' Fetch all pages with pagination handling
#' 
#' @param initial_url Starting URL with query parameters
#' @param max_pages Maximum number of pages to fetch
#' @return List of all entries from all pages
#' @export
fhir_fetch_all_pages <- function(initial_url, max_pages = NULL) {
  
  if (!nzchar(initial_url)) {
    stop("initial_url is required")
  }
  
  max_pages <- max_pages %||% .fhir_config$max_pages
  
  results <- list()
  url <- initial_url
  page <- 1
  total_entries <- 0
  
  message("Fetching paginated FHIR results...")
  
  while (!is.null(url) && page <= max_pages) {
    if (getOption("fhir.verbose", FALSE)) {
      message(sprintf("Fetching page %d", page))
    }
    
    # Fetch page
    bundle <- fetch_fhir_bundle(url)
    
    if (is.null(bundle)) {
      warning(sprintf("Failed to fetch page %d, stopping pagination", page))
      break
    }
    
    # Extract entries
    if (!is.null(bundle$entry) && length(bundle$entry) > 0) {
      results <- append(results, bundle$entry)
      total_entries <- total_entries + length(bundle$entry)
    }
    
    # Check total count
    if (!is.null(bundle$total)) {
      if (page == 1) {
        message(sprintf("Total results available: %d", bundle$total))
      }
      
      # Stop if we have all entries
      if (total_entries >= bundle$total) {
        break
      }
    }
    
    # Find next page URL
    url <- extract_next_url(bundle)
    
    # Progress indicator
    if (page %% 5 == 0) {
      message(sprintf("Fetched %d entries so far...", total_entries))
    }
    
    page <- page + 1
  }
  
  if (page > max_pages && !is.null(url)) {
    warning(sprintf("Stopped at max_pages limit (%d). More results available.", max_pages))
  }
  
  message(sprintf("Fetched %d total entries across %d pages", total_entries, page - 1))
  
  # Return as bundle-like structure
  return(list(
    resourceType = "Bundle",
    type = "searchset",
    total = total_entries,
    entry = results
  ))
}

#' Fetch a single FHIR bundle
fetch_fhir_bundle <- function(url) {
  # Make request with circuit breaker
  response <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = url,
    httr::add_headers(
      Authorization = fhir_auth_header(),
      Accept = "application/fhir+json"
    ),
    timeout = .fhir_config$timeout
  )
  
  if (is.null(response)) {
    return(NULL)
  }
  
  # Parse response safely
  content <- safe_parse_json(response)
  
  # Validate FHIR bundle
  if (!is.null(content)) {
    if (is.null(content$resourceType)) {
      warning("Response missing resourceType")
      return(NULL)
    }
    
    if (content$resourceType != "Bundle") {
      warning(sprintf("Expected Bundle, got %s", content$resourceType))
      return(NULL)
    }
    
    # Handle empty results gracefully
    if (is.null(content$entry)) {
      content$entry <- list()
    }
  }
  
  return(content)
}

#' Build FHIR URL with query parameters
build_fhir_url <- function(path, params = NULL) {
  # Ensure path starts with /
  if (!grepl("^/", path)) {
    path <- paste0("/", path)
  }
  
  url <- paste0(.fhir_config$base_url, path)
  
  if (!is.null(params) && length(params) > 0) {
    # Filter out NULL values
    params <- params[!sapply(params, is.null)]
    
    if (length(params) > 0) {
      query_string <- paste(
        mapply(function(key, value) {
          sprintf("%s=%s", key, utils::URLencode(as.character(value)))
        }, names(params), params),
        collapse = "&"
      )
      
      url <- sprintf("%s?%s", url, query_string)
    }
  }
  
  return(url)
}

#' Extract next page URL from bundle
extract_next_url <- function(bundle) {
  if (!is.null(bundle$link)) {
    for (link in bundle$link) {
      if (!is.null(link$relation) && link$relation == "next") {
        return(link$url)
      }
    }
  }
  return(NULL)
}

#' Safely parse JSON response
safe_parse_json <- function(response) {
  tryCatch({
    content <- httr::content(response, as = "parsed", type = "application/json", 
                            encoding = "UTF-8")
    
    # Check for FHIR OperationOutcome (error response)
    if (!is.null(content$resourceType) && content$resourceType == "OperationOutcome") {
      issues <- extract_operation_outcome_issues(content)
      warning(sprintf("FHIR OperationOutcome: %s", paste(issues, collapse = "; ")))
      return(NULL)
    }
    
    return(content)
    
  }, error = function(e) {
    warning(sprintf("Failed to parse FHIR response: %s", e$message))
    
    # Try to get raw content for debugging
    raw_content <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e2) {
      "Unable to retrieve raw content"
    })
    
    if (getOption("fhir.debug", FALSE)) {
      message(sprintf("Raw response: %s", substr(raw_content, 1, 500)))
    }
    
    return(NULL)
  })
}

#' Extract issues from OperationOutcome
extract_operation_outcome_issues <- function(outcome) {
  if (is.null(outcome$issue)) {
    return("Unknown error")
  }
  
  issues <- sapply(outcome$issue, function(issue) {
    severity <- issue$severity %||% "error"
    code <- issue$code %||% "unknown"
    diagnostics <- issue$diagnostics %||% issue$details$text %||% "No details"
    
    sprintf("[%s] %s: %s", severity, code, diagnostics)
  })
  
  return(issues)
}

#' Format date for FHIR queries
format_fhir_date <- function(date) {
  if (is.character(date)) {
    # Assume already in correct format
    return(date)
  } else if (inherits(date, "Date")) {
    return(format(date, "%Y-%m-%d"))
  } else if (inherits(date, "POSIXct") || inherits(date, "POSIXlt")) {
    return(format(date, "%Y-%m-%dT%H:%M:%S"))
  } else {
    stop("Date must be character, Date, or POSIXct/POSIXlt")
  }
}

#' Search for resources with flexible parameters
#' 
#' @param resource_type FHIR resource type (e.g., "Patient", "Observation")
#' @param search_params Named list of search parameters
#' @param use_cache Use cached results
#' @return FHIR Bundle or NULL
#' @export
fhir_search <- function(resource_type, 
                       search_params = list(),
                       use_cache = TRUE) {
  
  if (!nzchar(resource_type)) {
    stop("resource_type is required")
  }
  
  # Add default count if not specified
  if (is.null(search_params$`_count`)) {
    search_params$`_count` <- .fhir_config$default_page_size
  }
  
  # Build cache key from parameters
  cache_key <- sprintf("search_%s_%s", 
                      resource_type,
                      digest::digest(search_params))
  
  if (use_cache) {
    return(fhir_cached_request(
      cache_key = cache_key,
      fetch_fn = function() {
        url <- build_fhir_url(sprintf("/%s", resource_type), search_params)
        fetch_fhir_bundle(url)
      },
      ttl = 300
    ))
  }
  
  url <- build_fhir_url(sprintf("/%s", resource_type), search_params)
  return(fetch_fhir_bundle(url))
}

#' Get FHIR client status
#' 
#' @return List with client status information
#' @export
get_fhir_status <- function() {
  list(
    base_url = .fhir_config$base_url,
    initialized = nzchar(.fhir_config$base_url),
    authenticated = tryCatch({
      !is.null(get_token_info())
    }, error = function(e) FALSE),
    circuit_breaker = get_fhir_circuit_status(),
    cache = get_cache_stats(),
    config = list(
      default_page_size = .fhir_config$default_page_size,
      max_pages = .fhir_config$max_pages,
      timeout = .fhir_config$timeout
    )
  )
}

# Null-safe operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}