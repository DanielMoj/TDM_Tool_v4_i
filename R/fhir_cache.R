# R/fhir_cache.R
# FHIR Response Caching Layer
# Requires utils.R to be loaded first for %||% operator
# 
# @description
# Implements intelligent caching for FHIR responses to:
# - Reduce server load and network traffic
# - Improve response times
# - Handle offline scenarios gracefully
# - Support cache invalidation strategies

# Cache storage environments
.fhir_cache <- new.env(parent = emptyenv())
.fhir_cache_stats <- new.env(parent = emptyenv())
.fhir_cache_config <- list(
  default_ttl = 300,           # 5 minutes default
  max_cache_size = 100,        # Maximum number of cached items
  max_cache_memory = 50 * 1024^2,  # 50 MB max memory
  enable_compression = TRUE,    # Compress large responses
  enable_stats = TRUE,          # Track cache statistics
  stale_while_revalidate = 60  # Serve stale content for 60s while revalidating
)

# Initialize cache statistics
.fhir_cache_stats$hits <- 0
.fhir_cache_stats$misses <- 0
.fhir_cache_stats$evictions <- 0
.fhir_cache_stats$revalidations <- 0

#' Get cached FHIR response or fetch new
#' 
#' @param cache_key Unique identifier for this request
#' @param fetch_fn Function to fetch data if not cached
#' @param ttl Time-to-live in seconds (default 300 = 5 minutes)
#' @param force_refresh Force cache bypass
#' @param use_stale_on_error Return stale data if fetch fails
#' @return Cached or fresh data
#' @export
fhir_cached_request <- function(cache_key, 
                               fetch_fn, 
                               ttl = NULL,
                               force_refresh = FALSE,
                               use_stale_on_error = TRUE) {
  
  # Validate inputs
  if (!is.character(cache_key) || !nzchar(cache_key)) {
    stop("cache_key must be a non-empty string")
  }
  
  if (!is.function(fetch_fn)) {
    stop("fetch_fn must be a function")
  }
  
  ttl <- ttl %||% .fhir_cache_config$default_ttl
  
  # Force refresh bypasses cache
  if (force_refresh) {
    message("FHIR: Force refresh requested for key: ", cache_key)
    data <- fetch_fn()
    if (!is.null(data)) {
      set_cache_entry(cache_key, data, ttl)
    }
    return(data)
  }
  
  # Check cache
  cached <- get_cache_entry(cache_key)
  
  if (!is.null(cached)) {
    # Check if expired
    if (cached$expires_at > Sys.time()) {
      update_cache_stats("hits")
      if (getOption("fhir.cache.verbose", FALSE)) {
        message("FHIR cache HIT: ", cache_key)
      }
      return(cached$data)
    }
    
    # Expired but within stale-while-revalidate window
    stale_window <- cached$expires_at + .fhir_cache_config$stale_while_revalidate
    if (stale_window > Sys.time()) {
      # Return stale data and trigger async revalidation
      update_cache_stats("revalidations")
      if (getOption("fhir.cache.verbose", FALSE)) {
        message("FHIR: Serving stale data while revalidating: ", cache_key)
      }
      
      # Async revalidation (simplified - in production use promises/future)
      tryCatch({
        new_data <- fetch_fn()
        if (!is.null(new_data)) {
          set_cache_entry(cache_key, new_data, ttl)
        }
      }, error = function(e) {
        message("FHIR: Revalidation failed: ", e$message)
      })
      
      return(cached$data)
    }
  }
  
  # Cache miss - fetch new data
  update_cache_stats("misses")
  if (getOption("fhir.cache.verbose", FALSE)) {
    message("FHIR cache MISS: ", cache_key)
  }
  
  # Fetch with error handling
  data <- tryCatch({
    fetch_fn()
  }, error = function(e) {
    message("FHIR: Fetch failed: ", e$message)
    
    # Return stale data if available and flag set
    if (use_stale_on_error && !is.null(cached)) {
      message("FHIR: Returning stale data due to fetch error")
      return(cached$data)
    }
    
    stop(e)
  })
  
  # Store in cache
  if (!is.null(data)) {
    set_cache_entry(cache_key, data, ttl)
  }
  
  return(data)
}

#' Set cache entry
set_cache_entry <- function(key, data, ttl) {
  # Calculate size
  size <- object.size(data)
  
  # Compress if large and enabled
  if (.fhir_cache_config$enable_compression && size > 10240) {  # > 10KB
    data <- compress_data(data)
    compressed <- TRUE
  } else {
    compressed <- FALSE
  }
  
  # Store entry
  .fhir_cache[[key]] <- list(
    data = data,
    cached_at = Sys.time(),
    expires_at = Sys.time() + ttl,
    size = as.numeric(size),
    compressed = compressed,
    access_count = 0
  )
  
  # Enforce cache limits
  enforce_cache_limits()
}

#' Get cache entry
get_cache_entry <- function(key) {
  if (!exists(key, envir = .fhir_cache)) {
    return(NULL)
  }
  
  entry <- .fhir_cache[[key]]
  
  # Update access count
  entry$access_count <- entry$access_count + 1
  .fhir_cache[[key]] <- entry
  
  # Decompress if needed
  if (isTRUE(entry$compressed)) {
    entry$data <- decompress_data(entry$data)
  }
  
  return(entry)
}

#' Enforce cache size and memory limits
enforce_cache_limits <- function() {
  cache_keys <- ls(envir = .fhir_cache)
  
  # Check size limit
  if (length(cache_keys) > .fhir_cache_config$max_cache_size) {
    # Remove least recently used
    entries <- lapply(cache_keys, function(k) {
      e <- .fhir_cache[[k]]
      e$key <- k
      e
    })
    
    # Sort by access count and age
    entries <- entries[order(sapply(entries, function(e) e$access_count),
                           sapply(entries, function(e) e$cached_at))]
    
    # Remove oldest/least used
    to_remove <- head(entries, length(entries) - .fhir_cache_config$max_cache_size)
    for (entry in to_remove) {
      rm(list = entry$key, envir = .fhir_cache)
      update_cache_stats("evictions")
    }
  }
  
  # Check memory limit
  total_size <- sum(sapply(cache_keys, function(k) .fhir_cache[[k]]$size))
  
  if (total_size > .fhir_cache_config$max_cache_memory) {
    # Remove largest items first
    entries <- lapply(cache_keys, function(k) {
      e <- .fhir_cache[[k]]
      e$key <- k
      e
    })
    
    entries <- entries[order(sapply(entries, function(e) -e$size))]
    
    evicted <- 0
    for (entry in entries) {
      if (total_size <= .fhir_cache_config$max_cache_memory) {
        break
      }
      
      size <- .fhir_cache[[entry$key]]$size
      rm(list = entry$key, envir = .fhir_cache)
      total_size <- total_size - size
      evicted <- evicted + 1
      update_cache_stats("evictions")
    }
    
    if (evicted > 0 && getOption("fhir.cache.verbose", FALSE)) {
      message(sprintf("FHIR: Evicted %d entries for memory limit", evicted))
    }
  }
}

#' Clear FHIR cache
#' 
#' @param pattern Optional regex pattern to match keys
#' @param older_than Clear entries older than this (POSIXct or difftime)
#' @export
clear_fhir_cache <- function(pattern = NULL, older_than = NULL) {
  cache_keys <- ls(envir = .fhir_cache)
  
  # Filter by pattern if provided
  if (!is.null(pattern)) {
    cache_keys <- grep(pattern, cache_keys, value = TRUE)
  }
  
  # Filter by age if provided
  if (!is.null(older_than)) {
    if (inherits(older_than, "difftime")) {
      cutoff <- Sys.time() - older_than
    } else if (inherits(older_than, "POSIXct")) {
      cutoff <- older_than
    } else {
      stop("older_than must be POSIXct or difftime")
    }
    
    cache_keys <- cache_keys[sapply(cache_keys, function(key) {
      .fhir_cache[[key]]$cached_at < cutoff
    })]
  }
  
  # Remove selected keys
  if (length(cache_keys) > 0) {
    rm(list = cache_keys, envir = .fhir_cache)
    message(sprintf("FHIR cache: Cleared %d entries", length(cache_keys)))
  } else {
    message("FHIR cache: No entries to clear")
  }
}

#' Invalidate cache entries
#' 
#' @param resource_type FHIR resource type (e.g., "Patient", "Observation")
#' @param resource_id Optional specific resource ID
#' @export
invalidate_fhir_cache <- function(resource_type = NULL, resource_id = NULL) {
  if (is.null(resource_type)) {
    clear_fhir_cache()
    return()
  }
  
  # Build pattern for invalidation
  if (!is.null(resource_id)) {
    pattern <- sprintf("%s.*%s", resource_type, resource_id)
  } else {
    pattern <- sprintf("%s.*", resource_type)
  }
  
  clear_fhir_cache(pattern = pattern)
}

#' Get cache statistics
#' 
#' @return List with cache statistics
#' @export
get_cache_stats <- function() {
  cache_keys <- ls(envir = .fhir_cache)
  
  # Calculate cache metrics
  total_size <- 0
  expired_count <- 0
  current_time <- Sys.time()
  
  if (length(cache_keys) > 0) {
    sizes <- sapply(cache_keys, function(key) {
      entry <- .fhir_cache[[key]]
      if (entry$expires_at < current_time) {
        expired_count <<- expired_count + 1
      }
      entry$size
    })
    total_size <- sum(sizes)
  }
  
  # Calculate hit rate
  total_requests <- .fhir_cache_stats$hits + .fhir_cache_stats$misses
  hit_rate <- if (total_requests > 0) {
    .fhir_cache_stats$hits / total_requests * 100
  } else {
    0
  }
  
  list(
    entries = length(cache_keys),
    expired_entries = expired_count,
    total_size = total_size,
    total_size_mb = total_size / 1024^2,
    hits = .fhir_cache_stats$hits,
    misses = .fhir_cache_stats$misses,
    evictions = .fhir_cache_stats$evictions,
    revalidations = .fhir_cache_stats$revalidations,
    hit_rate_percent = round(hit_rate, 2),
    config = .fhir_cache_config
  )
}

#' Configure cache settings
#' 
#' @param default_ttl Default TTL in seconds
#' @param max_cache_size Maximum number of entries
#' @param max_cache_memory Maximum memory in bytes
#' @param enable_compression Enable compression for large entries
#' @param enable_stats Enable statistics tracking
#' @export
configure_fhir_cache <- function(default_ttl = NULL,
                                max_cache_size = NULL,
                                max_cache_memory = NULL,
                                enable_compression = NULL,
                                enable_stats = NULL) {
  
  if (!is.null(default_ttl)) {
    .fhir_cache_config$default_ttl <- default_ttl
  }
  
  if (!is.null(max_cache_size)) {
    .fhir_cache_config$max_cache_size <- max_cache_size
  }
  
  if (!is.null(max_cache_memory)) {
    .fhir_cache_config$max_cache_memory <- max_cache_memory
  }
  
  if (!is.null(enable_compression)) {
    .fhir_cache_config$enable_compression <- enable_compression
  }
  
  if (!is.null(enable_stats)) {
    .fhir_cache_config$enable_stats <- enable_stats
  }
  
  message("FHIR cache configuration updated")
  return(.fhir_cache_config)
}

#' Warm up cache with preloaded data
#' 
#' @param preload_fn Function that returns list of cache entries
#' @export
warmup_cache <- function(preload_fn) {
  if (!is.function(preload_fn)) {
    stop("preload_fn must be a function")
  }
  
  entries <- preload_fn()
  
  if (!is.list(entries)) {
    stop("preload_fn must return a list")
  }
  
  loaded <- 0
  for (item in entries) {
    if (!is.null(item$key) && !is.null(item$data)) {
      ttl <- item$ttl %||% .fhir_cache_config$default_ttl
      set_cache_entry(item$key, item$data, ttl)
      loaded <- loaded + 1
    }
  }
  
  message(sprintf("FHIR cache warmed up with %d entries", loaded))
}

# Helper functions

#' Update cache statistics
update_cache_stats <- function(stat_type) {
  if (!.fhir_cache_config$enable_stats) return()
  
  if (stat_type %in% names(.fhir_cache_stats)) {
    .fhir_cache_stats[[stat_type]] <- .fhir_cache_stats[[stat_type]] + 1
  }
}

#' Compress data for storage
compress_data <- function(data) {
  serialized <- serialize(data, NULL)
  memCompress(serialized, type = "gzip")
}

#' Decompress data from storage
decompress_data <- function(compressed_data) {
  decompressed <- memDecompress(compressed_data, type = "gzip")
  unserialize(decompressed)
}

#' Format bytes for display
format_bytes <- function(bytes) {
  if (bytes < 1024) {
    return(sprintf("%d B", bytes))
  } else if (bytes < 1024^2) {
    return(sprintf("%.1f KB", bytes / 1024))
  } else if (bytes < 1024^3) {
    return(sprintf("%.1f MB", bytes / 1024^2))
  } else {
    return(sprintf("%.1f GB", bytes / 1024^3))
  }
}

#' Reset cache statistics
#' @export
reset_cache_stats <- function() {
  .fhir_cache_stats$hits <- 0
  .fhir_cache_stats$misses <- 0
  .fhir_cache_stats$evictions <- 0
  .fhir_cache_stats$revalidations <- 0
  message("FHIR cache statistics reset")
}