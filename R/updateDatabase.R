.BIOTIC_API_BASE <- "https://biotic-api.hi.no/apis/nmdapi/biotic/v3"

.api_path <- function(...) {
  parts <- vapply(list(...), function(x) {
    utils::URLencode(as.character(x), reserved = TRUE)
  }, character(1))
  paste(c(.BIOTIC_API_BASE, parts), collapse = "/")
}

.api_xml_request <- function(url) {
  request <- httr2::request(url)
  request <- httr2::req_retry(
    request, max_tries = 4, retry_on_failure = TRUE
  )
  httr2::req_timeout(request, 60)
}

.api_get_xml <- function(url) {
  request <- .api_xml_request(url)
  response <- httr2::req_perform(request)
  httr2::resp_check_status(response)
  httr2::resp_body_xml(response)
}

.api_list_field_from_document <- function(document, field) {
  nodes <- xml2::xml_find_all(
    document,
    paste0("//*[local-name()='element'][@name='", field, "']")
  )
  trimws(xml2::xml_text(nodes))
}

.api_list_field <- function(url, field) {
  document <- .api_get_xml(url)
  .api_list_field_from_document(document, field)
}

.api_list_fields_parallel <- function(urls, field, max_active) {
  if (!length(urls)) return(list())
  requests <- lapply(urls, .api_xml_request)
  responses <- httr2::req_perform_parallel(
    requests,
    on_error = "return",
    progress = FALSE,
    max_active = max_active
  )
  lapply(responses, function(response) {
    if (inherits(response, "error")) stop(response)
    httr2::resp_check_status(response)
    document <- httr2::resp_body_xml(response)
    .api_list_field_from_document(document, field)
  })
}

.api_delivery_request <- function(missiontype, year, platform, delivery) {
  url <- paste0(
    .api_path(missiontype, year, platform, delivery, "dataset"),
    "?version=3.1"
  )
  request <- httr2::request(url)
  request <- httr2::req_method(request, "HEAD")
  request <- httr2::req_retry(
    request, max_tries = 4, retry_on_failure = TRUE
  )
  request <- httr2::req_timeout(request, 60)
  httr2::req_error(request, is_error = function(response) {
    httr2::resp_status(response) >= 400 && httr2::resp_status(response) != 404
  })
}

.api_delivery_headers_from_response <- function(response, delivery) {
  if (inherits(response, "error")) stop(response)
  if (httr2::resp_status(response) == 404) return(NULL)
  httr2::resp_check_status(response)

  value <- function(name) {
    result <- httr2::resp_header(response, name)
    if (is.null(result)) NA_character_ else as.character(result)
  }
  out <- c(
    last_modified = value("last-modified"),
    last_snapshot_code = value("last-snapshot-code"),
    last_snapshot_time = value("last-snapshot-time"),
    format_version = value("format_version")
  )
  if (all(is.na(out[c("last_modified", "last_snapshot_code", "last_snapshot_time")]))) {
    stop("The API returned no change metadata for delivery ", delivery, ".")
  }
  out
}

.api_delivery_headers <- function(missiontype, year, platform, delivery) {
  request <- .api_delivery_request(missiontype, year, platform, delivery)
  response <- httr2::req_perform(request)
  .api_delivery_headers_from_response(response, delivery)
}

.api_delivery_headers_parallel <- function(deliveries, max_active) {
  if (!nrow(deliveries)) return(list())
  requests <- lapply(seq_len(nrow(deliveries)), function(n) {
    delivery <- deliveries[n, , drop = FALSE]
    .api_delivery_request(
      delivery$missiontype, delivery$data_year, delivery$platform,
      delivery$delivery
    )
  })
  responses <- httr2::req_perform_parallel(
    requests,
    on_error = "return",
    progress = FALSE,
    max_active = max_active
  )
  lapply(seq_along(responses), function(n) {
    .api_delivery_headers_from_response(
      responses[[n]], deliveries$delivery[[n]]
    )
  })
}

.valid_source_years <- function(years) {
  years <- as.integer(years)
  years[!is.na(years) & years >= 1900L &
          years <= data.table::year(Sys.time())]
}

.discover_source_deliveries_sequential <- function(years = NULL) {
  missiontypes <- .api_list_field(.BIOTIC_API_BASE, "missiontypename")
  result <- list()
  n <- 0L

  for (missiontype in missiontypes) {
    available_years <- .valid_source_years(.api_list_field(
      .api_path(missiontype), "year"
    ))
    if (!is.null(years)) available_years <- intersect(available_years, years)

    for (year in available_years) {
      platforms <- .api_list_field(.api_path(missiontype, year), "platformpath")
      for (platform in platforms) {
        deliveries <- .api_list_field(
          .api_path(missiontype, year, platform), "delivery"
        )
        for (delivery in deliveries) {
          n <- n + 1L
          result[[n]] <- data.frame(
            missiontype = missiontype,
            data_year = as.integer(year),
            platform = platform,
            delivery = as.character(delivery),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (!length(result)) {
    return(data.frame(
      missiontype = character(), data_year = integer(), platform = character(),
      delivery = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, result)
}

.discover_source_deliveries_parallel <- function(years = NULL, workers = 8L) {
  missiontypes <- .api_list_field(.BIOTIC_API_BASE, "missiontypename")
  year_values <- .api_list_fields_parallel(
    lapply(missiontypes, .api_path), "year", workers
  )

  mission_years <- list()
  for (n in seq_along(missiontypes)) {
    available_years <- .valid_source_years(year_values[[n]])
    if (!is.null(years)) available_years <- intersect(available_years, years)
    if (length(available_years)) {
      mission_years[[length(mission_years) + 1L]] <- data.frame(
        missiontype = missiontypes[[n]], data_year = available_years,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(mission_years)) {
    return(data.frame(
      missiontype = character(), data_year = integer(), platform = character(),
      delivery = character(), stringsAsFactors = FALSE
    ))
  }
  mission_years <- do.call(rbind, mission_years)

  platform_values <- .api_list_fields_parallel(
    lapply(seq_len(nrow(mission_years)), function(n) {
      .api_path(mission_years$missiontype[[n]], mission_years$data_year[[n]])
    }),
    "platformpath", workers
  )
  mission_platforms <- list()
  for (n in seq_len(nrow(mission_years))) {
    if (length(platform_values[[n]])) {
      mission_platforms[[length(mission_platforms) + 1L]] <- data.frame(
        missiontype = mission_years$missiontype[[n]],
        data_year = mission_years$data_year[[n]],
        platform = platform_values[[n]], stringsAsFactors = FALSE
      )
    }
  }
  if (!length(mission_platforms)) {
    return(data.frame(
      missiontype = character(), data_year = integer(), platform = character(),
      delivery = character(), stringsAsFactors = FALSE
    ))
  }
  mission_platforms <- do.call(rbind, mission_platforms)

  delivery_values <- .api_list_fields_parallel(
    lapply(seq_len(nrow(mission_platforms)), function(n) {
      .api_path(
        mission_platforms$missiontype[[n]],
        mission_platforms$data_year[[n]],
        mission_platforms$platform[[n]]
      )
    }),
    "delivery", workers
  )
  result <- list()
  for (n in seq_len(nrow(mission_platforms))) {
    if (length(delivery_values[[n]])) {
      result[[length(result) + 1L]] <- data.frame(
        missiontype = mission_platforms$missiontype[[n]],
        data_year = mission_platforms$data_year[[n]],
        platform = mission_platforms$platform[[n]],
        delivery = as.character(delivery_values[[n]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(result)) {
    return(data.frame(
      missiontype = character(), data_year = integer(), platform = character(),
      delivery = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, result)
}

.discover_source_deliveries <- function(years = NULL, workers = 1L) {
  if (workers > 1L) {
    .discover_source_deliveries_parallel(years, workers)
  } else {
    .discover_source_deliveries_sequential(years)
  }
}

.manifest_delivery_keys <- function(value) {
  if (!nrow(value)) return(character())
  paste(value$missiontype, value$data_year, value$platform, value$delivery,
        sep = "\r")
}

.manifest_year_is_baseline <- function(value) {
  nrow(value) > 0L && all(
    is.na(value$last_modified) &
      is.na(value$last_snapshot_code) &
      is.na(value$last_snapshot_time)
  )
}

.manifest_row <- function(delivery, headers, checked_at) {
  data.frame(
    delivery,
    last_modified = unname(headers[["last_modified"]]),
    last_snapshot_code = unname(headers[["last_snapshot_code"]]),
    last_snapshot_time = unname(headers[["last_snapshot_time"]]),
    format_version = unname(headers[["format_version"]]),
    checked_at = checked_at,
    stringsAsFactors = FALSE
  )
}

.year_baseline_placeholder <- function(year, checked_at) {
  data.frame(
    missiontype = "", data_year = as.integer(year), platform = "", delivery = "",
    last_modified = NA_character_, last_snapshot_code = NA_character_,
    last_snapshot_time = NA_character_, format_version = NA_character_,
    checked_at = checked_at, stringsAsFactors = FALSE
  )
}

.manifest_row_newer_than <- function(value, checked_at) {
  baseline <- .parse_api_time(checked_at)
  signals <- c(
    .parse_api_time(value$last_modified),
    .parse_api_time(value$last_snapshot_time)
  )
  signals <- signals[!is.na(signals)]
  is.na(baseline) || !length(signals) || any(signals > baseline)
}

.report_metadata_progress <- function(progress, progress_bar, verbose, processed,
                                      total, progress_marks, reported_marks) {
  if (progress) {
    utils::setTxtProgressBar(progress_bar, processed)
  } else if (!verbose) {
    marks <- progress_marks[
      progress_marks <= processed & !progress_marks %in% reported_marks
    ]
    if (length(marks)) {
      mark <- max(marks)
      message("Metadata progress: ", mark, "/", total, " (",
              round(100 * mark / total), "%).")
    }
    reported_marks <- c(reported_marks, marks)
  }
  reported_marks
}

.discover_source_manifest <- function(years = NULL, verbose = FALSE,
                                      stored_manifest = NULL,
                                      metadata_workers = 1L,
                                      parsed_baseline_years = integer()) {
  metadata_workers <- as.integer(metadata_workers)
  if (length(metadata_workers) != 1L || is.na(metadata_workers) ||
      metadata_workers < 1L) {
    stop("metadata_workers must be a positive integer.", call. = FALSE)
  }
  deliveries <- if (metadata_workers > 1L) {
    .discover_source_deliveries(years, metadata_workers)
  } else {
    .discover_source_deliveries(years)
  }
  total <- nrow(deliveries)
  if (!total && is.null(stored_manifest)) return(.empty_source_manifest())

  checked_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  result <- list()
  unavailable <- 0L
  progress <- total > 0L && !verbose && interactive()
  progress_bar <- NULL
  if (progress) {
    progress_bar <- utils::txtProgressBar(min = 0, max = total, style = 3)
    on.exit(close(progress_bar), add = TRUE)
  } else if (!verbose) {
    message("Checking metadata for ", total, " deliveries.")
  }
  progress_marks <- unique(pmax(1L, ceiling(total * seq(0.1, 1, by = 0.1))))
  reported_marks <- integer()
  processed <- 0L

  if (!is.null(stored_manifest)) {
    changed_years <- integer()
    check_years <- sort(unique(c(deliveries$data_year, stored_manifest$data_year)))

    for (year in check_years) {
      year_deliveries <- deliveries[deliveries$data_year == year, , drop = FALSE]
      stored_year <- stored_manifest[stored_manifest$data_year == year, , drop = FALSE]
      baseline <- .manifest_year_is_baseline(stored_year)
      baseline_at <- if (baseline) stored_year$checked_at[[1]] else NA_character_
      stored_inventory <- stored_year[
        stored_year$missiontype != "" | stored_year$platform != "" |
          stored_year$delivery != "",
        , drop = FALSE
      ]
      stored_keys <- .manifest_delivery_keys(stored_inventory)
      rebase_parsed_baseline <- baseline && year %in% parsed_baseline_years
      year_result <- list()
      year_changed <- FALSE
      last_checked <- 0L
      parallel_headers <- if (metadata_workers > 1L && nrow(year_deliveries)) {
        .api_delivery_headers_parallel(year_deliveries, metadata_workers)
      } else NULL

      for (n in seq_len(nrow(year_deliveries))) {
        last_checked <- n
        delivery <- year_deliveries[n, , drop = FALSE]
        if (verbose) {
          message("Checking metadata: ", delivery$missiontype, " / ",
                  delivery$data_year, " / ", delivery$platform, " / ",
                  delivery$delivery)
        }
        headers <- if (metadata_workers > 1L) {
          parallel_headers[[n]]
        } else {
          .api_delivery_headers(
            delivery$missiontype, delivery$data_year, delivery$platform,
            delivery$delivery
          )
        }
        processed <- processed + 1L
        reported_marks <- .report_metadata_progress(
          progress, progress_bar, verbose, processed, total, progress_marks,
          reported_marks
        )
        if (is.null(headers)) {
          unavailable <- unavailable + 1L
          next
        }

        row <- .manifest_row(delivery, headers, checked_at)
        key <- .manifest_delivery_keys(row)
        if (baseline) {
          year_changed <- .manifest_row_newer_than(row, baseline_at) ||
            (!rebase_parsed_baseline && !key %in% stored_keys)
        } else {
          old <- stored_year[.manifest_delivery_keys(stored_year) == key, , drop = FALSE]
          year_changed <- nrow(old) != 1L ||
            !identical(.normalise_manifest(old), .normalise_manifest(row))
        }
        if (year_changed) break
        year_result[[length(year_result) + 1L]] <- row
      }

      if (year_changed) {
        changed_years <- c(changed_years, year)
        if (nrow(year_deliveries)) {
          result[[length(result) + 1L]] <- .year_baseline_placeholder(year, checked_at)
        }
        skipped <- nrow(year_deliveries) - last_checked
        if (skipped > 0L) {
          if (!verbose) {
            if (metadata_workers > 1L) {
              message("Change found for ", year, ".")
            } else {
              message(
                "Change found for ", year, "; skipped ", skipped,
                if (skipped == 1L) " remaining metadata request."
                else " remaining metadata requests."
              )
            }
          }
          processed <- processed + skipped
          reported_marks <- .report_metadata_progress(
            progress, progress_bar, verbose, processed, total, progress_marks,
            reported_marks
          )
        }
        next
      }

      current_year <- if (length(year_result)) {
        do.call(rbind, year_result)
      } else .empty_source_manifest()
      if (!rebase_parsed_baseline &&
          !identical(sort(.manifest_delivery_keys(current_year)), sort(stored_keys))) {
        changed_years <- c(changed_years, year)
        if (nrow(year_deliveries)) {
          result[[length(result) + 1L]] <- .year_baseline_placeholder(year, checked_at)
        }
      } else if (nrow(current_year)) {
        result[[length(result) + 1L]] <- current_year
      }
    }

    value <- if (length(result)) do.call(rbind, result) else .empty_source_manifest()
    attr(value, "changed_years") <- sort(unique(as.integer(changed_years)))
    attr(value, "delivery_inventory") <- deliveries
    if (unavailable) {
      warning(
        "Skipped ", unavailable,
        if (unavailable == 1L) " delivery" else " deliveries",
        " that disappeared from the API during metadata discovery.",
        call. = FALSE
      )
    }
    return(value)
  }

  parallel_headers <- vector("list", total)
  if (metadata_workers > 1L) {
    for (year in sort(unique(deliveries$data_year))) {
      indices <- which(deliveries$data_year == year)
      parallel_headers[indices] <- .api_delivery_headers_parallel(
        deliveries[indices, , drop = FALSE], metadata_workers
      )
    }
  }

  for (n in seq_len(total)) {
    delivery <- deliveries[n, , drop = FALSE]
    if (verbose) {
      message("Checking metadata: ", delivery$missiontype, " / ",
              delivery$data_year, " / ", delivery$platform, " / ",
              delivery$delivery)
    }
    headers <- if (metadata_workers > 1L) {
      parallel_headers[[n]]
    } else {
      .api_delivery_headers(
        delivery$missiontype, delivery$data_year, delivery$platform,
        delivery$delivery
      )
    }
    if (is.null(headers)) {
      unavailable <- unavailable + 1L
    } else {
      result[[n]] <- .manifest_row(delivery, headers, checked_at)
    }
    if (progress) {
      utils::setTxtProgressBar(progress_bar, n)
    } else if (!verbose && n %in% progress_marks) {
      message("Metadata progress: ", n, "/", total, " (",
              round(100 * n / total), "%).")
    }
  }

  if (unavailable) {
    warning(
      "Skipped ", unavailable, if (unavailable == 1L) " delivery" else " deliveries",
      " that disappeared from the API during metadata discovery.",
      call. = FALSE
    )
  }
  result <- Filter(Negate(is.null), result)
  if (!length(result)) return(.empty_source_manifest())
  do.call(rbind, result)
}

.manifest_comparison_columns <- c(
  "missiontype", "data_year", "platform", "delivery", "last_modified",
  "last_snapshot_code", "last_snapshot_time", "format_version"
)

.normalise_manifest <- function(manifest) {
  if (!nrow(manifest)) return(manifest[, .manifest_comparison_columns, drop = FALSE])
  value <- manifest[, .manifest_comparison_columns, drop = FALSE]
  value[] <- lapply(value, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  value <- value[
    do.call(order, value[c("missiontype", "data_year", "platform", "delivery")]),
    , drop = FALSE
  ]
  rownames(value) <- NULL
  value
}

.manifest_year_equal <- function(old, current, year) {
  old_year <- old[old$data_year == year, , drop = FALSE]
  current_year <- current[current$data_year == year, , drop = FALSE]
  identical(.normalise_manifest(old_year), .normalise_manifest(current_year))
}

.local_delivery_inventory <- function(connection) {
  if (!.legacy_database_is_compatible(connection)) {
    return(data.frame())
  }
  mission <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT DISTINCT missiontypename, startyear, platformname, platform,",
      "missionnumber FROM mission"
    )
  )
  data.frame(
    missiontype = as.character(mission$missiontypename),
    data_year = as.integer(mission$startyear),
    platform = paste0(mission$platformname, "_", mission$platform),
    delivery = as.character(mission$missionnumber),
    stringsAsFactors = FALSE
  )
}

.baseline_manifest_from_parsed <- function(parsed, year, checked_at) {
  mission <- parsed$mission
  if (is.null(mission) || !nrow(mission)) return(.empty_source_manifest())
  inventory <- unique(data.frame(
    missiontype = as.character(mission$missiontypename),
    data_year = as.integer(year),
    platform = paste0(mission$platformname, "_", mission$platform),
    delivery = as.character(mission$missionnumber),
    stringsAsFactors = FALSE
  ))
  data.frame(
    inventory,
    last_modified = NA_character_,
    last_snapshot_code = NA_character_,
    last_snapshot_time = NA_character_,
    format_version = NA_character_,
    checked_at = format(checked_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

.baseline_manifest_from_inventory <- function(inventory, year, checked_at) {
  inventory <- inventory[inventory$data_year == year, c(
    "missiontype", "data_year", "platform", "delivery"
  ), drop = FALSE]
  inventory <- unique(inventory)
  if (!nrow(inventory)) return(.empty_source_manifest())
  data.frame(
    inventory,
    last_modified = NA_character_,
    last_snapshot_code = NA_character_,
    last_snapshot_time = NA_character_,
    format_version = NA_character_,
    checked_at = format(checked_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

.parsed_baseline_years <- function(connection, stored_manifest) {
  if (!nrow(stored_manifest)) return(integer())
  local <- .local_delivery_inventory(connection)
  stored_by_year <- split(stored_manifest, stored_manifest$data_year)
  candidate_years <- as.integer(names(stored_by_year)[vapply(
    stored_by_year, .manifest_year_is_baseline, logical(1)
  )])
  candidate_years[vapply(candidate_years, function(year) {
    stored_year <- stored_manifest[stored_manifest$data_year == year, , drop = FALSE]
    local_year <- local[local$data_year == year, , drop = FALSE]
    identical(.manifest_keys(stored_year), .manifest_keys(local_year))
  }, logical(1))]
}

.manifest_keys <- function(value) {
  if (!nrow(value)) return(character())
  sort(unique(paste(value$missiontype, value$data_year, value$platform,
                    value$delivery, sep = "\r")))
}

.parse_api_time <- function(x) {
  x <- sub(" UTC$", "", as.character(x))
  value <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  formats <- c(
    "%a, %d %b %Y %H:%M:%S GMT", "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d %H:%M:%S"
  )
  for (time_format in formats) {
    missing <- is.na(value)
    if (!any(missing)) break
    parsed <- suppressWarnings(as.POSIXct(
      x[missing], format = time_format, tz = "UTC"
    ))
    value[missing] <- parsed
  }
  value
}

.legacy_year_is_current <- function(connection, manifest, year) {
  if (!.database_has_year(connection, year)) return(FALSE)
  remote <- manifest[manifest$data_year == year, , drop = FALSE]
  if (!nrow(remote)) return(FALSE)

  metadata <- .database_metadata(connection)
  built <- .parse_api_time(metadata$timeend[[1]])
  signals <- c(.parse_api_time(remote$last_modified),
               .parse_api_time(remote$last_snapshot_time))
  signals <- signals[!is.na(signals)]
  !is.na(built) && length(signals) && built >= max(signals)
}

.download_year_for_update <- function(year, icesAreas = NULL,
                                      cruiseSeries = NULL, gearCodes = NULL) {
  destination <- tempfile(fileext = ".xml")
  on.exit(unlink(destination), add = TRUE)
  url <- paste0(.api_path(year, "cache"), "?version=3.1")
  status <- NULL
  for (attempt in seq_len(4)) {
    status <- suppressWarnings(try(
      utils::download.file(url, destination, quiet = TRUE), silent = TRUE
    ))
    if (!inherits(status, "try-error")) break
  }
  if (inherits(status, "try-error") || is.na(file.info(destination)$size)) {
    stop("Could not download Biotic data for year ", year, " after 4 attempts.")
  }
  list(
    parsed = bioticToDatabase(
      destination,
      missionidPrefix = year,
      icesAreas = icesAreas,
      cruiseSeries = cruiseSeries,
      gearCodes = gearCodes
    ),
    filesize = file.info(destination)$size
  )
}

.refresh_reference_tables <- function(connection) {
  message("Refreshing cruise series, gear, taxa, and reference-code tables")

  # Download everything before opening the transaction so an API failure leaves
  # every existing reference table untouched.
  reference_tables <- list(
    csindex = prepareCruiseSeriesList(),
    gearindex = prepareGearList(),
    taxaindex = prepareTaxaList(),
    codeindex = prepareReferenceCodes()
  )
  empty <- names(reference_tables)[vapply(reference_tables, nrow, integer(1)) == 0L]
  if (length(empty)) {
    stop(
      "Reference refresh returned no rows for: ", paste(empty, collapse = ", "),
      ". Existing reference tables were left unchanged."
    )
  }

  DBI::dbWithTransaction(connection, {
    for (table in names(reference_tables)) {
      DBI::dbWriteTable(
        connection, table, reference_tables[[table]], overwrite = TRUE
      )
    }
  })

  invisible(list(
    cruiseSeries = data.table::as.data.table(reference_tables$csindex),
    gearCodes = data.table::as.data.table(reference_tables$gearindex)
  ))
}

.validate_rebuilt_database <- function(path, index_file) {
  if (!file.exists(path) || !file.exists(index_file)) return(FALSE)
  index_environment <- new.env(parent = emptyenv())
  index_valid <- !inherits(
    try(load(index_file, envir = index_environment), silent = TRUE),
    "try-error"
  ) && exists("index", envir = index_environment, inherits = FALSE)
  if (!index_valid) return(FALSE)
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  all(.BES_FACT_TABLES %in% DBI::dbListTables(connection)) &&
    identical(.database_schema_status(connection), "compatible")
}

.swap_rebuilt_database <- function(next_database, database, next_index, index_file) {
  suffix <- paste0(".backup-", Sys.getpid())
  database_backup <- paste0(database, suffix)
  index_backup <- paste0(index_file, suffix)
  database_had_old <- file.exists(database)
  index_had_old <- file.exists(index_file)

  if (database_had_old && !file.rename(database, database_backup)) {
    stop("Could not move the active database aside for the compatibility rebuild.")
  }
  if (index_had_old && !file.rename(index_file, index_backup)) {
    if (database_had_old) file.rename(database_backup, database)
    stop("Could not move the active database index aside.")
  }

  success <- file.rename(next_database, database)
  success <- success && file.rename(next_index, index_file)
  if (!success) {
    if (file.exists(database)) unlink(database)
    if (file.exists(index_file)) unlink(index_file)
    if (database_had_old) file.rename(database_backup, database)
    if (index_had_old) file.rename(index_backup, index_file)
    stop("Could not install the rebuilt database; the previous database was restored.")
  }

  if (file.exists(database_backup)) unlink(database_backup)
  if (file.exists(index_backup)) unlink(index_backup)
  invisible(NULL)
}

.rebuild_incompatible_database <- function(dbPath, dbName, dbIndexFile) {
  next_name <- paste0(dbName, "-next-", Sys.getpid())
  next_database <- normalizePath(
    paste0(file.path(dbPath, next_name), ".duckdb"), mustWork = FALSE
  )
  next_index <- normalizePath(
    file.path(dbPath, paste0("dbIndex-next-", Sys.getpid(), ".rda")),
    mustWork = FALSE
  )
  on.exit({
    if (file.exists(next_database)) unlink(next_database)
    if (file.exists(next_index)) unlink(next_index)
  }, add = TRUE)

  message("Database schema is incompatible. Building a complete replacement.")
  compileDatabase(dbPath = dbPath, dbName = next_name, dbIndexFile = next_index)
  if (!.validate_rebuilt_database(next_database, next_index)) {
    stop("The compatibility rebuild did not pass validation; the active database is unchanged.")
  }
  database <- normalizePath(
    paste0(file.path(dbPath, dbName), ".duckdb"), mustWork = FALSE
  )
  .swap_rebuilt_database(next_database, database, next_index, dbIndexFile)
  invisible(list(mode = "rebuild", years = NULL))
}

#' Incrementally update a BioticExplorer database
#'
#' Uses metadata-only API requests to identify years containing changed, added,
#' or removed deliveries. Once one changed delivery is found, remaining
#' deliveries in that year are skipped because the complete annual XML cache
#' must be replaced. Cruise-series, gear, taxa, and coded-field reference tables
#' are refreshed on every run. If the database schema is incompatible with this
#' package
#' version, a complete sibling database is built with
#' \code{\link{compileDatabase}} and safely swapped into place.
#'
#' @param years Optional integer vector limiting metadata checks and incremental
#'   replacements. A schema compatibility rebuild always includes all years.
#' @inheritParams compileDatabase
#' @param verbose Logical; emit one metadata-check message per delivery. The
#'   default uses a progress bar in interactive sessions and bounded milestone
#'   messages in non-interactive logs.
#' @param metadata_workers Positive integer controlling the maximum number of
#'   concurrent delivery-discovery and metadata requests. Database writes remain
#'   serial and transactional. Use \code{1} to disable parallel API checks.
#' @return Invisibly returns a list describing the update mode and changed years.
#' @export
updateDatabase <- function(
  years = NULL,
  dbPath = defaultDbPath(),
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  verbose = FALSE,
  metadata_workers = 8L
) {
  operation_started_at <- .start_operation_timer("Update")
  operation_succeeded <- FALSE
  on.exit({
    if (operation_succeeded) {
      .finish_operation_timer("Update", operation_started_at)
    }
  }, add = TRUE)

  time_start <- Sys.time()
  if (is.null(dbName)) dbName <- "bioticexplorer"
  dbPath <- path.expand(dbPath)
  dbIndexFile <- path.expand(dbIndexFile)
  database <- normalizePath(
    paste0(file.path(dbPath, dbName), ".duckdb"), mustWork = FALSE
  )
  if (!file.exists(database)) {
    message("No existing database found. Running compileDatabase().")
    compileDatabase(dbPath = dbPath, dbName = dbName, dbIndexFile = dbIndexFile)
    operation_succeeded <- TRUE
    return(invisible(list(mode = "compile", years = NULL)))
  }

  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  connection_open <- TRUE
  on.exit({
    if (connection_open) {
      try(DBI::dbDisconnect(connection, shutdown = TRUE), silent = TRUE)
    }
  }, add = TRUE)
  status <- .database_schema_status(connection)
  if (identical(status, "incompatible")) {
    DBI::dbDisconnect(connection, shutdown = TRUE)
    connection_open <- FALSE
    result <- .rebuild_incompatible_database(dbPath, dbName, dbIndexFile)
    operation_succeeded <- TRUE
    return(result)
  }

  if (identical(status, "legacy-compatible")) {
    old_metadata <- .database_metadata(connection)
    .write_database_metadata(
      connection, old_metadata$timestart[[1]], old_metadata$timeend[[1]],
      update_mode = "legacy-migration"
    )
  }
  .ensure_source_manifest(connection)

  refreshed_references <- .refresh_reference_tables(connection)

  requested_years <- if (is.null(years)) NULL else sort(unique(as.integer(years)))
  stored_manifest <- DBI::dbReadTable(connection, "source_manifest")
  discovery_manifest <- if (is.null(requested_years)) {
    stored_manifest
  } else {
    stored_manifest[stored_manifest$data_year %in% requested_years, , drop = FALSE]
  }
  parsed_baseline_years <- .parsed_baseline_years(connection, discovery_manifest)
  current_manifest <- .discover_source_manifest(
    requested_years, verbose = verbose,
    stored_manifest = if (nrow(discovery_manifest)) discovery_manifest else NULL,
    metadata_workers = metadata_workers,
    parsed_baseline_years = parsed_baseline_years
  )
  discovered_changed_years <- attr(current_manifest, "changed_years")
  delivery_inventory <- attr(current_manifest, "delivery_inventory")
  local_years <- if ("mission" %in% DBI::dbListTables(connection)) {
    DBI::dbGetQuery(connection, "SELECT DISTINCT startyear FROM mission")$startyear
  } else integer()
  target_years <- if (is.null(requested_years)) {
    sort(unique(c(current_manifest$data_year, stored_manifest$data_year, local_years)))
  } else requested_years

  legacy_manifest <- !nrow(stored_manifest)
  changed_years <- if (!is.null(discovered_changed_years)) {
    intersect(target_years, discovered_changed_years)
  } else {
    target_years[vapply(target_years, function(year) {
      if (legacy_manifest) {
        !.legacy_year_is_current(connection, current_manifest, year)
      } else {
        !.manifest_year_equal(stored_manifest, current_manifest, year)
      }
    }, logical(1))]
  }

  cruiseSeries <- refreshed_references$cruiseSeries
  gearCodes <- refreshed_references$gearCodes

  for (year in changed_years) {
    year_manifest <- current_manifest[current_manifest$data_year == year, , drop = FALSE]
    if (nrow(year_manifest)) {
      message("Downloading changed year: ", year)
      download_started_at <- Sys.time()
      downloaded <- .download_year_for_update(
        year, cruiseSeries = cruiseSeries, gearCodes = gearCodes
      )
      if (!is.null(discovered_changed_years)) {
        year_manifest <- if (!is.null(delivery_inventory)) {
          .baseline_manifest_from_inventory(
            delivery_inventory, year, download_started_at
          )
        } else {
          .baseline_manifest_from_parsed(
            downloaded$parsed, year, download_started_at
          )
        }
      }
      .write_database_year(
        connection, year, downloaded$parsed, downloaded$filesize,
        replace = TRUE, manifest = year_manifest
      )
    } else {
      message("Removing year no longer present in the API: ", year)
      .write_database_year(
        connection, year, list(), replace = TRUE,
        manifest = .empty_source_manifest()
      )
    }
  }

  unchanged_years <- setdiff(target_years, changed_years)
  if (length(unchanged_years)) {
    DBI::dbWithTransaction(connection, {
      for (year in unchanged_years) {
        DBI::dbExecute(connection, "DELETE FROM source_manifest WHERE data_year = ?",
                       params = list(as.integer(year)))
        rows <- current_manifest[current_manifest$data_year == year, , drop = FALSE]
        .append_table_by_name(connection, "source_manifest", rows)
      }
    })
  }

  .write_database_metadata(connection, time_start, Sys.time(), update_mode = "incremental")
  indexDatabase(connection = connection, dbIndexFile = dbIndexFile)
  DBI::dbDisconnect(connection, shutdown = TRUE)
  connection_open <- FALSE
  message(if (length(changed_years)) {
    paste("Updated years:", paste(changed_years, collapse = ", "))
  } else "Database is already up to date.")
  operation_succeeded <- TRUE
  invisible(list(mode = "incremental", years = changed_years))
}
