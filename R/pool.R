# R/pool.R
#
# v0.2.0: Pool registration is the user's entry point. A pool ties
# together three paths:
#   - data_path:     where the raw BRFSS data files live (required)
#   - codebook_path: where the codebook CSVs live (defaults to
#                    <data_path>/documentation/)
#   - crosswalk:     full path to the crosswalk CSV (defaults to
#                    <codebook_path>/<dataset>_crosswalk.csv)
#
# Most users only set data_path; the defaults take care of the rest.
# Power users (e.g., when data lives on a read-only share) can specify
# codebook_path and crosswalk explicitly to put their work in a
# project-local folder.
#
# Pool registration is session-scoped — re-run on each R session, most
# easily via your project's setup script or .Rprofile.

# ----------------------------------------------------------------------------
# Pool registration
# ----------------------------------------------------------------------------

#' Register a BRFSS dataset for the session
#'
#' Tells brfssTools where to find the data files for a dataset, plus
#' where the codebook documentation and crosswalk live (or should be
#' created).
#'
#' Three paths are involved:
#' \itemize{
#'   \item `data_path` — the directory containing the raw data files
#'     (required)
#'   \item `codebook_path` — the directory containing codebook CSVs
#'     (defaults to `<data_path>/documentation/`)
#'   \item `crosswalk` — the full path to the crosswalk CSV (defaults
#'     to `<codebook_path>/<dataset>_crosswalk.csv`)
#' }
#'
#' Most users only set `data_path` and let the package compute sensible
#' defaults. Power users — e.g., reading from a read-only network share —
#' can set `codebook_path` and `crosswalk` explicitly to put working
#' files in a project-local folder.
#'
#' Files in `data_path` can be `.csv` or `.xpt`. Year is detected from
#' filename (`LLCP{YYYY}` and similar patterns) or from the `SEQNO`
#' column as a fallback.
#'
#' @param dataset Character. Dataset identifier (e.g., "OR", "CDC",
#'   "MA"). Used as the prefix for codebook filenames
#'   (`<dataset>_<year>_codebook.csv`) and the crosswalk filename
#'   (`<dataset>_crosswalk.csv`).
#' @param data_path Character path to the directory containing raw
#'   data files. Required.
#' @param codebook_path Optional character path. Defaults to
#'   `<data_path>/documentation/`. The directory does not have to
#'   exist yet — useful if you intend to add codebooks later.
#' @param crosswalk Optional character path to the crosswalk CSV.
#'   Defaults to `<codebook_path>/<dataset>_crosswalk.csv`. The file
#'   does not have to exist yet — `brfss_draft_crosswalk()` will
#'   create it.
#' @return The resolved pool config (data_path, codebook_path,
#'   crosswalk, files), invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Typical usage: just point at the data directory
#' brfss_set_pool("OR", "Z:/Secure_Data/Oregon_BRFSS")
#' # codebook_path defaults to Z:/Secure_Data/Oregon_BRFSS/documentation
#' # crosswalk    defaults to Z:/Secure_Data/Oregon_BRFSS/documentation/OR_crosswalk.csv
#'
#' # Power user: read-only network share for data; work in project folder
#' brfss_set_pool(
#'   dataset       = "OR",
#'   data_path     = "Z:/Secure_Data/Oregon_BRFSS",
#'   codebook_path = "C:/projects/oregon/documentation",
#'   crosswalk     = "C:/projects/oregon/OR_crosswalk.csv"
#' )
#' }
brfss_set_pool <- function(dataset, data_path,
                           codebook_path = NULL,
                           crosswalk     = NULL) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single non-empty string.", call. = FALSE)
  }
  if (missing(data_path) || !is.character(data_path) ||
      length(data_path) != 1L) {
    stop("`data_path` is required and must be a single path.",
         call. = FALSE)
  }
  if (!dir.exists(data_path)) {
    stop(sprintf("Directory does not exist: %s", data_path),
         call. = FALSE)
  }
  data_path <- normalizePath(data_path, mustWork = TRUE)

  # Resolve codebook_path
  if (is.null(codebook_path)) {
    codebook_path <- file.path(data_path, "documentation")
  } else {
    if (!is.character(codebook_path) || length(codebook_path) != 1L) {
      stop("`codebook_path` must be a single path.", call. = FALSE)
    }
    codebook_path <- normalizePath(codebook_path, mustWork = FALSE)
  }

  # Resolve crosswalk path
  if (is.null(crosswalk)) {
    crosswalk <- file.path(codebook_path,
                            sprintf("%s_crosswalk.csv", dataset))
  } else {
    if (!is.character(crosswalk) || length(crosswalk) != 1L) {
      stop("`crosswalk` must be a single path.", call. = FALSE)
    }
    crosswalk <- normalizePath(crosswalk, mustWork = FALSE)
  }

  # Scan data_path for files
  message(sprintf("Scanning data_path for dataset '%s'...", dataset))
  files <- list.files(data_path, pattern = "\\.(csv|xpt)$",
                       full.names = TRUE, ignore.case = TRUE)

  if (length(files) == 0L) {
    warning(sprintf("No .csv or .xpt files found in: %s", data_path),
            call. = FALSE)
  }

  # Detect year per file; key by year
  index <- list()
  for (f in files) {
    yr <- brfss_detect_year(f)
    if (!is.null(yr) && !is.na(yr)) {
      key <- as.character(yr)
      if (!is.null(index[[key]])) {
        warning(sprintf(
          "Multiple files detected for year %s in dataset '%s'. Keeping: %s",
          key, dataset, basename(index[[key]])
        ), call. = FALSE)
        next
      }
      index[[key]] <- f
    }
  }

  config <- list(
    dataset       = dataset,
    data_path     = data_path,
    codebook_path = codebook_path,
    crosswalk     = crosswalk,
    files         = index
  )

  .brfss_set_pool_internal(dataset, config)

  message(sprintf("Indexed %d file(s) for dataset '%s'.",
                   length(index), dataset))

  # Helpful breadcrumbs about resolved paths
  message(sprintf("  data_path:     %s", data_path))
  message(sprintf("  codebook_path: %s%s",
                   codebook_path,
                   if (!dir.exists(codebook_path)) "  (will be created on first save)"
                   else ""))
  message(sprintf("  crosswalk:     %s%s",
                   crosswalk,
                   if (!file.exists(crosswalk)) "  (does not yet exist; run brfss_draft_crosswalk())"
                   else ""))

  invisible(config)
}

# ----------------------------------------------------------------------------
# Pool inspection
# ----------------------------------------------------------------------------

#' Show the currently registered pools
#'
#' Returns a tibble listing each registered pool with one row per
#' (dataset, year) combination, plus the resolved paths.
#'
#' @return A tibble with columns `dataset`, `data_path`,
#'   `codebook_path`, `crosswalk`, `year`, `file`. Returns NULL with
#'   a message if no pools are registered.
#' @export
brfss_pool_status <- function() {
  pool_names <- .brfss_list_pools()
  if (length(pool_names) == 0L) {
    message("No pools registered. Use brfss_set_pool() to register one.")
    return(invisible(NULL))
  }

  rows <- list()
  for (ds in pool_names) {
    cfg <- .brfss_get_pool(ds)
    if (length(cfg$files) == 0L) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        dataset       = ds,
        data_path     = cfg$data_path,
        codebook_path = cfg$codebook_path,
        crosswalk     = cfg$crosswalk,
        year          = NA_integer_,
        file          = NA_character_
      )
    } else {
      for (yr in names(cfg$files)) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          dataset       = ds,
          data_path     = cfg$data_path,
          codebook_path = cfg$codebook_path,
          crosswalk     = cfg$crosswalk,
          year          = as.integer(yr),
          file          = cfg$files[[yr]]
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$dataset, out$year), ]
  out
}

#' Get the resolved pool config for a dataset
#'
#' Returns a list with `data_path`, `codebook_path`, `crosswalk`, and
#' `files` (named list of year -> file path).
#'
#' @param dataset Character dataset name. If NULL and exactly one
#'   pool is registered, returns that one.
#' @return A list with the pool config, or NULL if no pool is
#'   registered for that dataset.
#' @export
brfss_pool_get <- function(dataset = NULL) {
  if (is.null(dataset)) {
    pool_names <- .brfss_list_pools()
    if (length(pool_names) == 0L) {
      message("No pools registered.")
      return(invisible(NULL))
    }
    if (length(pool_names) > 1L) {
      stop("Multiple pools registered: ",
           paste(pool_names, collapse = ", "),
           ". Specify with `dataset =`.", call. = FALSE)
    }
    dataset <- pool_names[1]
  }
  cfg <- .brfss_get_pool(dataset)
  if (is.null(cfg)) {
    message(sprintf("No pool registered for dataset '%s'.", dataset))
    return(invisible(NULL))
  }
  cfg
}

#' Resolve a single dataset, erroring helpfully if ambiguous or unset
#' @keywords internal
.brfss_resolve_dataset <- function(dataset) {
  pool_names <- .brfss_list_pools()
  if (length(pool_names) == 0L) {
    stop("No pool registered. Run brfss_set_pool() first.", call. = FALSE)
  }
  if (is.null(dataset)) {
    if (length(pool_names) > 1L) {
      stop("Multiple pools registered: ",
           paste(pool_names, collapse = ", "),
           ". Specify which one with `dataset =`.", call. = FALSE)
    }
    return(pool_names[1])
  }
  if (!dataset %in% pool_names) {
    stop(sprintf("No pool registered for dataset '%s'. Registered: %s",
                  dataset, paste(pool_names, collapse = ", ")),
         call. = FALSE)
  }
  dataset
}

#' Drop a registered pool (or all of them)
#'
#' @param dataset Character dataset name. If NULL, drops all pools.
#' @return Invisible NULL.
#' @export
brfss_clear_pool <- function(dataset = NULL) {
  .brfss_clear_pool(dataset)
  if (is.null(dataset)) {
    message("Cleared all registered pools.")
  } else {
    message(sprintf("Cleared pool for dataset '%s'.", dataset))
  }
  invisible(NULL)
}

# ----------------------------------------------------------------------------
# Year detection (unchanged from v0.1.x)
# ----------------------------------------------------------------------------

#' Detect the year a BRFSS file represents
#'
#' Tries the filename first, then peeks at the file's `SEQNO` column.
#'
#' @param file_path Character path to a `.csv` or `.xpt` file.
#' @return An integer year, or NA if undetectable.
#' @export
brfss_detect_year <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(sprintf("File does not exist: %s", file_path), call. = FALSE)
    return(NA_integer_)
  }

  bn <- basename(file_path)
  m <- regmatches(bn, regexpr("(?<=LLCP)[0-9]{4}", bn, perl = TRUE))
  if (length(m) > 0L) return(as.integer(m))
  m <- regmatches(bn, regexpr("(20[0-9]{2})", bn, perl = TRUE))
  if (length(m) > 0L) return(as.integer(m))

  # Fall back to peeking SEQNO
  peek <- tryCatch(.peek_pool_file(file_path, n_max = 5L),
                   error = function(e) NULL)
  if (is.null(peek)) return(NA_integer_)
  if ("SEQNO" %in% names(peek)) {
    seqno <- as.character(peek$SEQNO[1])
    if (!is.na(seqno) && nchar(seqno) >= 4L) {
      yr <- as.integer(substr(seqno, 1L, 4L))
      if (!is.na(yr) && yr >= 1990L && yr <= 2100L) return(yr)
    }
  }
  NA_integer_
}

# Internal: read the file (full).
.read_pool_file <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path, show_col_types = FALSE,
                            progress = FALSE,
                            locale = readr::locale(encoding = "UTF-8")))
  }
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Reading .xpt files requires `haven`. ",
           "install.packages(\"haven\")", call. = FALSE)
    }
    return(haven::read_xpt(path))
  }
  stop("Unsupported file type: ", path, call. = FALSE)
}

# Internal: peek the first n rows.
.peek_pool_file <- function(path, n_max = 5L) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(readr::read_csv(path, n_max = n_max, show_col_types = FALSE,
                            progress = FALSE,
                            locale = readr::locale(encoding = "UTF-8")))
  }
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Reading .xpt files requires `haven`.", call. = FALSE)
    }
    return(haven::read_xpt(path, n_max = n_max))
  }
  stop("Unsupported file type: ", path, call. = FALSE)
}

# Internal: fetch a specific (dataset, year) raw file.
.fetch_from_pool <- function(dataset, year) {
  cfg <- .brfss_get_pool(dataset)
  if (is.null(cfg)) {
    stop(sprintf("No pool registered for dataset '%s'.", dataset),
         call. = FALSE)
  }
  fp <- cfg$files[[as.character(year)]]
  if (is.null(fp)) {
    stop(sprintf("No file registered for dataset '%s' year %s.",
                  dataset, year), call. = FALSE)
  }
  .read_pool_file(fp)
}
