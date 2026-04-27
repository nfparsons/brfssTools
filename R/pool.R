# R/pool.R
#
# Dataset-aware pool management, format-agnostic file dispatch,
# and helpers to inspect what's currently registered.

# ----------------------------------------------------------------------------
# Pool registration
# ----------------------------------------------------------------------------

#' Register a directory of raw BRFSS files for a given dataset
#'
#' @description
#' Tells `brfssTools` where the raw survey files for a particular dataset
#' live on disk. You call this once per dataset per session. Files can be
#' CSV (Oregon convention) or XPT (CDC national convention); the engine
#' dispatches on extension automatically.
#'
#' Year is detected first from the filename — files matching `LLCP{YYYY}` or
#' similar — and falls back to peeking at the leading characters of the
#' `SEQNO` column.
#'
#' Unknown dataset names produce a warning (but still work). To formalize
#' a new dataset, add it to `.brfss_dataset_registry()` in `R/datasets.R`.
#'
#' @param dataset A single character. The dataset identifier
#'   (e.g., `"National"`, `"OR"`). See [brfss_datasets()] for the list of
#'   known datasets.
#' @param path Character path to the directory containing the dataset's
#'   yearly survey files.
#' @return Invisibly, the index (named list of year -> file path).
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("OR", "Z:/Secure_Data/Oregon_BRFSS")
#' brfss_set_pool("National", "~/brfss_national")
#' brfss_pool_status()
#' }
brfss_set_pool <- function(dataset, path) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single non-empty string.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(sprintf("Directory does not exist: %s", path), call. = FALSE)
  }

  .validate_dataset(dataset)

  message(sprintf("Scanning pool for dataset '%s'...", dataset))

  files <- list.files(
    path,
    pattern = "\\.(csv|xpt)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0L) {
    warning(sprintf("No .csv or .xpt files found in: %s", path), call. = FALSE)
  }

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

  .brfss_set_pool(dataset, index)
  message(sprintf("Indexed %d file(s) for dataset '%s'.", length(index), dataset))
  invisible(index)
}

#' Show currently registered data pools
#'
#' Returns a one-row-per-(dataset, year) tibble showing which raw data
#' files are registered and where they live on disk. Empty if no pools
#' have been registered yet.
#'
#' @return A tibble with columns `dataset`, `year`, and `file`.
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("OR", "/path/to/oregon")
#' brfss_pool_status()
#' }
brfss_pool_status <- function() {
  pools <- .brfss_get("pools", list())
  if (length(pools) == 0L) {
    return(tibble::tibble(
      dataset = character(),
      year    = integer(),
      file    = character()
    ))
  }

  rows <- purrr::imap(pools, function(idx, ds) {
    if (length(idx) == 0L) {
      return(tibble::tibble(
        dataset = ds, year = NA_integer_, file = NA_character_
      ))
    }
    tibble::tibble(
      dataset = ds,
      year    = as.integer(names(idx)),
      file    = unname(unlist(idx))
    )
  })

  out <- dplyr::bind_rows(rows)
  dplyr::arrange(out, .data$dataset, .data$year)
}

#' Clear one or all registered data pools
#'
#' Removes a dataset's pool registration from the package's session
#' environment. Useful for testing and for switching between alternate
#' data folders without restarting R.
#'
#' @param dataset A single dataset name to clear, or `NULL` (default) to
#'   clear every registered pool.
#' @return Invisibly, a character vector of cleared dataset names.
#' @export
brfss_clear_pool <- function(dataset = NULL) {
  pools <- .brfss_get("pools", list())
  if (is.null(dataset)) {
    cleared <- names(pools)
    .brfss_set("pools", list())
  } else {
    if (!is.character(dataset) || length(dataset) != 1L) {
      stop("`dataset` must be NULL or a single string.", call. = FALSE)
    }
    cleared <- intersect(dataset, names(pools))
    pools[cleared] <- NULL
    .brfss_set("pools", pools)
  }
  invisible(cleared)
}

# ----------------------------------------------------------------------------
# Year detection
# ----------------------------------------------------------------------------

#' Auto-detect the BRFSS survey year from a raw data file
#'
#' Tries two strategies in order:
#' 1. Parse a 4-digit year from the filename (e.g., `LLCP2020.XPT` -> 2020).
#' 2. Peek at the `SEQNO` column and extract the leading 4 characters.
#'
#' @param file_path Full path to a `.csv` or `.xpt` file.
#' @return Integer year, or `NULL` if detection fails.
#' @export
brfss_detect_year <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)

  # Strategy 1: filename pattern (handles the 2014 trailing-space quirk too)
  fname <- basename(file_path)
  m <- regmatches(fname, regexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", fname, perl = TRUE))
  if (length(m) == 1L) {
    yr <- suppressWarnings(as.integer(m))
    if (!is.na(yr) && yr >= 1984 && yr <= 2100) return(yr)
  }

  # Strategy 2: peek at SEQNO
  peek <- .peek_pool_file(file_path, n_max = 5L)
  if (is.null(peek)) return(NULL)

  seq_col <- grep("^seqno$", names(peek), ignore.case = TRUE, value = TRUE)
  if (length(seq_col) == 0L) {
    warning(sprintf("No SEQNO column found in %s", fname), call. = FALSE)
    return(NULL)
  }

  first_seq <- as.character(peek[[seq_col[1]]][1])
  yr <- suppressWarnings(as.integer(substr(first_seq, 1L, 4L)))
  if (is.na(yr)) return(NULL)
  yr
}

# ----------------------------------------------------------------------------
# File dispatch helpers (internal)
# ----------------------------------------------------------------------------

# Internal: read a pool file in full (any year). Dispatches on extension.
.read_pool_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    "csv" = readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    "xpt" = {
      rlang::check_installed("haven", reason = "to read SAS XPT files.")
      haven::read_xpt(path)
    },
    stop(sprintf("Unsupported file extension '%s' for: %s", ext, path),
         call. = FALSE)
  )
}

# Internal: read just the first n_max rows of a pool file (for type
# inference and SEQNO peeking).
.peek_pool_file <- function(path, n_max = 5L) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    "csv" = suppressWarnings(
      readr::read_csv(path, n_max = n_max,
                      show_col_types = FALSE, progress = FALSE)
    ),
    "xpt" = {
      rlang::check_installed("haven", reason = "to read SAS XPT files.")
      suppressWarnings(haven::read_xpt(path, n_max = n_max))
    },
    {
      warning(sprintf("Unsupported file extension: %s", ext), call. = FALSE)
      NULL
    }
  )
}

# Internal: read a yearly file from a registered pool, returning a tibble.
.fetch_from_pool <- function(dataset, year) {
  index <- .brfss_get_pool(dataset)

  if (is.null(index)) {
    stop(sprintf(
      "No pool registered for dataset '%s'. Call `brfss_set_pool('%s', path)` first%s.",
      dataset, dataset,
      if (identical(dataset, "National")) " or run `brfss_download()`" else ""
    ), call. = FALSE)
  }

  path <- index[[as.character(year)]]
  if (is.null(path) || !file.exists(path)) {
    warning(sprintf(
      "No file for dataset '%s' year %s in the registered pool.",
      dataset, year
    ), call. = FALSE)
    return(NULL)
  }

  .read_pool_file(path)
}
