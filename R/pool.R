# R/pool.R
#
# Dataset-aware pool management and file dispatch.

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
#' @param dataset A single character. The dataset identifier
#'   (e.g., `"National"`, `"OR"`). Conventionally a state's USPS abbreviation
#'   for state-restricted data, or `"National"` for the public CDC LLCP file.
#' @param path Character path to the directory containing the dataset's
#'   yearly survey files.
#' @return Invisibly, the index (named list of year -> file path).
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("OR", "Z:/Secure_Data/Oregon_BRFSS")
#' brfss_set_pool("National", "~/brfss_national")
#' }
brfss_set_pool <- function(dataset, path) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single non-empty string.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(sprintf("Directory does not exist: %s", path), call. = FALSE)
  }

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
  ext <- tolower(tools::file_ext(file_path))
  peek <- switch(
    ext,
    "csv" = suppressWarnings(
      readr::read_csv(file_path, n_max = 5L,
                      show_col_types = FALSE, progress = FALSE)
    ),
    "xpt" = {
      rlang::check_installed("haven", reason = "to read SAS XPT files.")
      suppressWarnings(haven::read_xpt(file_path, n_max = 5L))
    },
    {
      warning(sprintf("Unsupported file extension: %s", ext), call. = FALSE)
      return(NULL)
    }
  )

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

# Internal: read a yearly file from a registered pool, returning a tibble.
# Dispatches on file extension.
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
