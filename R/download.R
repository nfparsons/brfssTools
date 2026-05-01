# R/download.R
#
# Download CDC national BRFSS LLCP files into a local cache and register
# them as the "National" pool.

# CDC URL pattern: https://www.cdc.gov/brfss/annual_data/{YYYY}/files/LLCP{YYYY}XPT.ZIP
# Zip contents:    LLCP{YYYY}.XPT (sometimes with a trailing space, e.g. 2014)

#' Default cache directory for downloaded national BRFSS files
#'
#' Returns the OS-appropriate user cache path used by `brfss_download()`.
#' Override at runtime by passing `dir` to `brfss_download()`.
#'
#' @return Character path. The directory is created on first use.
#' @export
brfss_cache_dir <- function() {
  tools::R_user_dir("brfssTools", which = "cache")
}

#' Download national CDC BRFSS LLCP files
#'
#' @description
#' Downloads the public combined landline + cell phone BRFSS data files
#' (`LLCP{YYYY}.XPT`) for the requested years, extracts them into a local
#' cache, and registers the cache as the `"National"` pool. After this runs,
#' you can immediately call `brfss_pull(..., dataset = "National")`.
#'
#' Only years 2011 and later are supported; the 2011 weighting methodology
#' change (raking + cell phone frame) makes earlier files non-comparable.
#'
#' @param year Integer scalar or vector of survey years to download.
#'   All must be `>= 2011`.
#' @param dir Cache directory. Defaults to [brfssTools::brfss_cache_dir()].
#' @param overwrite If `TRUE`, re-download files even if a local copy exists.
#'   Default `FALSE`.
#' @param quiet If `TRUE`, suppress progress messages from the underlying
#'   download. Default `FALSE`.
#' @return Invisibly, a tibble with one row per requested year and columns
#'   `year`, `xpt_path`, `status` (`"downloaded"`, `"cached"`, or `"failed"`).
#' @export
#' @examples
#' \dontrun{
#' # First-time setup, pull a few years of national data
#' brfss_download(2020:2023)
#'
#' # Now usable like any other dataset
#' cw <- brfss_crosswalk()
#' brfss_pull(cw, c("survey_weight", "fmd"),
#'            dataset = "National", states = c("OR", "WA"))
#' }
brfss_download <- function(year,
                           dir = brfss_cache_dir(),
                           overwrite = FALSE,
                           quiet = FALSE) {

  rlang::check_installed("haven", reason = "to read downloaded XPT files.")

  if (!is.numeric(year) || length(year) == 0L) {
    stop("`year` must be a non-empty numeric vector.", call. = FALSE)
  }
  year <- as.integer(year)
  bad <- year[year < 2011L]
  if (length(bad)) {
    stop(sprintf(
      "Years before 2011 are not supported (got: %s). The 2011 weighting change makes earlier data non-comparable.",
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  results <- purrr::map(year, function(yr) {
    .download_one_year(yr, dir = dir, overwrite = overwrite, quiet = quiet)
  })

  out <- dplyr::bind_rows(results)

  # Register the cache as the National pool, so brfss_pull() works immediately.
  ok <- out$status %in% c("downloaded", "cached")
  if (any(ok)) {
    brfss_set_pool(dataset = "National", data_path = dir)
  } else {
    warning("No files were successfully downloaded; National pool not registered.",
            call. = FALSE)
  }

  invisible(out)
}

# Internal: handle a single year's zip download + extraction.
# Returns a one-row tibble describing the outcome.
.download_one_year <- function(year, dir, overwrite, quiet) {

  url <- sprintf(
    "https://www.cdc.gov/brfss/annual_data/%d/files/LLCP%dXPT.ZIP",
    year, year
  )
  zip_path <- file.path(dir, sprintf("LLCP%dXPT.ZIP", year))
  xpt_target <- file.path(dir, sprintf("LLCP%d.XPT", year))

  # Already present?
  if (file.exists(xpt_target) && !overwrite) {
    return(tibble::tibble(
      year = year, xpt_path = xpt_target, status = "cached"
    ))
  }

  # Download zip
  if (!file.exists(zip_path) || overwrite) {
    if (!quiet) message(sprintf("Downloading BRFSS %d from CDC...", year))
    dl <- tryCatch(
      utils::download.file(
        url = url, destfile = zip_path,
        mode = "wb", quiet = quiet
      ),
      error = function(e) e
    )
    if (inherits(dl, "error") || (is.numeric(dl) && dl != 0L)) {
      msg <- if (inherits(dl, "error")) conditionMessage(dl) else
        sprintf("download.file returned code %s", dl)
      warning(sprintf("Failed to download %d: %s", year, msg), call. = FALSE)
      return(tibble::tibble(
        year = year, xpt_path = NA_character_, status = "failed"
      ))
    }
  }

  # Extract
  extracted <- tryCatch(
    utils::unzip(zip_path, exdir = dir),
    error = function(e) e
  )
  if (inherits(extracted, "error") || length(extracted) == 0L) {
    msg <- if (inherits(extracted, "error")) conditionMessage(extracted) else
      "unzip produced no files"
    warning(sprintf("Failed to extract %d zip: %s", year, msg), call. = FALSE)
    return(tibble::tibble(
      year = year, xpt_path = NA_character_, status = "failed"
    ))
  }

  # Find the extracted XPT (CDC's 2014 file ships with a trailing space; handle it)
  xpt_found <- extracted[grepl("\\.XPT *$", extracted, ignore.case = TRUE)]
  if (length(xpt_found) == 0L) {
    warning(sprintf("No XPT found in extracted contents for %d.", year),
            call. = FALSE)
    return(tibble::tibble(
      year = year, xpt_path = NA_character_, status = "failed"
    ))
  }

  # Normalize filename if it has a trailing space
  src <- xpt_found[1]
  if (!identical(src, xpt_target)) {
    file.rename(from = src, to = xpt_target)
  }

  if (!quiet) message(sprintf("  -> %s", xpt_target))

  tibble::tibble(
    year = year, xpt_path = xpt_target, status = "downloaded"
  )
}
