# inst/dev/extract-national-metadata.R
#
# One-time extraction of variable + value-label metadata from the
# CDC National BRFSS XPT files cached locally by brfss_download().
#
# OHA's data is CSV without embedded metadata, so its codebook lives
# elsewhere (the year-specific OHA Data Dictionary xlsx files).
# CDC's XPT format DOES carry variable labels and value labels in the
# file header, so we extract them directly via haven.
#
# Output is a tidy long-format CSV with one row per (year, raw variable),
# matching the shape of the OHA codebook so the two can be joined for
# concept matching.
#
# Usage from package root:
#
#   source("inst/dev/extract-national-metadata.R")
#   extract_national_metadata(
#     cache_dir   = NULL,                    # NULL = brfss cache default
#     output_path = "cdc_codebook_long.csv"
#   )
#
# Output columns (matches oha_codebook_long.csv):
#   year, raw_var_name, position, label, value_labels, missing_values

extract_national_metadata <- function(
  cache_dir   = NULL,
  output_path = "cdc_codebook_long.csv"
) {

  for (pkg in c("haven", "dplyr", "readr", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Need package: ", pkg, call. = FALSE)
    }
  }

  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("brfssTools", "cache")
    message("Cache dir: ", cache_dir)
  }
  if (!dir.exists(cache_dir)) {
    stop("Cache dir does not exist: ", cache_dir,
         "\nDid you run brfss_download() yet?", call. = FALSE)
  }

  files <- list.files(
    cache_dir,
    pattern    = "\\.[xX][pP][tT]$",
    full.names = TRUE,
    recursive  = TRUE
  )
  if (length(files) == 0L) {
    stop("No XPT files found under: ", cache_dir, call. = FALSE)
  }
  message(sprintf("Found %d XPT file(s).", length(files)))

  detect_year <- function(fname) {
    m <- regmatches(fname, regexpr("\\d{4}", fname))
    if (length(m) == 0L) NA_integer_ else as.integer(m)
  }

  # Format haven's value labels (named vector: names = labels, values = codes)
  # into the same string shape OHA uses, e.g. "1 - Yes; 2 - No; 7 - Don't know"
  fmt_value_labels <- function(lbls) {
    if (is.null(lbls) || length(lbls) == 0L) return(NA_character_)
    codes  <- as.character(unname(lbls))
    labels <- as.character(names(lbls))
    paste(paste0(codes, " - ", labels), collapse = "; ")
  }

  get_label <- function(x) {
    l <- attr(x, "label", exact = TRUE)
    if (is.null(l)) NA_character_ else as.character(l)
  }

  process_one <- function(path) {
    yr <- detect_year(basename(path))
    if (is.na(yr)) {
      warning("Could not detect year from filename: ", basename(path),
              call. = FALSE)
      return(NULL)
    }
    message(sprintf("  %d  <-  %s", yr, basename(path)))

    df <- tryCatch(haven::read_xpt(path), error = function(e) e)
    if (inherits(df, "error")) {
      warning("Failed to read ", path, ": ", conditionMessage(df),
              call. = FALSE)
      return(NULL)
    }

    out <- tibble::tibble(
      year           = yr,
      raw_var_name   = names(df),
      position       = seq_along(df),
      label          = vapply(df, get_label, character(1)),
      value_labels   = vapply(df,
                              function(x) fmt_value_labels(
                                attr(x, "labels", exact = TRUE)),
                              character(1)),
      missing_values = NA_character_
    )

    rm(df); gc(verbose = FALSE)
    out
  }

  results <- list()
  for (f in files) {
    results[[length(results) + 1L]] <- process_one(f)
  }

  out <- dplyr::bind_rows(results)
  readr::write_csv(out, output_path, na = "")

  message(sprintf("\nDone. %d rows -> %s", nrow(out), output_path))
  message(sprintf("Years covered: %s",
                  paste(sort(unique(out$year)), collapse = ", ")))

  invisible(out)
}
