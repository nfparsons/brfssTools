# R/inventory.R
#
# Tools for scanning a registered pool to generate the inventory and
# values tibbles that drive the master crosswalk. These are the primary
# helpers for onboarding a new dataset (e.g., adding a new state).

#' Generate a raw-variable inventory from a registered pool
#'
#' @description
#' Scans every file in a registered pool and reports the variable names,
#' positions, types, and (for XPT files) any embedded SAS labels. The
#' returned tibble has the same shape as the bundled
#' `raw_inventory_brfss.csv` — useful as a starting point for adding a
#' new dataset to the master crosswalk, or for auditing what's actually
#' in your data files.
#'
#' Type inference uses a small sample of rows by default (1000), which
#' is plenty since XPT files carry full type metadata and CSVs are
#' guessed by `readr`.
#'
#' @param dataset The dataset whose pool to scan. Must already be
#'   registered via [brfssTools::brfss_set_pool()] or
#'   [brfssTools::brfss_download()].
#' @param source Source tag to assign in the resulting `source` column.
#'   Defaults to `dataset` itself ("this variable was observed in the
#'   `<dataset>` files"). Override to `"core"` if you intend to merge
#'   the result into the shared core crosswalk.
#' @param n_max Number of rows to read from each file. Default 1000.
#'   Use a smaller value (e.g., 0 for headers only) if you trust
#'   metadata.
#' @return A tibble with columns matching `raw_inventory_brfss.csv`:
#'   `source`, `year`, `raw_var_name`, `position`, `var_kind`,
#'   `question_text`, `module`, `missing_values_raw`, `source_file`,
#'   `parse_notes`. The latter four start as `NA` for the user to fill
#'   in.
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("WA", "/path/to/washington_brfss")
#' wa_inv <- brfss_inventory_pool("WA")
#'
#' # Compare against current crosswalk to find new variables
#' cw <- brfss_crosswalk()
#' new_vars <- dplyr::anti_join(wa_inv, cw$inventory,
#'                              by = c("year", "raw_var_name"))
#' }
brfss_inventory_pool <- function(dataset, source = dataset, n_max = 1000L) {
  cfg <- .brfss_get_pool(dataset)
  if (is.null(cfg)) {
    stop(sprintf(
      "No pool registered for dataset '%s'. Call brfss_set_pool() first.",
      dataset
    ), call. = FALSE)
  }
  index <- cfg$files
  if (length(index) == 0L) {
    return(.empty_inventory())
  }

  rows <- purrr::imap(index, function(file, yr_chr) {
    yr <- as.integer(yr_chr)
    df <- .peek_pool_file(file, n_max = n_max)
    if (is.null(df)) return(NULL)

    tibble::tibble(
      source             = source,
      year               = yr,
      raw_var_name       = names(df),
      position           = seq_along(df),
      var_kind           = vapply(df, .infer_var_kind, character(1)),
      question_text      = vapply(df, .extract_label,  character(1)),
      module             = NA_character_,
      missing_values_raw = NA_character_,
      source_file        = basename(file),
      parse_notes        = NA_character_
    )
  })

  dplyr::bind_rows(rows)
}

#' Generate a raw-values inventory from a registered pool
#'
#' @description
#' Scans every file in a registered pool and reports the unique observed
#' values for each variable, with frequency counts. Useful for building
#' the value-level crosswalk (`raw_values_brfss.csv`).
#'
#' This reads each file in full, which can be slow on large national
#' files. Use [brfssTools::brfss_inventory_pool()] first if you only
#' need column names and types.
#'
#' Variables with more than `max_unique` distinct values are skipped, on
#' the assumption that they are continuous or free-text and don't belong
#' in a value lookup table.
#'
#' @param dataset The dataset whose pool to scan.
#' @param source Source tag to assign in the resulting `source` column.
#'   Defaults to `dataset`.
#' @param vars Optional character vector of raw variable names to
#'   restrict the scan to. Defaults to `NULL` (scan all variables).
#' @param max_unique Skip variables with more than this many unique
#'   values (presumed continuous or free-text). Default 50.
#' @return A tibble with columns matching `raw_values_brfss.csv`:
#'   `source`, `year`, `raw_var_name`, `code`, `label`, `is_missing`,
#'   plus an extra `n` column with the observed frequency. `label` and
#'   `is_missing` start as `NA` for the user to fill in.
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("WA", "/path/to/washington_brfss")
#' wa_vals <- brfss_values_pool("WA", vars = c("ASTHNOW", "_AGEG5YR"))
#' }
brfss_values_pool <- function(dataset,
                              source = dataset,
                              vars = NULL,
                              max_unique = 50L) {
  cfg <- .brfss_get_pool(dataset)
  if (is.null(cfg)) {
    stop(sprintf(
      "No pool registered for dataset '%s'. Call brfss_set_pool() first.",
      dataset
    ), call. = FALSE)
  }
  index <- cfg$files
  if (length(index) == 0L) {
    return(.empty_values())
  }

  rows <- purrr::imap(index, function(file, yr_chr) {
    yr <- as.integer(yr_chr)
    df <- .read_pool_file(file)

    cols <- if (is.null(vars)) names(df) else intersect(vars, names(df))

    per_col <- purrr::map(cols, function(col) {
      vec <- as.character(df[[col]])
      uniq <- unique(vec)
      if (length(uniq) > max_unique) return(NULL)

      tab <- table(vec, useNA = "ifany")
      tibble::tibble(
        source       = source,
        year         = yr,
        raw_var_name = col,
        code         = names(tab),
        label        = NA_character_,
        is_missing   = NA,
        n            = as.integer(tab)
      )
    })

    dplyr::bind_rows(per_col)
  })

  dplyr::bind_rows(rows)
}

# ----------------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------------

# Infer a coarse "kind" tag for a column based on R/haven type info.
.infer_var_kind <- function(x) {
  if (is.factor(x))                            return("categorical")
  if (!is.null(attr(x, "labels", exact = TRUE))) return("categorical")
  if (is.numeric(x))                           return("numeric")
  if (is.character(x))                         return("character")
  if (is.logical(x))                           return("logical")
  "other"
}

# Extract a haven/SAS variable label, if present (XPT files carry these;
# CSVs do not).
.extract_label <- function(x) {
  lbl <- attr(x, "label", exact = TRUE)
  if (is.null(lbl)) return(NA_character_)
  as.character(lbl)
}

# Empty-inventory schema (used when a pool is registered but empty).
.empty_inventory <- function() tibble::tibble(
  source             = character(),
  year               = integer(),
  raw_var_name       = character(),
  position           = integer(),
  var_kind           = character(),
  question_text      = character(),
  module             = character(),
  missing_values_raw = character(),
  source_file        = character(),
  parse_notes        = character()
)

# Empty-values schema.
.empty_values <- function() tibble::tibble(
  source       = character(),
  year         = integer(),
  raw_var_name = character(),
  code         = character(),
  label        = character(),
  is_missing   = logical(),
  n            = integer()
)
