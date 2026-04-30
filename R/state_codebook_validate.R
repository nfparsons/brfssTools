# R/state_codebook_validate.R
#
# Validate that an incoming state codebook matches the schema brfssTools
# expects. Required columns and a few sanity checks. Surfaces errors with
# clear messages so users know what to fix.

#' Validate a state codebook tibble
#'
#' Checks that the codebook has the columns and structure brfssTools expects.
#' Returns the (possibly cleaned) tibble silently on success; errors with
#' actionable messages on failure.
#'
#' Required columns:
#'   - `year` — integer, BRFSS survey year
#'   - `raw_var_name` — character, the variable name as it appears in the
#'     state's raw data file
#'   - `label` — character, human-readable description
#'
#' Recommended columns (used if present, ignored if absent):
#'   - `value_labels` — character, "1=Yes; 2=No; 9=Refused" style
#'   - `position`, `column` — column number/letter in raw file
#'   - `missing_values` — values to treat as NA
#'
#' @param df A tibble or data.frame.
#' @param strict Logical. If TRUE, error on unrecognized columns. Default FALSE.
#' @return The validated tibble, with year coerced to integer.
#' @export
brfss_validate_state_codebook <- function(df, strict = FALSE) {
  if (!is.data.frame(df)) {
    stop("Codebook must be a data.frame or tibble.", call. = FALSE)
  }

  required <- c("year", "raw_var_name", "label")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols)) {
    stop(
      "State codebook missing required column(s): ",
      paste(missing_cols, collapse = ", "), "\n",
      "Required columns: ", paste(required, collapse = ", "), "\n",
      "Found columns:    ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  # Coerce year to integer if needed
  if (!is.numeric(df$year)) {
    coerced <- suppressWarnings(as.integer(df$year))
    if (any(is.na(coerced) & !is.na(df$year))) {
      stop("Codebook 'year' column must be coercible to integer; ",
           "found non-numeric values.", call. = FALSE)
    }
    df$year <- coerced
  } else {
    df$year <- as.integer(df$year)
  }

  # Sanity: year range
  yr_range <- range(df$year, na.rm = TRUE)
  if (yr_range[1] < 1984 || yr_range[2] > 2100) {
    warning(
      sprintf("State codebook year range is %d-%d, which looks suspect. ",
              yr_range[1], yr_range[2]),
      "BRFSS started in 1984.",
      call. = FALSE
    )
  }

  # Sanity: variable names should be character
  if (!is.character(df$raw_var_name)) {
    df$raw_var_name <- as.character(df$raw_var_name)
  }

  # Sanity: no duplicates within (year, raw_var_name)
  dup_idx <- which(duplicated(df[, c("year","raw_var_name")]))
  if (length(dup_idx)) {
    stop(
      "State codebook has duplicate (year, raw_var_name) pairs at row(s): ",
      paste(head(dup_idx, 10), collapse = ", "),
      if (length(dup_idx) > 10) sprintf(" (and %d more)", length(dup_idx) - 10) else "",
      ". Each variable must appear at most once per year.",
      call. = FALSE
    )
  }

  if (strict) {
    known_cols <- c(required,
                    "value_labels", "position", "column",
                    "missing_values", "type", "question")
    extra <- setdiff(names(df), known_cols)
    if (length(extra)) {
      stop("Strict mode: unrecognized column(s): ",
           paste(extra, collapse = ", "),
           call. = FALSE)
    }
  }

  invisible(tibble::as_tibble(df))
}
