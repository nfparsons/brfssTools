# R/parsers/_utils.R
# Shared helpers for codebook parsers.
# These functions mirror the behavior of their Python prototypes
# (see _prototype/parsers.py) so that the R outputs are identical.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(tibble)
  library(purrr)
  library(readr)
})

# -- s() --------------------------------------------------------------
# Stringify a value. Trim whitespace. Return NA_character_ for blanks.
# Matches Python _s(): returns None for None, empty, or whitespace-only.
.s <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  s <- stringr::str_trim(as.character(x))
  if (s == "") NA_character_ else s
}

# -- as_int_or_na() ---------------------------------------------------
# Coerce to integer; return NA_integer_ for anything non-numeric.
# Handles codebook quirks like "1.00" -> 1.
.as_int_or_na <- function(x) {
  s <- .s(x)
  if (is.na(s)) return(NA_integer_)
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n)) NA_integer_ else as.integer(n)
}

# -- split_value_labels() ---------------------------------------------
# Parse a packed value-label string such as:
#   "1 - Yes; 2 - No; 7a - Don't know / Not sure; 9a - Refused"
#   "-9.00a - Missing - No response; -8.00a - ..."
#
# Returns a tibble with columns: code (chr), label (chr), is_missing (lgl).
#
# Split strategy mirrors the Python version:
#   - Split on "; "  (labels in these codebooks don't contain "; ")
#   - Within each chunk, split on the FIRST " - " only
#     (labels themselves often contain " - ", e.g. "Missing - No response")
#
# is_missing heuristic:
#   - code ends in 'a' (OHA convention for missing codes), OR
#   - label starts with 'missing' (case-insensitive)
.split_value_labels <- function(packed) {
  if (is.na(packed) || !nzchar(packed)) {
    return(tibble::tibble(code = character(),
                          label = character(),
                          is_missing = logical()))
  }
  chunks <- stringr::str_split(packed, ";\\s+")[[1]]
  chunks <- stringr::str_trim(chunks)
  chunks <- chunks[nzchar(chunks)]
  if (length(chunks) == 0) {
    return(tibble::tibble(code = character(),
                          label = character(),
                          is_missing = logical()))
  }
  # Split each chunk on the first " - " (space-hyphen-space) only.
  split_once <- function(ch) {
    m <- stringr::str_locate(ch, "\\s-\\s")
    if (is.na(m[1])) {
      # Malformed chunk: treat as label-only.
      c(code = "", label = ch)
    } else {
      c(code  = stringr::str_trim(substr(ch, 1, m[1] - 1)),
        label = stringr::str_trim(substr(ch, m[2] + 1, nchar(ch))))
    }
  }
  parts <- do.call(rbind, lapply(chunks, split_once))
  code  <- unname(parts[, "code"])
  label <- unname(parts[, "label"])
  is_missing <- stringr::str_ends(code, "a") |
                stringr::str_starts(tolower(label), "missing")
  tibble::tibble(code = code, label = label, is_missing = is_missing)
}

# -- read_raw_sheet() -------------------------------------------------
# Read an entire sheet as character, no headers, blanks -> NA_character_.
# Returns a tibble where each row is one row of the sheet.
# Column names are positional: "...1", "...2", etc.
.read_raw_sheet <- function(path, sheet) {
  readxl::read_excel(
    path      = path,
    sheet     = sheet,
    col_names = FALSE,
    col_types = "text",
    trim_ws   = FALSE,          # we'll trim ourselves via .s()
    .name_repair = "minimal"
  )
}

# -- empty-inventory / empty-values constructors ----------------------
# Used by parsers to guarantee consistent column presence even when the
# parser returns nothing.
.empty_inventory <- function() {
  tibble::tibble(
    survey            = character(),
    year              = integer(),
    raw_var_name      = character(),
    question_text     = character(),
    position          = integer(),
    var_kind          = character(),
    module            = character(),
    missing_values_raw= character(),
    source_file       = character(),
    parse_notes       = character()
  )
}
.empty_values <- function() {
  tibble::tibble(
    survey       = character(),
    year         = integer(),
    raw_var_name = character(),
    code         = character(),
    label        = character(),
    is_missing   = logical()
  )
}

# -- inv_row() / val_row() --------------------------------------------
# Build a single-row tibble in the inventory or values schema.
# Used by the block-format parsers where we accumulate row-by-row.
.inv_row <- function(survey, year, raw_var_name,
                     question_text = NA_character_, position = NA_integer_,
                     var_kind = NA_character_, module = NA_character_,
                     missing_values_raw = NA_character_,
                     source_file, parse_notes = NA_character_) {
  tibble::tibble(
    survey = survey, year = as.integer(year),
    raw_var_name = raw_var_name,
    question_text = question_text,
    position = as.integer(position),
    var_kind = var_kind, module = module,
    missing_values_raw = missing_values_raw,
    source_file = source_file, parse_notes = parse_notes
  )
}
.val_row <- function(survey, year, raw_var_name, code, label, is_missing) {
  tibble::tibble(
    survey = survey, year = as.integer(year),
    raw_var_name = raw_var_name,
    code = as.character(code), label = label,
    is_missing = as.logical(is_missing)
  )
}

# -- flag_duplicate_keys() --------------------------------------------
# Some source codebooks list the same variable name twice with different
# question text (codebook-side data quality issue). Keep all rows but
# annotate parse_notes so the user can see them during curation.
flag_duplicate_keys <- function(inv) {
  if (nrow(inv) == 0) return(inv)
  inv %>%
    dplyr::group_by(survey, year, raw_var_name) %>%
    dplyr::mutate(
      .dup_n = dplyr::n(),
      parse_notes = dplyr::if_else(
        .dup_n > 1,
        dplyr::if_else(
          is.na(parse_notes) | parse_notes == "",
          sprintf("duplicate variable name in source codebook (%d rows)", .dup_n),
          paste0(parse_notes,
                 sprintf("; duplicate variable name in source codebook (%d rows)", .dup_n))
        ),
        parse_notes
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.dup_n)
}
