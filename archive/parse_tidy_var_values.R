# R/parsers/parse_tidy_var_values.R
#
# Parser for the tidy "Variables" + "Values" two-sheet layout.
# Used for: BRFSS 2017–2024, OHT 2019, SHS 2020, SHS 2022 (9 codebooks).
#
# Variables sheet columns (order varies by year):
#   Variable | Position | Label | Missing Values | Values        (recent)
#   Variable | Position | Label | Value Labels   | Missing Values (older)
# Values sheet: 2 cols -> var_name | packed value-label string
#
# Value-label sourcing strategy:
#   The Values column in the Variables sheet often holds a VLOOKUP formula.
#   readxl (and openpyxl's data_only mode) return the cached evaluated text
#   when Excel has saved it. The Values sheet has proven UNRELIABLE in
#   some years (BRFSS 2020: 276 of 337 Values-sheet entries do not match
#   any variable in the Variables sheet). So:
#     1. PREFER inline cached value labels from the Variables sheet.
#     2. FALL BACK to the Values sheet only when inline is missing.
#   Variables with neither source are emitted with zero value rows and
#   a parse_notes flag.

parse_tidy_var_values <- function(path, survey, year, source_file) {

  sheets <- readxl::excel_sheets(path)
  if (!"Variables" %in% sheets) {
    stop(sprintf("[%s] no 'Variables' sheet; wrong parser", source_file))
  }

  # Build fallback map from the Values sheet (may be stale / incomplete).
  value_fallback <- character()
  if ("Values" %in% sheets) {
    vs <- .read_raw_sheet(path, "Values")
    if (ncol(vs) >= 2 && nrow(vs) > 0) {
      keys <- vapply(vs[[1]], .s, character(1))
      vals <- vapply(vs[[2]], .s, character(1))
      keep <- !is.na(keys)
      value_fallback <- setNames(vals[keep], keys[keep])
    }
  }

  vs <- .read_raw_sheet(path, "Variables")
  if (nrow(vs) == 0) {
    return(list(inventory = .empty_inventory(), values = .empty_values()))
  }

  # Find header row: first row that has any non-blank cell.
  row_is_nonblank <- apply(vs, 1, function(r) any(!is.na(r) & nzchar(stringr::str_trim(r))))
  if (!any(row_is_nonblank)) {
    return(list(inventory = .empty_inventory(), values = .empty_values()))
  }
  header_idx <- which(row_is_nonblank)[1]
  header <- vapply(vs[header_idx, ], function(x) {
    v <- .s(x); if (is.na(v)) "" else v
  }, character(1))

  # Locate columns by case-insensitive name.
  find_col <- function(names) {
    lower <- tolower(header)
    for (n in names) {
      hit <- which(lower == tolower(n))
      if (length(hit)) return(hit[1])
    }
    NA_integer_
  }
  idx_var     <- find_col(c("Variable", "Variable Name"))
  idx_pos     <- find_col("Position")
  idx_label   <- find_col(c("Label", "Variable Label"))
  idx_missing <- find_col("Missing Values")
  idx_values  <- find_col(c("Value Labels", "Values"))

  if (is.na(idx_var) || is.na(idx_label)) {
    stop(sprintf("[%s] can't find Variable/Label cols in header: %s",
                 source_file, paste(header, collapse = " | ")))
  }

  # Iterate data rows.
  data_rows <- vs[(header_idx + 1):nrow(vs), , drop = FALSE]
  inv_list <- vector("list", nrow(data_rows))
  val_list <- vector("list", nrow(data_rows))

  for (i in seq_len(nrow(data_rows))) {
    r <- as.character(data_rows[i, ])
    if (all(is.na(r) | !nzchar(stringr::str_trim(ifelse(is.na(r), "", r))))) next

    var <- .s(r[idx_var])
    if (is.na(var)) next

    question     <- .s(r[idx_label])
    position     <- if (!is.na(idx_pos))     .as_int_or_na(r[idx_pos])     else NA_integer_
    missing_raw  <- if (!is.na(idx_missing)) .s(r[idx_missing])             else NA_character_
    inline       <- if (!is.na(idx_values))  .s(r[idx_values])              else NA_character_

    # Source selection.
    packed <- NA_character_
    if (!is.na(inline) && !stringr::str_starts(inline, "=")) {
      packed <- inline
    } else if (var %in% names(value_fallback) && !is.na(value_fallback[[var]])) {
      packed <- value_fallback[[var]]
    }

    parse_notes <- if (is.na(packed)) {
      "no value labels in Variables sheet or Values sheet"
    } else NA_character_

    inv_list[[i]] <- .inv_row(
      survey = survey, year = year,
      raw_var_name = var,
      question_text = question,
      position = position,
      missing_values_raw = missing_raw,
      source_file = source_file,
      parse_notes = parse_notes
    )

    vl <- .split_value_labels(packed)
    if (nrow(vl) > 0) {
      val_list[[i]] <- tibble::tibble(
        survey = survey, year = as.integer(year),
        raw_var_name = var,
        code = vl$code, label = vl$label, is_missing = vl$is_missing
      )
    }
  }

  list(
    inventory = dplyr::bind_rows(purrr::compact(inv_list)),
    values    = dplyr::bind_rows(purrr::compact(val_list))
  )
}
