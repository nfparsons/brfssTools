# R/parsers/parse_brfss_2016.R
#
# Parser for BRFSS 2016.
# Sheets: Modules+Variables, Values.
# Modules+Variables is already tidy: one row per variable with columns
#   Variable | Position | Label | Value Labels | Missing Values
# (Value Labels is a VLOOKUP formula -> use Values sheet for labels.)

parse_brfss_2016 <- function(path, source_file) {

  value_map <- character()
  sheets <- readxl::excel_sheets(path)
  if ("Values" %in% sheets) {
    vs <- .read_raw_sheet(path, "Values")
    if (ncol(vs) >= 2 && nrow(vs) > 0) {
      keys <- vapply(vs[[1]], .s, character(1))
      vals <- vapply(vs[[2]], .s, character(1))
      keep <- !is.na(keys)
      value_map <- setNames(vals[keep], keys[keep])
    }
  }

  mv <- .read_raw_sheet(path, "Modules+Variables")
  row_nb <- apply(mv, 1, function(r)
    any(!is.na(r) & nzchar(stringr::str_trim(ifelse(is.na(r), "", r)))))
  nb_idx <- which(row_nb)
  if (length(nb_idx) < 2) {
    return(list(inventory = .empty_inventory(), values = .empty_values()))
  }
  data_start <- nb_idx[2]  # skip header

  inv_list <- list()
  val_list <- list()

  for (i in seq(data_start, nrow(mv))) {
    r <- as.character(mv[i, ])
    var <- if (length(r) >= 1) .s(r[1]) else NA_character_
    if (is.na(var)) next
    position <- if (length(r) >= 2) .as_int_or_na(r[2]) else NA_integer_
    question <- if (length(r) >= 3) .s(r[3])            else NA_character_
    miss_raw <- if (length(r) >= 5) .s(r[5])            else NA_character_

    inv_list[[length(inv_list) + 1]] <- .inv_row(
      survey = "BRFSS", year = 2016L,
      raw_var_name = var,
      question_text = question,
      position = position,
      missing_values_raw = miss_raw,
      source_file = source_file
    )

    if (var %in% names(value_map) && !is.na(value_map[[var]])) {
      vl <- .split_value_labels(value_map[[var]])
      if (nrow(vl) > 0) {
        val_list[[length(val_list) + 1]] <- tibble::tibble(
          survey = "BRFSS", year = 2016L,
          raw_var_name = var,
          code = vl$code, label = vl$label, is_missing = vl$is_missing
        )
      }
    }
  }

  list(
    inventory = dplyr::bind_rows(inv_list),
    values    = dplyr::bind_rows(val_list)
  )
}
