# R/parsers/parse_brfss_2012_2015.R
#
# Parser for BRFSS 2012–2015 codebooks.
# Sheets: Modules+Sections, Operational, Values.
# Modules+Sections rows are either module headers (col A filled, col C blank)
# or variable rows (col A = varname, col C = question text).
# Operational sheet is flat: Variable | Calculated? | Modules/Questions.
# Values sheet: 2 cols -> var_name | packed value-label string.

parse_brfss_2012_2015 <- function(path, year, source_file) {

  # Build value-label lookup from the Values sheet.
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

  make_value_rows <- function(var) {
    if (!var %in% names(value_map) || is.na(value_map[[var]])) {
      return(.empty_values())
    }
    vl <- .split_value_labels(value_map[[var]])
    if (nrow(vl) == 0) return(.empty_values())
    tibble::tibble(
      survey = "BRFSS", year = as.integer(year),
      raw_var_name = var,
      code = vl$code, label = vl$label, is_missing = vl$is_missing
    )
  }

  inv_list <- list()
  val_list <- list()

  # --- Modules+Sections sheet ----------------------------------------
  ms <- .read_raw_sheet(path, "Modules+Sections")
  row_is_nonblank <- apply(ms, 1, function(r)
    any(!is.na(r) & nzchar(stringr::str_trim(ifelse(is.na(r), "", r)))))

  # Skip title row AND column header row (first two non-blank rows).
  nonblank_idx <- which(row_is_nonblank)
  if (length(nonblank_idx) < 3) {
    return(list(inventory = .empty_inventory(), values = .empty_values()))
  }
  data_start <- nonblank_idx[3]

  current_module <- NA_character_
  for (i in seq(data_start, nrow(ms))) {
    r <- as.character(ms[i, ])
    col_a <- if (length(r) >= 1) .s(r[1]) else NA_character_
    col_c <- if (length(r) >= 3) .s(r[3]) else NA_character_

    if (is.na(col_a) && is.na(col_c)) next

    if (!is.na(col_a) && is.na(col_c)) {
      # Module/section header row.
      current_module <- col_a
      next
    }
    if (!is.na(col_a) && !is.na(col_c)) {
      # Variable row.
      var <- col_a
      inv_list[[length(inv_list) + 1]] <- .inv_row(
        survey = "BRFSS", year = year,
        raw_var_name = var,
        question_text = col_c,
        var_kind = "question",
        module = current_module,
        source_file = source_file
      )
      val_list[[length(val_list) + 1]] <- make_value_rows(var)
    }
  }

  # --- Operational sheet ---------------------------------------------
  if ("Operational" %in% sheets) {
    op <- .read_raw_sheet(path, "Operational")
    row_nb <- apply(op, 1, function(r)
      any(!is.na(r) & nzchar(stringr::str_trim(ifelse(is.na(r), "", r)))))
    nb_idx <- which(row_nb)
    if (length(nb_idx) >= 2) {
      for (i in seq(nb_idx[2], nrow(op))) {
        r <- as.character(op[i, ])
        var <- if (length(r) >= 1) .s(r[1]) else NA_character_
        q   <- if (length(r) >= 3) .s(r[3]) else NA_character_
        if (is.na(var)) next
        inv_list[[length(inv_list) + 1]] <- .inv_row(
          survey = "BRFSS", year = year,
          raw_var_name = var,
          question_text = q,
          var_kind = "operational",
          source_file = source_file
        )
        val_list[[length(val_list) + 1]] <- make_value_rows(var)
      }
    }
  }

  list(
    inventory = dplyr::bind_rows(inv_list),
    values    = dplyr::bind_rows(val_list)
  )
}
