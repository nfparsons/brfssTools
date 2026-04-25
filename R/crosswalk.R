# R/crosswalk.R
#
# Crosswalk helper library for the brfssTools package.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(purrr)
})

# ============================================================================
# Global Configuration (BYOD Workflow)
# ============================================================================

#' Set the directory for raw BRFSS survey data
#'
#' @description
#' Tells `brfssTools` where your local pool of raw survey data is stored.
#' This directory should contain the yearly survey files saved as `.csv`
#' objects (e.g., "BRFSS_2020.csv").
#'
#' @param path Character string pointing to the local data pool.
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("Z:/Secure_Data/Oregon_Surveys/Harmonized_Pool")
#' }
brfss_set_pool <- function(path) {
  if (!dir.exists(path)) {
    stop("The specified directory does not exist.", call. = FALSE)
  }

  message("Scanning and indexing survey pool...")

  # Find all CSVs in the folder
  csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

  if (length(csv_files) == 0) {
    warning("No .csv files found in the specified directory.")
  }

  # Build an index mapping the year to the specific file path
  pool_index <- list()
  for (f in csv_files) {
    # FIXED: Updated to the new brfss_ prefix
    yr <- brfss_detect_year(f)
    if (!is.null(yr)) {
      pool_index[[as.character(yr)]] <- f
    }
  }

  # Save the index to global options
  options(brfss_survey_index = pool_index)
  message(sprintf("Successfully indexed %d BRFSS files.", length(pool_index)))
}

# Internal helper to fetch data from the pool on the fly
.fetch_from_pool <- function(survey, year) {
  pool_index <- getOption("brfss_survey_index")

  if (is.null(pool_index)) {
    stop("Data pool not set. Please run `brfss_set_pool('path/to/dir')` first.", call. = FALSE)
  }

  # Look up the exact file path for this year from our auto-detected index
  exact_file <- pool_index[[as.character(year)]]

  if (is.null(exact_file) || !file.exists(exact_file)) {
    warning(sprintf("Could not find data for year %s in the survey pool.", year), call. = FALSE)
    return(NULL)
  }

  readr::read_csv(exact_file, show_col_types = FALSE)
}

# ============================================================================
# Private helpers
# ============================================================================

.normalize_code <- function(x) {
  xc <- as.character(x)
  num <- suppressWarnings(as.numeric(xc))
  out <- ifelse(
    is.na(num) & !is.na(xc),
    xc,
    ifelse(
      is.na(num),
      NA_character_,
      sub("\\.?0+$", "", sprintf("%.6f", num))
    )
  )
  out
}

.parse_inline_rule <- function(rule) {
  if (is.na(rule) || !nzchar(rule)) {
    return(setNames(character(), character()))
  }
  pairs <- stringr::str_split(rule, "\\s*;\\s*")[[1]]
  pairs <- pairs[nzchar(pairs)]
  mapping <- character(0)
  for (p in pairs) {
    kv <- stringr::str_split(p, "\\s*=\\s*", n = 2)[[1]]
    if (length(kv) != 2) {
      stop(sprintf("Malformed inline recode pair '%s' in rule '%s'", p, rule))
    }
    k <- stringr::str_trim(kv[1])
    v <- stringr::str_trim(kv[2])
    mapping[[k]] <- if (toupper(v) == "NA") NA_character_ else v
  }
  mapping
}

.get_recode_fn <- function(fn_name) {
  if (!exists(fn_name, mode = "function")) {
    stop(sprintf(
      "Recode function '%s' not found. Ensure it is exported in brfssTools.",
      fn_name
    ))
  }
  get(fn_name, mode = "function")
}

.empty_concepts <- function() tibble::tibble(
  concept_id              = character(),
  concept_label           = character(),
  domain                  = character(),
  subdomain               = character(),
  construct_notes         = character(),
  canonical_response_type = character()
)

.empty_concept_values <- function() tibble::tibble(
  concept_id = character(),
  code       = character(),
  label      = character()
)

.empty_concept_map <- function() tibble::tibble(
  concept_id          = character(),
  survey              = character(),
  year                = integer(),
  raw_var_name        = character(),
  question_text_drift = character(),
  recode_type         = character(),
  recode_rule         = character(),
  notes               = character()
)

# ============================================================================
# load_crosswalk()
# ============================================================================

#' Load the BRFSS crosswalk into memory.
#'
#' @param path Directory containing the CSVs. Defaults to NULL, which automatically
#'   finds the bundled crosswalk inside the installed brfssTools package.
#' @param years Optional integer vector of years to load (e.g., c(2018, 2019)).
#' @return A list of tibbles representing the crosswalk schema.
#' @export
brfss_crosswalk <- function(path = NULL, years = NULL) {
  lake <- "brfss"

  # MAGIC HAPPENS HERE: Automatically find the package's internal CSVs
  if (is.null(path)) {
    path <- system.file("extdata", package = "brfssTools")
    if (path == "") stop("Could not find the extdata directory in brfssTools.")
  }

  inv_path <- file.path(path, sprintf("raw_inventory_%s.csv", lake))
  val_path <- file.path(path, sprintf("raw_values_%s.csv", lake))
  if (!file.exists(inv_path)) stop("Missing: ", inv_path)
  if (!file.exists(val_path)) stop("Missing: ", val_path)

  inv <- readr::read_csv(
    inv_path,
    col_types = readr::cols(
      survey = "c", year = "i", raw_var_name = "c",
      question_text = "c", position = "i", var_kind = "c",
      module = "c", missing_values_raw = "c",
      source_file = "c", parse_notes = "c"
    ),
    show_col_types = FALSE
  )
  val <- readr::read_csv(
    val_path,
    col_types = readr::cols(
      survey = "c", year = "i", raw_var_name = "c",
      code = "c", label = "c", is_missing = "l"
    ),
    show_col_types = FALSE
  )

  concepts_path <- file.path(path, sprintf("concepts_%s.csv", lake))
  concepts <- if (file.exists(concepts_path)) {
    readr::read_csv(concepts_path, col_types = readr::cols(.default = "c"),
                    show_col_types = FALSE)
  } else .empty_concepts()

  cv_path <- file.path(path, sprintf("concept_values_%s.csv", lake))
  concept_values <- if (file.exists(cv_path)) {
    readr::read_csv(cv_path, col_types = readr::cols(.default = "c"),
                    show_col_types = FALSE)
  } else .empty_concept_values()

  cm_path <- file.path(path, sprintf("concept_map_%s.csv", lake))
  concept_map <- if (file.exists(cm_path)) {
    readr::read_csv(
      cm_path,
      col_types = readr::cols(
        concept_id = "c", survey = "c", year = "i",
        raw_var_name = "c", question_text_drift = "c",
        recode_type = "c", recode_rule = "c", notes = "c"
      ),
      show_col_types = FALSE
    )
  } else .empty_concept_map()

  if (!is.null(years)) {
    inv <- dplyr::filter(inv, year %in% years)
    val <- dplyr::filter(val, year %in% years)
    concept_map <- dplyr::filter(concept_map, year %in% years)
  }

  list(
    inventory      = inv,
    values         = val,
    concepts       = concepts,
    concept_values = concept_values,
    concept_map    = concept_map,
    lake           = lake
  )
}

# ============================================================================
# brfss_search()
# ============================================================================

#' Text-search the crosswalk.
#'
#' @param cw A loaded crosswalk object.
#' @param query Search pattern (regex). Case-insensitive.
#' @param scope Where to search: "all" (default), "concept", "question", or "raw_var".
#' @return A tibble of matched variables and concepts.
#' @export
brfss_search <- function(cw, query, scope = c("all","concept","question","raw_var")) {
  scope <- match.arg(scope)
  rx <- stringr::regex(query, ignore_case = TRUE)

  joined <- cw$inventory |>
    dplyr::left_join(cw$concept_map |>
                       dplyr::select(concept_id, survey, year, raw_var_name),
                     by = c("survey", "year", "raw_var_name"))

  joined <- joined |>
    dplyr::left_join(
      cw$concepts |>
        dplyr::select(concept_id, concept_label, construct_notes),
      by = "concept_id"
    )

  match_concept  <- stringr::str_detect(
    paste(joined$concept_label, joined$construct_notes, sep = " | "), rx
  ) & !is.na(joined$concept_id)
  match_question <- stringr::str_detect(joined$question_text, rx)
  match_raw_var  <- stringr::str_detect(joined$raw_var_name, rx)

  match <- switch(
    scope,
    all       = match_concept | match_question | match_raw_var,
    concept   = match_concept,
    question  = match_question,
    raw_var   = match_raw_var
  )
  match[is.na(match)] <- FALSE

  matched_in <- dplyr::case_when(
    match_concept  ~ "concept",
    match_question ~ "question",
    match_raw_var  ~ "raw_var",
    TRUE           ~ NA_character_
  )

  joined |>
    dplyr::mutate(matched_in = matched_in) |>
    dplyr::filter(match) |>
    dplyr::arrange(survey, year, raw_var_name) |>
    dplyr::select(survey, year, raw_var_name, question_text,
                  matched_in, concept_id)
}

# ============================================================================
# brfss_lookup()
# ============================================================================

#' Pull the full crosswalk for one or more concepts.
#'
#' @param cw A loaded crosswalk object.
#' @param concept_id Character vector of concept IDs.
#' @return A tibble mapping the concept to all relevant years and surveys.
#' @export
brfss_lookup <- function(cw, concept_id) {
  if (length(concept_id) == 0) return(tibble::tibble())

  unknown <- setdiff(concept_id, cw$concepts$concept_id)
  if (length(unknown)) {
    warning("Unknown concept_id(s): ", paste(unknown, collapse = ", "))
  }

  cw$concept_map |>
    dplyr::filter(concept_id %in% .env$concept_id) |>
    dplyr::left_join(cw$concepts, by = "concept_id") |>
    dplyr::left_join(
      cw$inventory |> dplyr::select(survey, year, raw_var_name,
                                    question_text, position, var_kind, module,
                                    missing_values_raw, source_file, parse_notes),
      by = c("survey", "year", "raw_var_name")
    ) |>
    dplyr::arrange(concept_id, survey, year)
}

# ============================================================================
# brfss_harmonize_vec()
# ============================================================================

#' Apply a recode rule to a raw-value vector.
#'
#' @param x Raw value vector.
#' @param recode_type "identity", "inline", or "function".
#' @param recode_rule The string rule or function name.
#' @param is_missing_codes Character vector of raw codes to force to NA.
#' @return A character vector of harmonized values.
#' @export
brfss_harmonize_vec <- function(x, recode_type, recode_rule = NA_character_,
                                is_missing_codes = character()) {

  xn <- .normalize_code(x)
  mc <- .normalize_code(is_missing_codes)
  xn[xn %in% mc] <- NA_character_

  switch(
    recode_type,
    "identity" = xn,
    "inline"   = {
      mapping    <- .parse_inline_rule(recode_rule)
      names(mapping) <- .normalize_code(names(mapping))
      idx <- match(xn, names(mapping))
      unname(mapping[idx])
    },
    "function" = {
      fn <- .get_recode_fn(recode_rule)
      as.character(fn(xn))
    },
    stop(sprintf("Unknown recode_type: '%s' (expected identity / inline / function)",
                 recode_type))
  )
}

# ============================================================================
# brfss_pull()
# ============================================================================

#' Pull one or more concepts from user data.
#'
#' @param cw A loaded crosswalk object.
#' @param concept_ids Character vector of concept IDs to pull.
#' @param data Optional. Named list of tibbles/data.frames.
#' @param id_cols Character vector of column names to preserve (e.g., "SEQNO").
#' @param harmonize If TRUE (default), apply recode rules.
#' @param keep_raw If TRUE, keep the raw value alongside the harmonized one.
#' @param output String: "long" (default) or "wide".
#' @export
brfss_pull <- function(cw, concept_ids, data = NULL,
                       id_cols = character(),
                       harmonize = TRUE,
                       keep_raw = FALSE,
                       output = c("long", "wide")) {

  output <- match.arg(output)

  cm <- cw$concept_map |>
    dplyr::filter(concept_id %in% .env$concept_ids)

  if (nrow(cm) == 0) {
    warning("No concept_map rows for the requested concept_ids.")
    return(tibble::tibble())
  }

  miss_by_var <- cw$values |>
    dplyr::filter(is_missing) |>
    dplyr::group_by(survey, year, raw_var_name) |>
    dplyr::summarize(missing_codes = list(unique(code)), .groups = "drop")

  out_parts <- list()

  for (i in seq_len(nrow(cm))) {
    row   <- cm[i, ]
    key   <- sprintf("%s_%d", row$survey, row$year)

    df <- if (!is.null(data)) data[[key]] else .fetch_from_pool(row$survey, row$year)

    if (is.null(df)) next
    if (!(row$raw_var_name %in% names(df))) next

    raw_vec <- df[[row$raw_var_name]]
    n <- length(raw_vec)

    part <- tibble::tibble(
      concept_id   = rep(row$concept_id,   n),
      survey       = rep(row$survey,       n),
      year         = rep(row$year,         n),
      raw_var_name = rep(row$raw_var_name, n)
    )
    for (col in id_cols) part[[col]] <- df[[col]]

    if (keep_raw || !harmonize) part$raw_value <- as.character(raw_vec)

    if (harmonize) {
      mc <- miss_by_var |>
        dplyr::filter(survey == row$survey, year == row$year, raw_var_name == row$raw_var_name) |>
        dplyr::pull(missing_codes)
      mc <- if (length(mc) == 0) character() else mc[[1]]

      # Now correctly calls brfss_harmonize_vec
      part$harmonized_value <- brfss_harmonize_vec(
        raw_vec, recode_type = row$recode_type,
        recode_rule = row$recode_rule, is_missing_codes = mc
      )
    }
    out_parts[[length(out_parts) + 1]] <- part
  }

  long_df <- dplyr::bind_rows(out_parts)

  if (output == "wide" && nrow(long_df) > 0) {
    val_cols <- if (keep_raw && harmonize) c("harmonized_value", "raw_value") else if (harmonize) "harmonized_value" else "raw_value"

    wide_df <- long_df |>
      dplyr::select(survey, year, dplyr::all_of(id_cols), concept_id, dplyr::all_of(val_cols)) |>
      tidyr::pivot_wider(
        names_from = concept_id,
        values_from = dplyr::all_of(val_cols)
      )

    if (length(val_cols) == 1) {
      names(wide_df) <- gsub(paste0("^", val_cols, "_"), "", names(wide_df))
    }
    return(wide_df)
  }

  long_df
}

# ============================================================================
# brfss_detect_year()
# ============================================================================

#' Auto-detect the BRFSS survey year from a raw CSV file
#'
#' Peeks at the first 5 rows of a CSV to find the SEQNO column and
#' extracts the 4-digit survey year from the leading characters.
#'
#' @param file_path Full path to the CSV file.
#' @return Integer representing the survey year (e.g., 2018), or NULL if it fails.
#' @export
brfss_detect_year <- function(file_path) {

  if (!file.exists(file_path)) return(NULL)

  # Read ONLY the first 5 rows to make this lightning fast
  peek <- suppressWarnings(
    readr::read_csv(file_path, n_max = 5, show_col_types = FALSE, progress = FALSE)
  )

  # Find the SEQNO column (ignoring case, just in case)
  seq_col <- grep("^seqno", names(peek), ignore.case = TRUE, value = TRUE)

  if (length(seq_col) == 0) {
    warning(sprintf("Could not find a SEQNO column in %s", basename(file_path)))
    return(NULL)
  }

  # Extract the first 4 characters of the first row's SEQNO
  first_seq <- as.character(peek[[seq_col[1]]][1])
  year_str <- substr(first_seq, 1, 4)

  # Convert to integer and return
  as.integer(year_str)
}
