# R/crosswalk.R
#
# Crosswalk helper library for the Oregon survey projects.
# This file provides five public functions:
#
#   load_crosswalk(lake, path)      - Read the 5 CSVs into an R object
#   cw_search(cw, query, ...)       - Text search across concepts / raw vars
#   cw_lookup(cw, concept_id)       - Pull the full crosswalk for a concept
#   cw_harmonize_vec(...)           - Apply a single recode rule to a vector
#   cw_pull(cw, concept_ids, data)  - End-to-end: pull from user data,
#                                     return a long harmonized tibble
#
# IMPORTANT: before using cw_harmonize_vec / cw_pull with recode_type =
# "function", you must source R/recode_funs.R so the named functions are
# in scope.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(purrr)
})

# ============================================================================
# Private helpers
# ============================================================================

# Normalize a value code so that "1", "1.0", "1.00" all compare equal, while
# preserving non-numeric codes like "7a" and "-9.00a" untouched.
#
# Example:
#   .normalize_code(c("1", "1.00", "7a", "-9.00a", NA_character_))
#   -> c("1",  "1",    "7a", "-9.00a", NA)
#
# Why this matters: OHT 2015 stores value codes as "1.00", "2.00" etc. in the
# codebook. User data read from CSV may have these as doubles or integers,
# producing "1" after as.character(). Without normalization the recode
# lookup fails silently, dropping all values to NA.
.normalize_code <- function(x) {
  xc <- as.character(x)
  num <- suppressWarnings(as.numeric(xc))
  out <- ifelse(
    is.na(num) & !is.na(xc),
    xc,
    ifelse(
      is.na(num),
      NA_character_,
      # Format then strip trailing zeros and bare trailing dot
      sub("\\.?0+$", "", sprintf("%.6f", num))
    )
  )
  out
}

# Parse an inline recode rule string like
#   "1=1; 2=0; 7a=NA"
# into a named character vector:
#   c("1"="1", "2"="0", "7a"=NA_character_)
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

# Resolve a named recode function from the calling env's search path.
# Expects R/recode_funs.R to have been source()d first.
.get_recode_fn <- function(fn_name) {
  if (!exists(fn_name, mode = "function")) {
    stop(sprintf(
      "Recode function '%s' not found. Did you source('R/recode_funs.R') first?",
      fn_name
    ))
  }
  get(fn_name, mode = "function")
}

# Schema helpers — used when the concepts / concept_map / concept_values CSVs
# don't exist yet (fresh project). Return empty tibbles with correct columns.
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

#' Load a data lake's crosswalk into memory.
#'
#' @param lake "brfss" or "ohtshs".
#' @param path Directory containing the five CSVs. Defaults to
#'   "data/crosswalk" relative to the working directory.
#' @return A list with elements: inventory, values, concepts,
#'   concept_values, concept_map, lake. All are tibbles.
load_crosswalk <- function(lake = c("brfss", "ohtshs"),
                           path = "data/crosswalk") {
  lake <- match.arg(lake)

  # Required files: the raw inventory + values must exist.
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

  # Optional curation files: start empty, filled in by the user.
  # These are scoped per lake (each lake has its own concept vocabulary,
  # since the two lakes are never compared to each other).
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
# cw_search()
# ============================================================================

#' Text-search the crosswalk.
#'
#' @param cw A loaded crosswalk object.
#' @param query Search pattern (regex). Case-insensitive.
#' @param scope Where to search: "all" (default), "concept" (concept_label +
#'   construct_notes), "question" (raw question_text), "raw_var" (variable
#'   names only).
#' @return A tibble with columns: survey, year, raw_var_name, question_text,
#'   matched_in, concept_id (NA when unmapped).
cw_search <- function(cw, query, scope = c("all","concept","question","raw_var")) {
  scope <- match.arg(scope)
  rx <- stringr::regex(query, ignore_case = TRUE)

  # Start from the raw inventory joined to concept_map (to attach concept_id).
  joined <- cw$inventory |>
    dplyr::left_join(cw$concept_map |>
                       dplyr::select(concept_id, survey, year, raw_var_name),
                     by = c("survey", "year", "raw_var_name"))

  # Join concept metadata for concept_label / construct_notes search.
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
# cw_lookup()
# ============================================================================

#' Pull the full crosswalk for one or more concepts.
#'
#' @param cw A loaded crosswalk object.
#' @param concept_id Character vector of concept IDs.
#' @return A tibble: one row per (concept_id, survey, year, raw_var_name) with
#'   concept metadata, raw-inventory metadata, and the recode rule.
cw_lookup <- function(cw, concept_id) {
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
# cw_harmonize_vec()
# ============================================================================

#' Apply a recode rule to a raw-value vector.
#'
#' @param x Raw value vector (any type; coerced to character).
#' @param recode_type One of: "identity", "inline", "function".
#' @param recode_rule Depends on recode_type:
#'   \itemize{
#'     \item identity: ignored.
#'     \item inline: string like \code{"1=1; 2=0; 7a=NA"}.
#'     \item function: name of a function in R/recode_funs.R.
#'   }
#' @param is_missing_codes Character vector of raw codes to force to NA
#'   BEFORE applying the recode. Typically the set of \code{code}s where
#'   \code{is_missing} is TRUE in \code{raw_values}.
#' @return A character vector of harmonized values (coerce downstream if
#'   you need numeric / logical).
cw_harmonize_vec <- function(x, recode_type, recode_rule = NA_character_,
                              is_missing_codes = character()) {

  # Step 1: normalize the raw input so "1.00" matches rules written as "1".
  xn <- .normalize_code(x)

  # Step 2: force missing codes to NA.
  mc <- .normalize_code(is_missing_codes)
  xn[xn %in% mc] <- NA_character_

  # Step 3: apply the recode.
  switch(
    recode_type,
    "identity" = xn,
    "inline"   = {
      mapping    <- .parse_inline_rule(recode_rule)
      # Normalize the lookup keys too so "1" and "1.00" match.
      names(mapping) <- .normalize_code(names(mapping))
      # Use match() to preserve NA handling: unmapped -> NA, NA -> NA
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
# cw_pull()
# ============================================================================

#' Pull one or more concepts from user data, return a long harmonized tibble.
#'
#' @param cw A loaded crosswalk object.
#' @param concept_ids Character vector of concept IDs to pull.
#' @param data Named list of tibbles/data.frames, keys "<SURVEY>_<YEAR>"
#'   (e.g. \code{list("SHS_2024" = shs_2024_df, "OHT_2019" = oht_2019_df)}).
#'   Missing keys for a (survey, year) in the concept_map -> warning, skipped.
#' @param id_cols Character vector of column names to preserve from each
#'   user data frame (typically respondent IDs, weights, etc.). Must exist in
#'   ALL supplied data frames.
#' @param harmonize If TRUE (default), apply recode rules. If FALSE, return
#'   only raw values.
#' @param keep_raw If TRUE, keep the raw value alongside the harmonized one.
#' @return Long tibble. Columns:
#'   \itemize{
#'     \item \code{concept_id}
#'     \item \code{survey}, \code{year}
#'     \item one column per \code{id_cols}
#'     \item \code{raw_var_name}
#'     \item \code{raw_value} (if \code{keep_raw} or !harmonize)
#'     \item \code{harmonized_value} (if harmonize)
#'   }
cw_pull <- function(cw, concept_ids, data,
                    id_cols = character(),
                    harmonize = TRUE,
                    keep_raw = FALSE) {

  stopifnot(is.list(data), !is.null(names(data)))

  cm <- cw$concept_map |>
    dplyr::filter(concept_id %in% .env$concept_ids)
  if (nrow(cm) == 0) {
    warning("No concept_map rows for the requested concept_ids.")
    return(tibble::tibble())
  }

  # Missing-code sets per (survey, year, raw_var_name), from raw_values.
  miss_by_var <- cw$values |>
    dplyr::filter(is_missing) |>
    dplyr::group_by(survey, year, raw_var_name) |>
    dplyr::summarize(missing_codes = list(unique(code)), .groups = "drop")

  out_parts <- list()

  for (i in seq_len(nrow(cm))) {
    row   <- cm[i, ]
    key   <- sprintf("%s_%d", row$survey, row$year)
    df    <- data[[key]]
    if (is.null(df)) {
      warning(sprintf(
        "No data frame for '%s' (concept %s / var %s). Skipping.",
        key, row$concept_id, row$raw_var_name
      ))
      next
    }
    if (!(row$raw_var_name %in% names(df))) {
      warning(sprintf(
        "Variable '%s' not found in data frame '%s' (concept %s). Skipping.",
        row$raw_var_name, key, row$concept_id
      ))
      next
    }
    if (length(id_cols) > 0) {
      miss_id <- setdiff(id_cols, names(df))
      if (length(miss_id)) {
        stop(sprintf("id_cols missing from data frame '%s': %s",
                     key, paste(miss_id, collapse = ", ")))
      }
    }

    raw_vec <- df[[row$raw_var_name]]
    n <- length(raw_vec)

    # Build the output tibble with n rows from the start (tibble doesn't
    # recycle length-n into a 1-row skeleton the way data.frame does).
    part <- tibble::tibble(
      concept_id   = rep(row$concept_id,   n),
      survey       = rep(row$survey,       n),
      year         = rep(row$year,         n),
      raw_var_name = rep(row$raw_var_name, n)
    )
    for (col in id_cols) part[[col]] <- df[[col]]

    if (keep_raw || !harmonize) {
      part$raw_value <- as.character(raw_vec)
    }
    if (harmonize) {
      mc <- miss_by_var |>
        dplyr::filter(survey == row$survey, year == row$year,
                      raw_var_name == row$raw_var_name) |>
        dplyr::pull(missing_codes)
      mc <- if (length(mc) == 0) character() else mc[[1]]
      part$harmonized_value <- cw_harmonize_vec(
        raw_vec,
        recode_type = row$recode_type,
        recode_rule = row$recode_rule,
        is_missing_codes = mc
      )
    }
    out_parts[[length(out_parts) + 1]] <- part
  }

  dplyr::bind_rows(out_parts)
}
