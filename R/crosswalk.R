# R/crosswalk.R
#
# Master crosswalk loader, search, lookup, and per-vector harmonization.

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
    return(stats::setNames(character(), character()))
  }
  pairs <- stringr::str_split(rule, "\\s*;\\s*")[[1]]
  pairs <- pairs[nzchar(pairs)]
  mapping <- character(0)
  for (p in pairs) {
    kv <- stringr::str_split(p, "\\s*=\\s*", n = 2)[[1]]
    if (length(kv) != 2L) {
      stop(sprintf("Malformed inline recode pair '%s' in rule '%s'", p, rule),
           call. = FALSE)
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
    ), call. = FALSE)
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
  source              = character(),
  year                = integer(),
  raw_var_name        = character(),
  question_text_drift = character(),
  recode_type         = character(),
  recode_rule         = character(),
  notes               = character()
)

# Backwards-compat shim: older CSVs used `survey` instead of `source`.
# Silently rename and convert "BRFSS" -> "core" so legacy crosswalks load.
.coerce_source <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)
  if ("source" %in% names(df)) return(df)
  if ("survey" %in% names(df)) {
    df <- dplyr::rename(df, source = "survey")
    df$source <- ifelse(df$source == "BRFSS", "core", df$source)
  }
  df
}

# ============================================================================
# brfss_crosswalk()
# ============================================================================

#' Load the master BRFSS crosswalk
#'
#' @description
#' Reads the bundled crosswalk CSVs and returns them as a list of tibbles.
#' One master crosswalk covers every dataset; rules carry a `source` tag
#' indicating where they apply: `"core"` for rules that hold across all
#' BRFSS datasets, or a state code (e.g. `"OR"`) for state-added items.
#'
#' @param path Optional directory containing the crosswalk CSVs. Defaults
#'   to the bundled crosswalk inside the installed package.
#' @param dataset Optional character vector of datasets to filter the
#'   `concept_map` to. For example, `dataset = "OR"` keeps rules tagged
#'   `"core"` plus `"OR"`. `NULL` (default) returns the full master
#'   crosswalk unfiltered.
#' @param years Optional integer vector of years to load.
#' @return A list of tibbles: `inventory`, `values`, `concepts`,
#'   `concept_values`, and `concept_map`.
#' @export
#' @examples
#' \dontrun{
#' cw_full <- brfss_crosswalk()
#' cw_or   <- brfss_crosswalk(dataset = "OR", years = 2018:2023)
#' cw_us   <- brfss_crosswalk(dataset = "National")
#' }
brfss_crosswalk <- function(path = NULL, dataset = NULL, years = NULL) {
  lake <- "brfss"

  if (is.null(path)) {
    path <- system.file("extdata", package = "brfssTools")
    if (!nzchar(path)) {
      stop("Could not find extdata directory in brfssTools.", call. = FALSE)
    }
  }

  inv_path <- file.path(path, sprintf("raw_inventory_%s.csv", lake))
  val_path <- file.path(path, sprintf("raw_values_%s.csv", lake))
  if (!file.exists(inv_path)) stop("Missing: ", inv_path, call. = FALSE)
  if (!file.exists(val_path)) stop("Missing: ", val_path, call. = FALSE)

  inv <- readr::read_csv(
    inv_path, show_col_types = FALSE,
    col_types = readr::cols(.default = "c", year = "i", position = "i")
  )
  val <- readr::read_csv(
    val_path, show_col_types = FALSE,
    col_types = readr::cols(.default = "c", year = "i", is_missing = "l")
  )
  inv <- .coerce_source(inv)
  val <- .coerce_source(val)

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
      cm_path, show_col_types = FALSE,
      col_types = readr::cols(.default = "c", year = "i")
    )
  } else .empty_concept_map()
  concept_map <- .coerce_source(concept_map)

  # Apply dataset filter to concept_map. Each dataset gets "core" plus its
  # own state-added rules. (If a dataset has no state-added rules, the
  # extra source tag in `keep` simply matches nothing.)
  if (!is.null(dataset)) {
    keep <- unique(c("core", dataset))
    concept_map <- dplyr::filter(concept_map, .data$source %in% .env$keep)
  }

  if (!is.null(years)) {
    inv <- dplyr::filter(inv, .data$year %in% .env$years)
    val <- dplyr::filter(val, .data$year %in% .env$years)
    concept_map <- dplyr::filter(concept_map, .data$year %in% .env$years)
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

#' Text-search the crosswalk
#'
#' @param cw A loaded crosswalk object.
#' @param query Search pattern (regex). Case-insensitive.
#' @param scope Where to search: `"all"` (default), `"concept"`, `"question"`,
#'   or `"raw_var"`.
#' @return A tibble of matching variables with the field where the match
#'   occurred.
#' @export
brfss_search <- function(cw, query, scope = c("all", "concept", "question", "raw_var")) {
  scope <- match.arg(scope)
  rx <- stringr::regex(query, ignore_case = TRUE)

  joined <- cw$inventory |>
    dplyr::left_join(
      cw$concept_map |>
        dplyr::select(concept_id, source, year, raw_var_name),
      by = c("source", "year", "raw_var_name")
    ) |>
    dplyr::left_join(
      cw$concepts |>
        dplyr::select(concept_id, concept_label, construct_notes),
      by = "concept_id"
    )

  hit_concept  <- stringr::str_detect(
    paste(joined$concept_label, joined$construct_notes, sep = " | "), rx
  ) & !is.na(joined$concept_id)
  hit_question <- stringr::str_detect(joined$question_text, rx)
  hit_raw_var  <- stringr::str_detect(joined$raw_var_name, rx)

  is_match <- switch(
    scope,
    all       = hit_concept | hit_question | hit_raw_var,
    concept   = hit_concept,
    question  = hit_question,
    raw_var   = hit_raw_var
  )
  is_match[is.na(is_match)] <- FALSE

  matched_in <- dplyr::case_when(
    hit_concept  ~ "concept",
    hit_question ~ "question",
    hit_raw_var  ~ "raw_var",
    TRUE         ~ NA_character_
  )

  joined |>
    dplyr::mutate(
      matched_in = .env$matched_in,
      is_match   = .env$is_match
    ) |>
    dplyr::filter(.data$is_match) |>
    dplyr::arrange(.data$source, .data$year, .data$raw_var_name) |>
    dplyr::select(source, year, raw_var_name, question_text,
                  matched_in, concept_id)
}

# ============================================================================
# brfss_lookup()
# ============================================================================

#' Pull the full crosswalk for one or more concepts
#'
#' @param cw A loaded crosswalk object.
#' @param concept_id Character vector of concept IDs.
#' @return A tibble mapping the concept(s) to all relevant years and sources.
#' @export
brfss_lookup <- function(cw, concept_id) {
  if (length(concept_id) == 0L) return(tibble::tibble())

  unknown <- setdiff(concept_id, cw$concepts$concept_id)
  if (length(unknown)) {
    warning("Unknown concept_id(s): ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }

  cw$concept_map |>
    dplyr::filter(.data$concept_id %in% .env$concept_id) |>
    dplyr::left_join(cw$concepts, by = "concept_id") |>
    dplyr::left_join(
      cw$inventory |>
        dplyr::select(source, year, raw_var_name,
                      question_text, position, var_kind, module,
                      missing_values_raw, source_file, parse_notes),
      by = c("source", "year", "raw_var_name")
    ) |>
    dplyr::arrange(.data$concept_id, .data$source, .data$year)
}

# ============================================================================
# brfss_harmonize_vec()
# ============================================================================

#' Apply a recode rule to a raw-value vector
#'
#' @param x Raw value vector.
#' @param recode_type One of `"identity"`, `"inline"`, or `"function"`.
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
      mapping <- .parse_inline_rule(recode_rule)
      names(mapping) <- .normalize_code(names(mapping))
      idx <- match(xn, names(mapping))
      unname(mapping[idx])
    },
    "function" = {
      fn <- .get_recode_fn(recode_rule)
      as.character(fn(xn))
    },
    stop(sprintf(
      "Unknown recode_type: '%s' (expected identity / inline / function)",
      recode_type
    ), call. = FALSE)
  )
}
