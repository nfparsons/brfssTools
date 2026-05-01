# R/crosswalk.R
#
# Thin exploration helpers on top of the new crosswalk bundle.
# Loading is in cw_load() (R/crosswalk_io.R); this file just provides
# search and lookup conveniences for the user.

#' Search the crosswalk and codebooks
#'
#' Case-insensitive regex search. Looks across concept IDs, state and CDC
#' variable names, and labels (from the codebooks). Returns a tibble of
#' matching rows with a `matched_in` column showing which field hit.
#'
#' @param query Search pattern (regex, case-insensitive).
#' @param scope Where to search:
#'   `"all"` (default), `"concept"`, `"variable"`, or `"label"`.
#' @param path  Optional config dir override.
#' @return A tibble with columns: concept_id, year, state_var, cdc_var,
#'   matched_in, plus the matching label (from cdc_codebook or state_codebook).
#' @export
#' @examples
#' \dontrun{
#' brfss_search("asthma")
#' brfss_search("ACE", scope = "concept")
#' brfss_search("CASTHNO", scope = "variable")
#' }
brfss_search <- function(query,
                         scope = c("all", "concept", "variable", "label"),
                         path  = NULL) {
  scope <- match.arg(scope)
  if (missing(query) || !nzchar(query)) {
    stop("`query` must be a non-empty regex string.", call. = FALSE)
  }
  rx <- stringr::regex(query, ignore_case = TRUE)
  bundle <- cw_load(path)

  cw <- bundle$crosswalk
  if (is.null(cw) || nrow(cw) == 0L) {
    return(tibble::tibble())
  }

  # Pull labels from the codebooks for any cdc_var/state_var on each row
  cdc_labels <- bundle$cdc_codebook |>
    dplyr::filter(!is.na(.data$label)) |>
    dplyr::group_by(.data$raw_var_name) |>
    dplyr::slice_max(.data$year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(cdc_var = "raw_var_name", cdc_label = "label")

  state_labels <- bundle$state_codebook |>
    dplyr::filter(!is.na(.data$label)) |>
    dplyr::group_by(.data$raw_var_name) |>
    dplyr::slice_max(.data$year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(state_var = "raw_var_name", state_label = "label")

  joined <- cw |>
    dplyr::left_join(cdc_labels,   by = "cdc_var") |>
    dplyr::left_join(state_labels, by = "state_var")

  hit_concept  <- stringr::str_detect(joined$concept_id, rx)
  hit_variable <- stringr::str_detect(joined$state_var, rx) |
                  stringr::str_detect(joined$cdc_var, rx)
  hit_label    <- stringr::str_detect(
    paste(joined$state_label %||% "", joined$cdc_label %||% "", sep = " | "), rx
  )
  hit_label[is.na(hit_label)] <- FALSE

  is_match <- switch(
    scope,
    all      = hit_concept | hit_variable | hit_label,
    concept  = hit_concept,
    variable = hit_variable,
    label    = hit_label
  )
  is_match[is.na(is_match)] <- FALSE

  matched_in <- dplyr::case_when(
    hit_concept  ~ "concept",
    hit_variable ~ "variable",
    hit_label    ~ "label",
    TRUE         ~ NA_character_
  )

  joined |>
    dplyr::mutate(matched_in = .env$matched_in,
                  is_match   = .env$is_match) |>
    dplyr::filter(.data$is_match) |>
    dplyr::select(
      "concept_id", "year", "state_var", "cdc_var",
      "matched_in", "state_label", "cdc_label"
    ) |>
    dplyr::arrange(.data$concept_id, .data$year)
}

#' Look up the full crosswalk for one or more concepts
#'
#' Returns every (year, state_var, cdc_var) row mapped to the requested
#' concept(s), joined with labels from both codebooks.
#'
#' @param concept_ids Character vector of concept IDs.
#' @param path        Optional config dir override.
#' @return A tibble with columns:
#'   concept_id, year, state_var, cdc_var, is_primary, source, score, notes,
#'   state_label, cdc_label.
#' @export
brfss_lookup <- function(concept_ids, path = NULL) {
  if (length(concept_ids) == 0L) return(tibble::tibble())
  bundle <- cw_load(path)

  unknown <- setdiff(concept_ids, bundle$crosswalk$concept_id)
  if (length(unknown)) {
    warning("Unknown concept_id(s): ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }

  cdc_labels <- bundle$cdc_codebook |>
    dplyr::filter(!is.na(.data$label)) |>
    dplyr::select(year = "year", cdc_var = "raw_var_name", cdc_label = "label")

  state_labels <- bundle$state_codebook |>
    dplyr::filter(!is.na(.data$label)) |>
    dplyr::select(year = "year", state_var = "raw_var_name",
                  state_label = "label")

  bundle$crosswalk |>
    dplyr::filter(.data$concept_id %in% .env$concept_ids) |>
    dplyr::left_join(cdc_labels,   by = c("year", "cdc_var")) |>
    dplyr::left_join(state_labels, by = c("year", "state_var")) |>
    dplyr::arrange(.data$concept_id, .data$year)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
