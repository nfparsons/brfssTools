# R/crosswalk_audit.R
#
# Diff a crosswalk's `core`-tagged rules against a reference dataset's
# actual variable inventory to surface candidates for state-tag retagging.
#
# Background: as the master crosswalk evolves, state-specific variables
# can accidentally end up tagged `source = "core"` instead of the state's
# code (e.g., "OR"). When `core` rules are applied during a National
# pull, the engine looks for those raw column names in LLCP files and
# finds nothing — at best harmless wasted lookups, at worst confusing
# warnings. This audit surfaces those rows so they can be retagged in a
# focused migration.

#' Audit `core`-tagged crosswalk rules against a reference dataset
#'
#' @description
#' Compares each `source = "core"` row in the crosswalk's `concept_map`
#' against the actual variable inventory of a reference dataset (typically
#' `"National"`). Rows whose `raw_var_name` does not appear in the
#' reference inventory for that year are flagged as candidates for
#' retagging to a state-specific source.
#'
#' Use this once when onboarding a National pool to clean up legacy
#' state-as-core curation noise. Inspect the returned tibble, decide
#' which rows to retag, then write a single migration script to apply
#' the changes.
#'
#' @section What "in reference" means:
#' By default (`strict_year_match = TRUE`), a row counts as "present in
#' reference" only if its `(year, raw_var_name)` pair appears in the
#' reference inventory at that exact year. Set
#' `strict_year_match = FALSE` to relax: a row counts as present if its
#' `raw_var_name` appears in the reference inventory in **any** year.
#' Strict matching is more useful for actual cleanup; relaxed matching is
#' useful for sanity-checking whether a variable is "ever" in the
#' reference at all.
#'
#' @param cw Crosswalk object from [brfssTools::brfss_crosswalk()]. To audit
#'   all `core` rows, pass a crosswalk that wasn't filtered by `dataset`.
#' @param reference_dataset Name of the registered pool to use as the
#'   reference. Default `"National"`. Must be registered via
#'   [brfssTools::brfss_set_pool()] or [brfssTools::brfss_download()].
#' @param current_source Source tag to audit. Default `"core"`.
#' @param suggested_source Source tag to suggest for rows that fail
#'   the inventory check. Default `"OR"`.
#' @param strict_year_match Logical. If `TRUE` (default), match
#'   `raw_var_name` per-year against reference. If `FALSE`, match
#'   across all years.
#' @param ignore_case Logical. If `TRUE`, match `raw_var_name` case-
#'   insensitively. Default `FALSE` (BRFSS XPT files use stable
#'   uppercase, so a case mismatch is a real curation issue worth
#'   surfacing).
#' @return A tibble with one row per audited `concept_map` row. Columns:
#'   `concept_id`, `year`, `raw_var_name`, `source`, `in_reference`,
#'   `reference_years_seen` (comma-separated list, or `NA`),
#'   `suggested_source`, `needs_action` (`TRUE` iff
#'   `source != suggested_source`).
#' @export
#' @examples
#' \dontrun{
#' brfss_download(2012:2024)
#' cw <- brfss_crosswalk()  # no dataset filter -> all rules
#'
#' audit <- brfss_crosswalk_audit(cw)
#'
#' # Inspect candidates for retagging
#' library(dplyr)
#' audit |>
#'   filter(needs_action) |>
#'   count(raw_var_name, suggested_source, sort = TRUE)
#'
#' # Or look at a specific concept's audit verdict across years
#' audit |> filter(concept_id == "wtrk_all")
#' }
brfss_crosswalk_audit <- function(cw,
                                  reference_dataset = "National",
                                  current_source = "core",
                                  suggested_source = "OR",
                                  strict_year_match = TRUE,
                                  ignore_case = FALSE) {

  if (!is.list(cw) || is.null(cw$concept_map)) {
    stop("`cw` must be a crosswalk object from brfss_crosswalk().",
         call. = FALSE)
  }

  # Pull the rows we want to audit
  audit_rows <- dplyr::filter(cw$concept_map,
                              .data$source == .env$current_source)
  if (nrow(audit_rows) == 0L) {
    message(sprintf(
      "No rows tagged source = '%s' found in the crosswalk; nothing to audit.",
      current_source
    ))
    return(.empty_audit())
  }

  # Get the reference inventory (will error informatively if pool not
  # registered)
  inv <- brfss_inventory_pool(reference_dataset, source = reference_dataset,
                              n_max = 0L)
  if (nrow(inv) == 0L) {
    stop(sprintf(
      "Reference pool '%s' is registered but its inventory is empty.",
      reference_dataset
    ), call. = FALSE)
  }

  # Normalize keys for comparison
  norm <- if (ignore_case) toupper else identity

  inv_keys <- inv |>
    dplyr::transmute(
      year = as.integer(.data$year),
      raw_var_name = norm(.data$raw_var_name)
    ) |>
    dplyr::distinct()

  audit_keys <- audit_rows |>
    dplyr::mutate(
      raw_var_name_norm = norm(.data$raw_var_name),
      year              = as.integer(.data$year)
    )

  # ------ Strict year match: lookup (year, raw_var_name) ------
  if (strict_year_match) {
    inv_lookup <- inv_keys |>
      dplyr::transmute(
        year = .data$year,
        raw_var_name_norm = .data$raw_var_name,
        in_reference = TRUE
      )
    audit_keys <- audit_keys |>
      dplyr::left_join(inv_lookup, by = c("year", "raw_var_name_norm")) |>
      dplyr::mutate(in_reference = !is.na(.data$in_reference))
  } else {
    # ----- Relaxed match: variable appears in reference in any year -----
    audit_keys$in_reference <-
      audit_keys$raw_var_name_norm %in% inv_keys$raw_var_name
  }

  # Always compute the years-seen list (independent of strict mode) so
  # the user can see year coverage of the variable in the reference
  yr_seen <- inv_keys |>
    dplyr::group_by(.data$raw_var_name) |>
    dplyr::summarise(
      reference_years_seen = paste(sort(unique(.data$year)), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::rename(raw_var_name_norm = "raw_var_name")

  audit_keys <- audit_keys |>
    dplyr::left_join(yr_seen, by = "raw_var_name_norm") |>
    dplyr::mutate(
      suggested_source = dplyr::if_else(
        .data$in_reference,
        .data$source,
        .env$suggested_source
      ),
      needs_action = .data$source != .data$suggested_source
    ) |>
    dplyr::select(
      "concept_id", "year", "raw_var_name", "source",
      "in_reference", "reference_years_seen",
      "suggested_source", "needs_action"
    )

  audit_keys
}

# Empty-result skeleton (shape matches the populated case)
.empty_audit <- function() tibble::tibble(
  concept_id           = character(),
  year                 = integer(),
  raw_var_name         = character(),
  source               = character(),
  in_reference         = logical(),
  reference_years_seen = character(),
  suggested_source     = character(),
  needs_action         = logical()
)
