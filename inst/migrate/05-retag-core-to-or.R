# inst/migrate/05-retag-core-to-or.R
#
# Migration 05: retag rows currently tagged `source = "core"` whose
# raw_var_name is not in National at the mapped year, but IS in OR at
# the mapped year. These are OR-specific variables that got mis-tagged
# as core during the original schema migration.
#
# Sourcing this file does nothing. Call the function explicitly:
#
#   source("inst/migrate/05-retag-core-to-or.R")
#   migrate_05_retag_core_to_or()           # dry run, no file changes
#   migrate_05_retag_core_to_or(apply = TRUE)  # actually rewrite the CSV
#
# Inputs:
#   - inst/extdata/concept_map_brfss.csv (read, optionally rewritten)
#   - National pool (must be downloaded; see brfss_download())
#   - OR pool (must be registered)
#
# Outputs (when apply = TRUE):
#   - inst/extdata/concept_map_brfss.csv overwritten with retagged rows
#   - inst/migrate/05-phantom-review.csv created with the rows that
#     can't be confidently retagged (kept as core, surfaced for review)

migrate_05_retag_core_to_or <- function(
  concept_map_path = "inst/extdata/concept_map_brfss.csv",
  phantom_review_path = "inst/migrate/05-phantom-review.csv",
  apply = FALSE
) {

  stopifnot(requireNamespace("brfssTools", quietly = TRUE))
  stopifnot(requireNamespace("dplyr",      quietly = TRUE))
  stopifnot(requireNamespace("readr",      quietly = TRUE))

  if (!file.exists(concept_map_path)) {
    stop("Cannot find concept_map at: ", concept_map_path,
         "\nAre you running from the package root?", call. = FALSE)
  }

  message("Building National crosswalk for audit...")
  cw <- brfssTools::brfss_crosswalk(
    dataset = "National",
    year    = 2012:2024
  )

  message("Running crosswalk audit...")
  audit <- brfssTools::brfss_crosswalk_audit(cw)

  message("Loading OR inventory...")
  or_inv <- brfssTools::brfss_inventory_pool("OR") |>
    dplyr::distinct(raw_var_name, year) |>
    dplyr::mutate(in_or = TRUE)

  # Build retag plan: needs_action rows that ARE in OR get retagged.
  # Rows that are NOT in OR are phantoms (kept as core, surfaced).
  retag_plan <- audit |>
    dplyr::filter(needs_action) |>
    dplyr::left_join(or_inv, by = c("raw_var_name", "year")) |>
    dplyr::mutate(in_or = dplyr::coalesce(in_or, FALSE))

  to_retag <- retag_plan |> dplyr::filter(in_or)
  phantoms <- retag_plan |> dplyr::filter(!in_or)

  message(sprintf("Plan: retag %d row(s); leave %d phantom(s) as core.",
                  nrow(to_retag), nrow(phantoms)))

  # Load concept_map
  message("Reading concept_map...")
  cm <- readr::read_csv(concept_map_path, show_col_types = FALSE)
  before_counts <- dplyr::count(cm, source)

  # Build join key. Match on the full quartet (concept_id, year,
  # raw_var_name, source = "core") so we don't accidentally retag a
  # row that is already correctly tagged as OR.
  retag_keys <- to_retag |>
    dplyr::distinct(concept_id, year, raw_var_name) |>
    dplyr::mutate(source = "core", .do_retag = TRUE)

  cm_new <- cm |>
    dplyr::left_join(
      retag_keys,
      by = c("concept_id", "year", "raw_var_name", "source")
    ) |>
    dplyr::mutate(
      source = dplyr::if_else(
        !is.na(.data$.do_retag) & .data$.do_retag,
        "OR",
        .data$source
      )
    ) |>
    dplyr::select(-.do_retag)

  after_counts <- dplyr::count(cm_new, source)
  retag_count  <- sum(cm$source == "core") - sum(cm_new$source == "core")

  # Sanity check 1: total row count must be unchanged
  if (nrow(cm) != nrow(cm_new)) {
    stop("Row count changed during retag (", nrow(cm), " -> ",
         nrow(cm_new), "). Aborting.", call. = FALSE)
  }

  # Sanity check 2: retag count must match plan
  if (retag_count != nrow(to_retag)) {
    warning(sprintf(
      "Retag count mismatch: plan said %d, actual was %d. Investigate before applying.",
      nrow(to_retag), retag_count
    ))
  }

  # Sanity check 3: no duplicate rows after retag
  dups <- cm_new |>
    dplyr::count(concept_id, source, year, raw_var_name) |>
    dplyr::filter(n > 1)
  if (nrow(dups) > 0) {
    warning(sprintf(
      "Retag created %d duplicate (concept_id, source, year, raw_var_name) row(s). Investigate before applying.",
      nrow(dups)
    ))
  }

  # Report
  cat("\n--- BEFORE ---\n"); print(before_counts)
  cat("\n--- AFTER ---\n");  print(after_counts)
  cat(sprintf("\nRetagged: %d rows from core -> OR\n", retag_count))
  cat(sprintf("Phantoms (kept as core): %d rows\n",     nrow(phantoms)))

  if (!apply) {
    message("\nDRY RUN. No files written. Re-run with apply = TRUE to commit.")
    return(invisible(list(
      cm_new   = cm_new,
      to_retag = to_retag,
      phantoms = phantoms
    )))
  }

  # Write outputs
  message("Writing updated concept_map...")
  readr::write_csv(cm_new, concept_map_path, na = "")

  message("Writing phantom review file...")
  phantoms |>
    dplyr::select(concept_id, year, raw_var_name, source, suggested_source) |>
    dplyr::arrange(raw_var_name, year) |>
    readr::write_csv(phantom_review_path, na = "")

  message(sprintf("Done. concept_map updated: %s", concept_map_path))
  message(sprintf("Phantom review file:       %s", phantom_review_path))

  invisible(list(
    cm_new   = cm_new,
    to_retag = to_retag,
    phantoms = phantoms
  ))
}
