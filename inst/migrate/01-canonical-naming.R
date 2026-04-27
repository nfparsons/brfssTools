# inst/migrate/01-canonical-naming.R
#
# One-shot migration: rename concept_ids to canonical snake_case naming
# and convert legacy `survey` schema to `source`. Idempotent.
#
# To run:
#   source("inst/migrate/01-canonical-naming.R")
#   migrate_01_canonical_naming()
#
# Sourcing the file alone defines the function but performs no I/O.
# This is intentional — migration scripts must NEVER auto-run, since
# users (and CI) source them in contexts where their side effects
# would be wrong.

migrate_01_canonical_naming <- function(extdata = "inst/extdata") {
  suppressPackageStartupMessages({
    requireNamespace("dplyr",  quietly = TRUE) || stop("Install dplyr.")
    requireNamespace("readr",  quietly = TRUE) || stop("Install readr.")
  })
  stopifnot(dir.exists(extdata))

  renames <- c(
    RACE1     = "race_array_1",
    RACE2     = "race_array_2",
    RACE3     = "race_array_3",
    RACE4     = "race_array_4",
    RACE5     = "race_array_5",
    RACE6     = "race_array_6",
    RACE7     = "race_array_7",
    RACE8     = "race_array_8",
    RACE9     = "race_array_9",
    RACE10    = "race_array_10",
    ageg5yr   = "age_5yr",
    age65yr   = "age_65plus",
    PSU       = "psu"
  )

  apply_renames <- function(x) {
    hit <- match(x, names(renames))
    ifelse(is.na(hit), x, unname(renames[hit]))
  }

  # ---- concept_map -------------------------------------------------------
  cm_path <- file.path(extdata, "concept_map_brfss.csv")
  cm <- readr::read_csv(cm_path, show_col_types = FALSE,
                        col_types = readr::cols(.default = "c", year = "i"))

  if ("survey" %in% names(cm) && !"source" %in% names(cm)) {
    cm <- dplyr::rename(cm, source = "survey")
  }

  cm_migrated <- cm |>
    dplyr::mutate(
      concept_id = apply_renames(.data$concept_id),
      source     = dplyr::if_else(.data$source == "BRFSS", "core",
                                  .data$source)
    ) |>
    dplyr::distinct()

  readr::write_csv(cm_migrated, cm_path, na = "")
  message(sprintf("concept_map: %d rows -> %d rows (%d deduped)",
                  nrow(cm), nrow(cm_migrated), nrow(cm) - nrow(cm_migrated)))

  # ---- concepts ----------------------------------------------------------
  c_path <- file.path(extdata, "concepts_brfss.csv")
  c_tbl <- readr::read_csv(c_path, show_col_types = FALSE,
                           col_types = readr::cols(.default = "c"))
  c_migrated <- c_tbl |>
    dplyr::mutate(concept_id = apply_renames(.data$concept_id))
  readr::write_csv(c_migrated, c_path, na = "")
  message(sprintf("concepts: %d rows", nrow(c_migrated)))

  # ---- concept_values ---------------------------------------------------
  cv_path <- file.path(extdata, "concept_values_brfss.csv")
  cv <- readr::read_csv(cv_path, show_col_types = FALSE,
                        col_types = readr::cols(.default = "c"))
  cv_migrated <- cv |>
    dplyr::mutate(concept_id = apply_renames(.data$concept_id))
  readr::write_csv(cv_migrated, cv_path, na = "")
  message(sprintf("concept_values: %d rows", nrow(cv_migrated)))

  # ---- integrity --------------------------------------------------------
  ids_cm <- unique(cm_migrated$concept_id)
  ids_c  <- unique(c_migrated$concept_id)
  ids_cv <- unique(cv_migrated$concept_id)
  stopifnot(
    "concept_map references concept_id not in concepts" = all(ids_cm %in% ids_c),
    "concepts has concept_id not in concept_map"        = all(ids_c %in% ids_cm),
    "concept_values references concept_id not in concepts" = all(ids_cv %in% ids_c)
  )
  message("Referential integrity OK.")

  dups <- cm_migrated |>
    dplyr::count(.data$concept_id, .data$year, .data$raw_var_name) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dups)) {
    message(sprintf(
      "\n%d (concept_id, year, raw_var_name) keys still duplicated; review:",
      nrow(dups)
    ))
    print(dups)
  }
  invisible(NULL)
}
