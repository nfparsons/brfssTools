# inst/migrate/03-canonical-age.R
#
# Rename AGE -> age_continuous in all three CSVs. Idempotent.
#
# To run:
#   source("inst/migrate/03-canonical-age.R")
#   migrate_03_canonical_age()
#
# Run after migrate_02_survey_design().

migrate_03_canonical_age <- function(extdata = "inst/extdata") {
  suppressPackageStartupMessages({
    requireNamespace("dplyr", quietly = TRUE) || stop("Install dplyr.")
    requireNamespace("readr", quietly = TRUE) || stop("Install readr.")
  })
  stopifnot(dir.exists(extdata))

  rename_one <- function(path, old, new) {
    df <- readr::read_csv(path, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
    df <- dplyr::mutate(df,
                        concept_id = dplyr::if_else(.data$concept_id == old,
                                                    new, .data$concept_id))
    readr::write_csv(df, path, na = "")
  }

  rename_one(file.path(extdata, "concept_map_brfss.csv"),    "AGE",
             "age_continuous")
  rename_one(file.path(extdata, "concepts_brfss.csv"),       "AGE",
             "age_continuous")
  rename_one(file.path(extdata, "concept_values_brfss.csv"), "AGE",
             "age_continuous")
  message("Renamed AGE -> age_continuous in all three files.")
  invisible(NULL)
}
