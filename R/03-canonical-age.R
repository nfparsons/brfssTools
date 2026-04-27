# inst/migrate/03-canonical-age.R
#
# Migration 03: rename AGE -> age_continuous. All 13 years (2012-2024)
# in the OR concept_map already point to raw column AGE; this is just
# a concept_id rename.
#
# Run after 02-survey-design.R. Idempotent.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

extdata <- "inst/extdata"
stopifnot(dir.exists(extdata))

rename_one <- function(path, old, new) {
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = cols(.default = "c"))
  df <- df |> mutate(concept_id = if_else(.data$concept_id == old, new,
                                          .data$concept_id))
  write_csv(df, path, na = "")
}

rename_one(file.path(extdata, "concept_map_brfss.csv"),    "AGE", "age_continuous")
rename_one(file.path(extdata, "concepts_brfss.csv"),       "AGE", "age_continuous")
rename_one(file.path(extdata, "concept_values_brfss.csv"), "AGE", "age_continuous")

message("Renamed AGE -> age_continuous in all three files.")
