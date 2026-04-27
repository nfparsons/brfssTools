# inst/migrate/01-canonical-naming.R
#
# One-shot migration: rename concept_ids to canonical snake_case naming
# and convert legacy `survey` schema to `source`. Run from the package
# root after installing tidyverse:
#
#   source("inst/migrate/01-canonical-naming.R")
#
# Reads from inst/extdata/, writes back to inst/extdata/. Idempotent:
# safe to re-run; rows already in the new schema are passed through
# unchanged.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

extdata <- "inst/extdata"
stopifnot(dir.exists(extdata))

# ---------------------------------------------------------------------------
# Concept-id renames (applied to all three CSVs).
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Migrate concept_map_brfss.csv
# ---------------------------------------------------------------------------
cm_path <- file.path(extdata, "concept_map_brfss.csv")
cm <- read_csv(cm_path, show_col_types = FALSE,
               col_types = cols(.default = "c", year = "i"))

# Rename column if legacy
if ("survey" %in% names(cm) && !"source" %in% names(cm)) {
  cm <- rename(cm, source = "survey")
}

cm_migrated <- cm |>
  mutate(
    concept_id = apply_renames(.data$concept_id),
    source     = if_else(.data$source == "BRFSS", "core", .data$source)
  ) |>
  distinct()  # drops 2 exact-duplicate rows (CELLPERCENT 2014, SMARTPH 2014)

write_csv(cm_migrated, cm_path, na = "")
message(sprintf("concept_map: %d rows -> %d rows (%d deduped)",
                nrow(cm), nrow(cm_migrated), nrow(cm) - nrow(cm_migrated)))

# ---------------------------------------------------------------------------
# Migrate concepts_brfss.csv
# ---------------------------------------------------------------------------
c_path <- file.path(extdata, "concepts_brfss.csv")
c_tbl <- read_csv(c_path, show_col_types = FALSE,
                  col_types = cols(.default = "c"))
c_migrated <- c_tbl |>
  mutate(concept_id = apply_renames(.data$concept_id))
write_csv(c_migrated, c_path, na = "")
message(sprintf("concepts: %d rows", nrow(c_migrated)))

# ---------------------------------------------------------------------------
# Migrate concept_values_brfss.csv
# ---------------------------------------------------------------------------
cv_path <- file.path(extdata, "concept_values_brfss.csv")
cv <- read_csv(cv_path, show_col_types = FALSE,
               col_types = cols(.default = "c"))
cv_migrated <- cv |>
  mutate(concept_id = apply_renames(.data$concept_id))
write_csv(cv_migrated, cv_path, na = "")
message(sprintf("concept_values: %d rows", nrow(cv_migrated)))

# ---------------------------------------------------------------------------
# Final integrity check
# ---------------------------------------------------------------------------
ids_cm <- unique(cm_migrated$concept_id)
ids_c  <- unique(c_migrated$concept_id)
ids_cv <- unique(cv_migrated$concept_id)

stopifnot(
  "concept_map references concept_id not in concepts" = all(ids_cm %in% ids_c),
  "concepts has concept_id not in concept_map"        = all(ids_c %in% ids_cm),
  "concept_values references concept_id not in concepts" = all(ids_cv %in% ids_c)
)
message("Referential integrity OK.")

# Surface remaining duplicate keys for human review
dups <- cm_migrated |>
  count(.data$concept_id, .data$year, .data$raw_var_name) |>
  filter(.data$n > 1)
if (nrow(dups)) {
  message(sprintf("\n%d (concept_id, year, raw_var_name) keys still duplicated; review:",
                  nrow(dups)))
  print(dups)
}
