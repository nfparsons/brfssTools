# inst/migrate/04-race-hispanic-arrays.R
#
# Migration 04: rename hisp_flag -> hisp_array_1 (continuing the array
# naming convention), add hisp_array_{2,3,4} and race_array_{11..14}
# concepts, and append per-year mappings for OR's race/Hispanic columns
# 2017-2024.
#
# After this migration, brfss_clean_race() can be used end-to-end
# against OR data 2017+ without manual column wrangling.
#
# Run after 03-canonical-age.R. Idempotent.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

extdata <- "inst/extdata"
stopifnot(dir.exists(extdata))

# ---------------------------------------------------------------------------
# 1. Rename hisp_flag -> hisp_array_1 across all three CSVs
# ---------------------------------------------------------------------------
rename_one <- function(path, old, new) {
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = cols(.default = "c"))
  df <- df |> mutate(concept_id = if_else(.data$concept_id == old, new,
                                          .data$concept_id))
  write_csv(df, path, na = "")
}
rename_one(file.path(extdata, "concept_map_brfss.csv"),    "hisp_flag", "hisp_array_1")
rename_one(file.path(extdata, "concepts_brfss.csv"),       "hisp_flag", "hisp_array_1")
rename_one(file.path(extdata, "concept_values_brfss.csv"), "hisp_flag", "hisp_array_1")
message("Renamed hisp_flag -> hisp_array_1.")

# ---------------------------------------------------------------------------
# 2. Add new concepts to concepts_brfss.csv
# ---------------------------------------------------------------------------
c_path <- file.path(extdata, "concepts_brfss.csv")
c_tbl  <- read_csv(c_path, show_col_types = FALSE,
                   col_types = cols(.default = "c"))

new_concepts <- tribble(
  ~concept_id,    ~concept_label,                              ~domain,
  "hisp_array_2", "Hispanic origin array slot 2",              "race_ethnicity",
  "hisp_array_3", "Hispanic origin array slot 3",              "race_ethnicity",
  "hisp_array_4", "Hispanic origin array slot 4",              "race_ethnicity",
  "race_array_11", "Race array slot 11 (REAL-D wide format)",  "race_ethnicity",
  "race_array_12", "Race array slot 12 (REAL-D wide format)",  "race_ethnicity",
  "race_array_13", "Race array slot 13 (REAL-D wide format)",  "race_ethnicity",
  "race_array_14", "Race array slot 14 (REAL-D wide format)",  "race_ethnicity"
) |>
  mutate(subdomain = NA_character_,
         construct_notes = NA_character_,
         canonical_response_type = NA_character_)

c_tbl <- c_tbl |>
  filter(!.data$concept_id %in% new_concepts$concept_id) |>
  bind_rows(new_concepts)
write_csv(c_tbl, c_path, na = "")

# ---------------------------------------------------------------------------
# 3. Build per-year race/hispanic concept_map rows (all source = "OR")
# ---------------------------------------------------------------------------
hisp_2017_2021 <- expand_grid(
  slot = 1:4, year = 2017:2021
) |>
  transmute(
    concept_id   = paste0("hisp_array_", .data$slot),
    source       = "OR",
    year         = as.integer(.data$year),
    raw_var_name = paste0("HISPANC3_", .data$slot),
    question_text_drift = "identical",
    recode_type  = "identity",
    recode_rule  = NA_character_,
    notes        = NA_character_
  )

hisp_2022_2024 <- expand_grid(
  slot = 1:4, year = 2022:2024
) |>
  transmute(
    concept_id   = paste0("hisp_array_", .data$slot),
    source       = "OR",
    year         = as.integer(.data$year),
    raw_var_name = sprintf("HISPANC3_%02d", .data$slot),
    question_text_drift = "identical",
    recode_type  = "identity",
    recode_rule  = NA_character_,
    notes        = NA_character_
  )

# 2017-2020: 10-slot MRACE1 array
race_2017_2020 <- expand_grid(
  slot = 1:10, year = 2017:2020
) |>
  transmute(
    concept_id   = paste0("race_array_", .data$slot),
    source       = "OR",
    year         = as.integer(.data$year),
    raw_var_name = paste0("MRACE1_", .data$slot),
    question_text_drift = "identical",
    recode_type  = "identity",
    recode_rule  = NA_character_,
    notes        = NA_character_
  )

# 2021: 14-slot MRACE1 array
race_2021 <- tibble(
  concept_id = paste0("race_array_", 1:14),
  source = "OR",
  year = 2021L,
  raw_var_name = paste0("MRACE1_", 1:14),
  question_text_drift = "identical",
  recode_type = "identity",
  recode_rule = NA_character_,
  notes = NA_character_
)

# 2022-2024: 14-slot MRACE2 array, zero-padded
race_2022_2024 <- expand_grid(
  slot = 1:14, year = 2022:2024
) |>
  transmute(
    concept_id   = paste0("race_array_", .data$slot),
    source       = "OR",
    year         = as.integer(.data$year),
    raw_var_name = sprintf("MRACE2_%02d", .data$slot),
    question_text_drift = "identical",
    recode_type  = "identity",
    recode_rule  = NA_character_,
    notes        = NA_character_
  )

new_rows <- bind_rows(
  hisp_2017_2021, hisp_2022_2024,
  race_2017_2020, race_2021, race_2022_2024
)

# ---------------------------------------------------------------------------
# 4. Append to concept_map (idempotent: dedupe against existing keys)
# ---------------------------------------------------------------------------
cm_path <- file.path(extdata, "concept_map_brfss.csv")
cm <- read_csv(cm_path, show_col_types = FALSE,
               col_types = cols(.default = "c", year = "i"))

key_cols <- c("concept_id", "source", "year", "raw_var_name")
cm <- cm |>
  anti_join(new_rows, by = key_cols) |>
  bind_rows(new_rows)
write_csv(cm, cm_path, na = "")

message(sprintf("Added %d concept_map rows. Total now: %d.",
                nrow(new_rows), nrow(cm)))

# ---------------------------------------------------------------------------
# 5. Final integrity check
# ---------------------------------------------------------------------------
ids_cm <- unique(cm$concept_id)
ids_c  <- unique(c_tbl$concept_id)
stopifnot(
  "concept_map references concept_id not in concepts" = all(ids_cm %in% ids_c),
  "concepts has concept_id not in concept_map"        = all(ids_c %in% ids_cm)
)
message("Referential integrity OK.")
