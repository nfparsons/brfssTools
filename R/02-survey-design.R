# inst/migrate/02-survey-design.R
#
# Migration 02: rename HISPANC3 -> hisp_flag, and add canonical survey-design
# concepts (survey_weight, strata) with OR-specific mappings.
#
# Run after 01-canonical-naming.R. Idempotent: safe to re-run.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

extdata <- "inst/extdata"
stopifnot(dir.exists(extdata))

# ---------------------------------------------------------------------------
# Rename HISPANC3 -> hisp_flag (canonical name for the single 1-5 Hispanic flag)
# ---------------------------------------------------------------------------
rename_one <- function(path, old, new) {
  df <- read_csv(path, show_col_types = FALSE,
                 col_types = cols(.default = "c"))
  df <- df |> mutate(concept_id = if_else(.data$concept_id == old, new,
                                          .data$concept_id))
  write_csv(df, path, na = "")
}

rename_one(file.path(extdata, "concept_map_brfss.csv"),    "HISPANC3", "hisp_flag")
rename_one(file.path(extdata, "concepts_brfss.csv"),       "HISPANC3", "hisp_flag")
rename_one(file.path(extdata, "concept_values_brfss.csv"), "HISPANC3", "hisp_flag")

message("Renamed HISPANC3 -> hisp_flag in all three files.")

# ---------------------------------------------------------------------------
# Add survey_weight and strata concepts to concepts_brfss.csv
# ---------------------------------------------------------------------------
c_path <- file.path(extdata, "concepts_brfss.csv")
c_tbl  <- read_csv(c_path, show_col_types = FALSE,
                   col_types = cols(.default = "c"))

new_concepts <- tribble(
  ~concept_id,    ~concept_label,                              ~domain,           ~subdomain, ~construct_notes, ~canonical_response_type,
  "survey_weight", "Survey design weight (dataset-specific)",   "survey_design",   NA_character_, NA_character_, NA_character_,
  "strata",        "Survey design stratum",                     "survey_design",   NA_character_, NA_character_, NA_character_
)

c_tbl <- c_tbl |>
  filter(!.data$concept_id %in% new_concepts$concept_id) |>
  bind_rows(new_concepts)
write_csv(c_tbl, c_path, na = "")

# ---------------------------------------------------------------------------
# Add survey_weight + strata mappings to concept_map_brfss.csv
# ---------------------------------------------------------------------------
cm_path <- file.path(extdata, "concept_map_brfss.csv")
cm <- read_csv(cm_path, show_col_types = FALSE,
               col_types = cols(.default = "c", year = "i"))

new_rows <- tribble(
  ~concept_id,     ~source, ~year, ~raw_var_name,     ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
  "survey_weight", "OR",    2012L, "wtrk_ABCD_abocd", "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2013L, "wtrk_ABCD_abocd", "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2014L, "wtrk_ABCD_abocd", "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2015L, "wtrk_ABCD_abocd", "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2016L, "wtrk_AC_aoc",     "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2017L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2018L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2019L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2020L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2021L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2022L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2023L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "survey_weight", "OR",    2024L, "wtrk_all",        "identical",          "identity",   NA_character_, NA_character_,
  "strata",        "OR",    2024L, "STSTR",           "identical",          "identity",   NA_character_, NA_character_
)

# De-duplicate against any existing rows with the same key
key_cols <- c("concept_id", "source", "year", "raw_var_name")
cm <- cm |>
  anti_join(new_rows, by = key_cols) |>
  bind_rows(new_rows)
write_csv(cm, cm_path, na = "")

message(sprintf("Added %d new concept_map rows; map now has %d rows.",
                nrow(new_rows), nrow(cm)))

# ---------------------------------------------------------------------------
# Final integrity check
# ---------------------------------------------------------------------------
ids_cm <- unique(cm$concept_id)
ids_c  <- unique(c_tbl$concept_id)
stopifnot(
  "concept_map references concept_id not in concepts" = all(ids_cm %in% ids_c),
  "concepts has concept_id not in concept_map"        = all(ids_c %in% ids_cm)
)
message("Referential integrity OK.")
