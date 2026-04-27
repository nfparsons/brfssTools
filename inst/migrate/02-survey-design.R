# inst/migrate/02-survey-design.R
#
# Rename HISPANC3 -> hisp_flag (later replaced with hisp_array_1 in
# Migration 04), and add canonical survey-design concepts (survey_weight,
# strata) with OR-specific mappings. Idempotent.
#
# To run:
#   source("inst/migrate/02-survey-design.R")
#   migrate_02_survey_design()
#
# Run after migrate_01_canonical_naming().

migrate_02_survey_design <- function(extdata = "inst/extdata") {
  suppressPackageStartupMessages({
    requireNamespace("dplyr",  quietly = TRUE) || stop("Install dplyr.")
    requireNamespace("readr",  quietly = TRUE) || stop("Install readr.")
    requireNamespace("tibble", quietly = TRUE) || stop("Install tibble.")
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
  rename_one(file.path(extdata, "concept_map_brfss.csv"),    "HISPANC3",
             "hisp_flag")
  rename_one(file.path(extdata, "concepts_brfss.csv"),       "HISPANC3",
             "hisp_flag")
  rename_one(file.path(extdata, "concept_values_brfss.csv"), "HISPANC3",
             "hisp_flag")
  message("Renamed HISPANC3 -> hisp_flag in all three files.")

  # ---- add concepts -----------------------------------------------------
  c_path <- file.path(extdata, "concepts_brfss.csv")
  c_tbl  <- readr::read_csv(c_path, show_col_types = FALSE,
                            col_types = readr::cols(.default = "c"))
  new_concepts <- tibble::tribble(
    ~concept_id,    ~concept_label,                              ~domain,
    "survey_weight", "Survey design weight (dataset-specific)",   "survey_design",
    "strata",        "Survey design stratum",                     "survey_design"
  ) |>
    dplyr::mutate(subdomain = NA_character_,
                  construct_notes = NA_character_,
                  canonical_response_type = NA_character_)
  c_tbl <- c_tbl |>
    dplyr::filter(!.data$concept_id %in% new_concepts$concept_id) |>
    dplyr::bind_rows(new_concepts)
  readr::write_csv(c_tbl, c_path, na = "")

  # ---- append per-year mappings ----------------------------------------
  cm_path <- file.path(extdata, "concept_map_brfss.csv")
  cm <- readr::read_csv(cm_path, show_col_types = FALSE,
                        col_types = readr::cols(.default = "c", year = "i"))
  new_rows <- tibble::tribble(
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
  key_cols <- c("concept_id", "source", "year", "raw_var_name")
  cm <- cm |>
    dplyr::anti_join(new_rows, by = key_cols) |>
    dplyr::bind_rows(new_rows)
  readr::write_csv(cm, cm_path, na = "")
  message(sprintf("Added %d new concept_map rows; map now has %d rows.",
                  nrow(new_rows), nrow(cm)))

  ids_cm <- unique(cm$concept_id)
  ids_c  <- unique(c_tbl$concept_id)
  stopifnot(
    "concept_map references concept_id not in concepts" = all(ids_cm %in% ids_c),
    "concepts has concept_id not in concept_map"        = all(ids_c %in% ids_cm)
  )
  message("Referential integrity OK.")
  invisible(NULL)
}
