# inst/migrate/04-race-hispanic-arrays.R
#
# Rename hisp_flag -> hisp_array_1, add hisp_array_{2,3,4} and
# race_array_{11..14} concepts, and append per-year mappings for OR's
# race/Hispanic columns 2017-2024. Idempotent.
#
# After this migration, brfss_clean_race() can be used end-to-end against
# OR data 2017+ without manual column wrangling.
#
# To run:
#   source("inst/migrate/04-race-hispanic-arrays.R")
#   migrate_04_race_hispanic_arrays()
#
# Run after migrate_03_canonical_age().

migrate_04_race_hispanic_arrays <- function(extdata = "inst/extdata") {
  suppressPackageStartupMessages({
    requireNamespace("dplyr",  quietly = TRUE) || stop("Install dplyr.")
    requireNamespace("readr",  quietly = TRUE) || stop("Install readr.")
    requireNamespace("tibble", quietly = TRUE) || stop("Install tibble.")
    requireNamespace("tidyr",  quietly = TRUE) || stop("Install tidyr.")
  })
  stopifnot(dir.exists(extdata))

  # ---- 1. rename hisp_flag -> hisp_array_1 -----------------------------
  rename_one <- function(path, old, new) {
    df <- readr::read_csv(path, show_col_types = FALSE,
                          col_types = readr::cols(.default = "c"))
    df <- dplyr::mutate(df,
                        concept_id = dplyr::if_else(.data$concept_id == old,
                                                    new, .data$concept_id))
    readr::write_csv(df, path, na = "")
  }
  rename_one(file.path(extdata, "concept_map_brfss.csv"),    "hisp_flag",
             "hisp_array_1")
  rename_one(file.path(extdata, "concepts_brfss.csv"),       "hisp_flag",
             "hisp_array_1")
  rename_one(file.path(extdata, "concept_values_brfss.csv"), "hisp_flag",
             "hisp_array_1")
  message("Renamed hisp_flag -> hisp_array_1.")

  # ---- 2. add new concepts ---------------------------------------------
  c_path <- file.path(extdata, "concepts_brfss.csv")
  c_tbl  <- readr::read_csv(c_path, show_col_types = FALSE,
                            col_types = readr::cols(.default = "c"))
  new_concepts <- tibble::tribble(
    ~concept_id,     ~concept_label,                              ~domain,
    "hisp_array_2",  "Hispanic origin array slot 2",              "race_ethnicity",
    "hisp_array_3",  "Hispanic origin array slot 3",              "race_ethnicity",
    "hisp_array_4",  "Hispanic origin array slot 4",              "race_ethnicity",
    "race_array_11", "Race array slot 11 (REAL-D wide format)",   "race_ethnicity",
    "race_array_12", "Race array slot 12 (REAL-D wide format)",   "race_ethnicity",
    "race_array_13", "Race array slot 13 (REAL-D wide format)",   "race_ethnicity",
    "race_array_14", "Race array slot 14 (REAL-D wide format)",   "race_ethnicity"
  ) |>
    dplyr::mutate(subdomain = NA_character_,
                  construct_notes = NA_character_,
                  canonical_response_type = NA_character_)
  c_tbl <- c_tbl |>
    dplyr::filter(!.data$concept_id %in% new_concepts$concept_id) |>
    dplyr::bind_rows(new_concepts)
  readr::write_csv(c_tbl, c_path, na = "")

  # ---- 3. build per-year concept_map rows ------------------------------
  hisp_2017_2021 <- tidyr::expand_grid(slot = 1:4, year = 2017:2021) |>
    dplyr::transmute(
      concept_id          = paste0("hisp_array_", .data$slot),
      source              = "OR",
      year                = as.integer(.data$year),
      raw_var_name        = paste0("HISPANC3_", .data$slot),
      question_text_drift = "identical",
      recode_type         = "identity",
      recode_rule         = NA_character_,
      notes               = NA_character_
    )
  hisp_2022_2024 <- tidyr::expand_grid(slot = 1:4, year = 2022:2024) |>
    dplyr::transmute(
      concept_id          = paste0("hisp_array_", .data$slot),
      source              = "OR",
      year                = as.integer(.data$year),
      raw_var_name        = sprintf("HISPANC3_%02d", .data$slot),
      question_text_drift = "identical",
      recode_type         = "identity",
      recode_rule         = NA_character_,
      notes               = NA_character_
    )
  race_2017_2020 <- tidyr::expand_grid(slot = 1:10, year = 2017:2020) |>
    dplyr::transmute(
      concept_id          = paste0("race_array_", .data$slot),
      source              = "OR",
      year                = as.integer(.data$year),
      raw_var_name        = paste0("MRACE1_", .data$slot),
      question_text_drift = "identical",
      recode_type         = "identity",
      recode_rule         = NA_character_,
      notes               = NA_character_
    )
  race_2021 <- tibble::tibble(
    concept_id          = paste0("race_array_", 1:14),
    source              = "OR",
    year                = 2021L,
    raw_var_name        = paste0("MRACE1_", 1:14),
    question_text_drift = "identical",
    recode_type         = "identity",
    recode_rule         = NA_character_,
    notes               = NA_character_
  )
  race_2022_2024 <- tidyr::expand_grid(slot = 1:14, year = 2022:2024) |>
    dplyr::transmute(
      concept_id          = paste0("race_array_", .data$slot),
      source              = "OR",
      year                = as.integer(.data$year),
      raw_var_name        = sprintf("MRACE2_%02d", .data$slot),
      question_text_drift = "identical",
      recode_type         = "identity",
      recode_rule         = NA_character_,
      notes               = NA_character_
    )
  new_rows <- dplyr::bind_rows(hisp_2017_2021, hisp_2022_2024,
                               race_2017_2020, race_2021, race_2022_2024)

  # ---- 4. append (idempotent via anti_join on key) ---------------------
  cm_path <- file.path(extdata, "concept_map_brfss.csv")
  cm <- readr::read_csv(cm_path, show_col_types = FALSE,
                        col_types = readr::cols(.default = "c", year = "i"))
  key_cols <- c("concept_id", "source", "year", "raw_var_name")
  cm <- cm |>
    dplyr::anti_join(new_rows, by = key_cols) |>
    dplyr::bind_rows(new_rows)
  readr::write_csv(cm, cm_path, na = "")
  message(sprintf("Added %d concept_map rows. Total now: %d.",
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
