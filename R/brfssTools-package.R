#' brfssTools: Harmonize Longitudinal BRFSS Survey Data
#'
#' Tools for harmonizing the Behavioral Risk Factor Surveillance System
#' (BRFSS) survey data across years and across datasets (national CDC LLCP
#' files plus state-restricted files), where variable names, response codes,
#' and question text drift between waves. Provides a `tidycensus`-style
#' workflow: download or register a pool of raw survey files, search a
#' bundled master crosswalk, and pull harmonized concepts as a tidy tibble.
#'
#' @keywords internal
#' @importFrom rlang .data .env
"_PACKAGE"

# Bare column names referenced inside dplyr/tidyselect verbs. Required to
# silence R CMD check's "no visible binding" notes, since `.data$col` is
# soft-deprecated inside `select()` calls. Keep alphabetized.
utils::globalVariables(c(
  "code",
  "concept_id",
  "concept_label",
  "construct_notes",
  "dataset",
  "is_match",
  "is_missing",
  "matched_in",
  "missing_codes",
  "missing_values_raw",
  "module",
  "parse_notes",
  "position",
  "question_text",
  "raw_var_name",
  "source",
  "source_file",
  "var_kind",
  "year"
))
