#' brfssTools: Harmonize Longitudinal BRFSS Survey Data
#'
#' Tools for harmonizing the Behavioral Risk Factor Surveillance System
#' (BRFSS) survey data across years, where variable names, response codes,
#' and question text drift between waves. Provides a `tidycensus`-style
#' workflow for searching a packaged crosswalk, pulling concepts from a
#' user-supplied pool of raw survey files, and deriving standardized
#' race/ethnicity classifications from REAL-D arrays. Functions take data
#' as their first argument and return tibbles, so they compose naturally
#' with both base R (`|>`) and magrittr (`%>%`) pipes.
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

# Declare bare column names referenced inside dplyr/tidyselect verbs
# (especially `select()`, where `.data$col` is soft-deprecated as of
# tidyselect 1.2.0). Keep this list alphabetized.
utils::globalVariables(c(
  "code",
  "concept_id",
  "concept_label",
  "construct_notes",
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
  "source_file",
  "survey",
  "var_kind",
  "year"
))
