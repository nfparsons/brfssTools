#' brfssTools: Harmonize Longitudinal BRFSS Survey Data
#'
#' Tools for harmonizing the Behavioral Risk Factor Surveillance System
#' (BRFSS) survey data across years and across sources (national CDC LLCP
#' files plus state-collected files), where variable names, response codes,
#' and question text drift between waves.
#'
#' brfssTools provides a `tidycensus`-style workflow built around a
#' user-editable crosswalk. Initialize a state config, register a pool of
#' raw data files, edit the crosswalk via an interactive Shiny GUI, set up
#' declarative transformations for derived variables like race, and pull
#' harmonized concepts as a tidy tibble. State and CDC data live behind a
#' single `concept_id` addressing system: different state variables across
#' years can share a `concept_id` (e.g., `ACEHURT1`/`ACEHURT2`/`ACEHURT3`
#' all collapse to `ACEHURT`), and `brfss_pull("ACEHURT")` resolves to the
#' right per-year column automatically.
#'
#' @section Getting started:
#' New users should start with one of the workflow vignettes:
#' * `vignette("state-data-workflow")` — for analysts with state-collected
#'   BRFSS files.
#' * `vignette("national-data-workflow")` — for analysts using only CDC's
#'   public LLCP files via [brfss_download()].
#'
#' @section Setup:
#' * [brfss_init_state()] — scaffold a user config directory.
#' * [brfss_set_pool()] / [brfss_download()] — register a directory of raw
#'   data files (state) or fetch CDC's public files (national).
#' * [brfss_config_path()] — show the active config directory.
#'
#' @section Editing the crosswalk:
#' * [brfss_crosswalk_editor()] — interactive Shiny GUI (CDC-anchored or
#'   state-anchored heatmap, edit panel, bulk concept rename).
#' * [cw_load()], [cw_save()] — load and save the crosswalk bundle from
#'   the console.
#' * [cw_add_pair()], [cw_remove_pair()], [cw_update_pair()],
#'   [cw_mark_state_only()], [cw_mark_cdc_only()],
#'   [cw_replace_cdc_partner()], [cw_replace_state_partner()],
#'   [cw_rename_concept()], [cw_recompute_concept_ids()] — programmatic
#'   crosswalk CRUD.
#' * [brfss_search()], [brfss_lookup()] — explore the crosswalk and
#'   codebooks.
#'
#' @section Transformations (derived variables):
#' Two systems for variables that don't map cleanly to a single column.
#' Declarative YAML categorical maps are preferred for typical recodes;
#' functional `.R` files are the escape hatch for anything YAML can't
#' express.
#' * [brfss_setup_categorical_map()], [brfss_setup_race_map()] — declarative
#'   YAML.
#' * [brfss_setup_transformation()], [brfss_setup_race()] — functional
#'   `.R`.
#' * [brfss_load_categorical_map()],
#'   [brfss_render_transformation_code()],
#'   [brfss_list_transformations()] — utilities.
#'
#' @section Pulling data:
#' * [brfss_pull()] — the workhorse. Accepts `concepts`, `domains`, `tags`,
#'   plus `years` and `states`. Auto-detects state vs. CDC source from
#'   registered pools. Applies your transformations per year.
#'
#' @keywords internal
#' @importFrom rlang .data .env
"_PACKAGE"

# Bare column names referenced inside dplyr/tidyselect verbs. Required to
# silence R CMD check's "no visible binding" notes. Keep alphabetized.
utils::globalVariables(c(
  "cdc_label",
  "cdc_var",
  "concept_id",
  "concept_ids",
  "dataset",
  "in_reference",
  "is_match",
  "is_primary",
  "label",
  "matched_in",
  "needs_action",
  "oha_var",
  "raw_var_name",
  "reference_years_seen",
  "score",
  "source",
  "state_label",
  "state_var",
  "suggested_source",
  "year"
))
