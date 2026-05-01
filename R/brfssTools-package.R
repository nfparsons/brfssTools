#' brfssTools: Harmonize Longitudinal BRFSS Survey Data
#'
#' Tools for harmonizing the Behavioral Risk Factor Surveillance System
#' (BRFSS) survey data across years. The crosswalk is anchored on
#' \strong{the user's most recent year of data}: a draft function reads
#' the variable list from the latest year's data file and produces a
#' starting crosswalk where each variable becomes a \code{concept_id}.
#' The user fills in equivalents for older years via a Shiny editor;
#' cells can be flagged \code{is_calculated = 1} to store inline YAML
#' defining a year-specific computation.
#'
#' Domain assignment is pre-filled where possible from the shipped CDC
#' codebook reference and left as \code{"Unassigned"} otherwise.
#'
#' @section Getting started:
#' New users should follow this order:
#' \enumerate{
#'   \item Register a data pool: [brfss_set_pool()]
#'   \item Initialize a config and draft the crosswalk:
#'     [brfss_init_state()] with \code{draft = TRUE}
#'   \item Open the editor to fill in older years and assign domains:
#'     [brfss_crosswalk_editor()]
#' }
#'
#' See \code{vignette("getting-started", package = "brfssTools")} for
#' the full walkthrough.
#'
#' @section Setup:
#' * [brfss_init_state()] — scaffold a user config directory; optionally
#'   draft the crosswalk in the same call.
#' * [brfss_set_pool()] / [brfss_pool_status()] — register and inspect
#'   raw data files.
#' * [brfss_config_path()] — show the active config directory.
#' * [brfss_status()] — print where everything lives.
#' * [brfss_reset()] — wipe back to a clean state, with backup.
#' * [brfss_export_config()] / [brfss_import_config()] — move work
#'   between machines.
#'
#' @section Drafting and migrating crosswalks:
#' * [brfss_draft_crosswalk()] — read the most recent year's variables
#'   from the registered pool, produce a draft crosswalk.
#' * [brfss_redraft_crosswalk()] — idempotent extend; adds new years
#'   and new variables without overwriting existing mappings.
#' * [brfss_migrate_crosswalk_to_v2()] — one-shot upgrade of a
#'   v0.1.0 crosswalk to v0.2.0 schema.
#'
#' @section Editing:
#' * [brfss_crosswalk_editor()] — Shiny GUI.
#' * [cw_load()], [cw_save()] — load and save from the console.
#' * [cw_set_var()] — set a state variable for a (concept, year) cell.
#' * [cw_set_calc()] — flag a cell as calculated and provide YAML.
#' * [cw_assign_domain()] — assign a concept to a domain/subdomain.
#' * [cw_verify()] — clear the unverified flag on a cell.
#' * [cw_rename_concept()] — rename a concept across all its rows.
#' * [cw_merge_concepts()] — merge one concept's mappings into another.
#' * [cw_add_concept()] / [cw_remove_concept()] — concept-level CRUD.
#'
#' @section Reference:
#' * [brfss_assign_domains()] — domain auto-assignment via CDC codebook.
#' * [sanitize_concept_id()] — make a variable name R-friendly.
#'
#' @section Pulling data:
#' \code{brfss_pull()} for the v0.2.0 schema is deferred to a follow-up
#' release. The crosswalk-building workflow is the focus of v0.2.0.
#'
#' @keywords internal
#' @importFrom rlang .data .env
"_PACKAGE"

# Bare column names referenced inside dplyr/tidyselect verbs. Required to
# silence R CMD check's "no visible binding" notes. Keep alphabetized.
utils::globalVariables(c(
  "concept_id",
  "dataset",
  "domain",
  "is_calculated",
  "raw_var_name",
  "state_var",
  "subdomain",
  "year"
))
