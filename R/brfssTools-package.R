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
#' Each project specifies three paths: where the raw data lives, where
#' the codebook documentation lives, and where the crosswalk file lives.
#' By default, the codebook and crosswalk live in a \code{documentation/}
#' subfolder of the data directory, but power users can put them
#' anywhere.
#'
#' @section Getting started:
#' \enumerate{
#'   \item Register a pool: [brfss_set_pool()] (only \code{data_path}
#'     is required; codebook and crosswalk paths default to
#'     \code{<data_path>/documentation/}).
#'   \item Draft the crosswalk: [brfss_draft_crosswalk()].
#'   \item Edit: [brfss_crosswalk_editor()].
#' }
#'
#' For National-data users: [brfss_use_national()] downloads CDC's
#' public files and registers them as a pool in one call.
#'
#' See \code{vignette("getting-started", package = "brfssTools")} for
#' the full walkthrough.
#'
#' @section Pool management:
#' * [brfss_set_pool()] — register a dataset with three paths
#'   (\code{data_path} required; \code{codebook_path} and
#'   \code{crosswalk} default to \code{<data_path>/documentation/}).
#' * [brfss_pool_status()] — show all registered pools as a tibble.
#' * [brfss_pool_get()] — get the resolved pool config for one
#'   dataset.
#' * [brfss_clear_pool()] — drop a registered pool.
#' * [brfss_status()] — printed summary of all registered pools.
#' * [brfss_detect_year()] — infer the survey year a data file
#'   represents (used internally by [brfss_set_pool()] but available
#'   for inspection).
#'
#' @section Drafting and migrating crosswalks:
#' * [brfss_draft_crosswalk()] — produce a draft from your most recent
#'   year of data.
#' * [brfss_redraft_crosswalk()] — idempotent extend; adds new years
#'   and new variables without overwriting existing mappings.
#' * [brfss_assign_domains()] — domain auto-assignment via CDC codebook.
#'   Called automatically during drafting.
#' * [sanitize_concept_id()] — make a variable name R-friendly.
#' * [brfss_migrate_crosswalk_to_v2()] — one-shot upgrade of a
#'   v0.1.0 crosswalk file to v0.2.0 schema.
#' * [brfss_migrate_config_dir()] — for users who built work in a
#'   pre-release v0.2.0 development build with a config dir.
#'
#' @section Editing:
#' * [brfss_crosswalk_editor()] — Shiny GUI.
#' * [cw_load()] / [cw_save()] — load and save from the console.
#' * [cw_set_var()] — set a state variable for a (concept, year) cell.
#' * [cw_set_calc()] — set inline calculation YAML for a cell.
#' * [cw_assign_domain()] — assign a concept to a domain/subdomain.
#' * [cw_verify()] — clear the unverified flag on a cell.
#' * [cw_rename_concept()] — rename a concept across all its rows.
#' * [cw_merge_concepts()] — merge one concept's mappings into
#'   another, then delete the source.
#' * [cw_add_concept()] / [cw_remove_concept()] — concept-level CRUD.
#'
#' @section Codebook documentation:
#' If properly-formatted codebook CSVs are present in the registered
#' \code{codebook_path}, the editor displays question text and value
#' labels for each cell.
#' * [brfss_load_codebook()] — load and validate one (dataset, year)
#'   codebook.
#' * [brfss_codebook_help()] — print the format spec or AI-agent
#'   prompt for converting raw codebooks into the expected format.
#'
#' @section National BRFSS:
#' * [brfss_use_national()] — download CDC LLCP files and register as
#'   the "National" pool in one call.
#' * [brfss_download()] — direct download interface (used by
#'   [brfss_use_national()]; also available for explicit use).
#' * [brfss_cache_dir()] — show the package's download cache location.
#'
#' @section Pool inspection:
#' * [brfss_inventory_pool()] — extract a long-format inventory of
#'   variables across years from a registered pool.
#' * [brfss_values_pool()] — extract value-frequency tables for one
#'   or more variables across the pool.
#' * [brfss_datasets()] — registry of dataset codes recognized by the
#'   package.
#'
#' @section Transformation runtime:
#' * [brfss_load_transformation_spec()],
#'   [brfss_load_categorical_map()] — parse YAML transformation specs
#'   from a string or file. Used internally to evaluate inline
#'   calculation YAML at pull time.
#' * [brfss_validate_state_codebook()] — validate a state-codebook
#'   CSV against the legacy schema.
#'
#' @section Survey design:
#' * [brfss_as_svy()] — wrap a pulled tibble into a survey design
#'   object using BRFSS's standard weighting variables.
#' * [brfss_clean_race()], [brfss_race()] — race recoders carried
#'   over from v0.1.0; see help pages for current behavior.
#'
#' @section Deferred to v0.2.x:
#' These functions are exported with stub bodies that error with a
#' message about the deferred implementation. They're visible here so
#' users coming from v0.1.0 know what happened to them:
#' * [brfss_pull()] — loading harmonized data using the v0.2.0 schema.
#'   Crosswalk-building is the focus of v0.2.0.
#' * [brfss_search()], [brfss_lookup()] — console search/lookup of
#'   crosswalk rows. Will be reimplemented for the v0.2.0 schema.
#' * [brfss_setup_categorical_map()], [brfss_setup_race_map()],
#'   [brfss_setup_race()], [brfss_setup_transformation()],
#'   [brfss_render_transformation_code()],
#'   [brfss_list_transformations()] — file-based transformation
#'   helpers, replaced by inline calculation YAML in the crosswalk.
#'
#' @keywords internal
#' @importFrom rlang .data .env
"_PACKAGE"

# Bare column names referenced inside dplyr/tidyselect verbs. Required to
# silence R CMD check's "no visible binding" notes.
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
