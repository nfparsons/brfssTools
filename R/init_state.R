# R/init_state.R
#
# v0.2.0: Crosswalks are user-anchored (their data drives the concepts).
# brfss_init_state() creates an empty config + state codebook. With
# draft = TRUE and a registered pool, also drafts the crosswalk
# immediately via brfss_draft_crosswalk().
#
# v0.1.0 init_state archived in inst/archive/R/init_state_v01.R.

#' Initialize a brfssTools config for a state
#'
#' Sets up the user config directory: validates and copies in the state
#' codebook, creates an empty crosswalk, sets up metadata. With
#' \code{draft = TRUE}, immediately drafts the crosswalk from your
#' registered data pool's most recent year.
#'
#' @param state Two-letter state postal code or "DEMO".
#' @param state_codebook_path Path to the state's codebook CSV. Required
#'   unless \code{state == "DEMO"}.
#' @param path Optional explicit config path. Defaults to
#'   \code{tools::R_user_dir("brfssTools", "config")}.
#' @param overwrite Logical. If FALSE (default), errors when the config
#'   dir already exists and is non-empty.
#' @param draft Logical. If TRUE, calls \code{brfss_draft_crosswalk()}
#'   after the config is initialized. Requires a pool to be registered
#'   first via \code{brfss_set_pool()}. Default FALSE.
#' @return The resolved config path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Try the demo workflow:
#' brfss_init_state("DEMO")
#'
#' # Real state, draft crosswalk in one step:
#' brfss_set_pool("OR", "Z:/Secure_Data/Oregon_BRFSS")
#' brfss_init_state(
#'   state               = "OR",
#'   state_codebook_path = "~/data/oregon_codebook.csv",
#'   draft               = TRUE
#' )
#' }
brfss_init_state <- function(state,
                             state_codebook_path = NULL,
                             path = NULL,
                             overwrite = FALSE,
                             draft = FALSE) {
  if (missing(state) || !is.character(state) || length(state) != 1L ||
      !nzchar(state)) {
    stop("`state` must be a state postal code (e.g., \"OR\") or \"DEMO\".",
         call. = FALSE)
  }
  state <- toupper(state)

  cfg <- .brfss_resolve_config_path(path)

  if (dir.exists(cfg) && length(list.files(cfg)) > 0 && !overwrite) {
    stop(
      "Config directory already populated at:\n  ", cfg, "\n",
      "Set overwrite = TRUE to replace, or pass a different `path =`.",
      call. = FALSE
    )
  }

  dir.create(cfg, recursive = TRUE, showWarnings = FALSE)

  is_demo <- state == "DEMO"
  if (!is_demo && is.null(state_codebook_path)) {
    stop(
      "State '", state, "' requires a `state_codebook_path` pointing at ",
      "your state's codebook CSV.\n",
      "To see the schema, try the demo first: ",
      "brfss_init_state(\"DEMO\").",
      call. = FALSE
    )
  }

  pkg_extdata <- .brfss_package_extdata()

  # Copy state codebook
  if (is_demo) {
    src <- file.path(pkg_extdata, "state_codebook_demo.csv")
    if (!file.exists(src)) {
      stop("DEMO state codebook missing from package extdata. ",
           "Reinstall brfssTools.", call. = FALSE)
    }
    file.copy(src, file.path(cfg, "state_codebook.csv"), overwrite = TRUE)
  } else {
    if (!file.exists(state_codebook_path)) {
      stop("state_codebook_path does not exist: ", state_codebook_path,
           call. = FALSE)
    }
    cb <- readr::read_csv(state_codebook_path, show_col_types = FALSE,
                          locale = readr::locale(encoding = "UTF-8"),
                          progress = FALSE)
    cb <- brfss_validate_state_codebook(cb)
    readr::write_csv(cb, file.path(cfg, "state_codebook.csv"), na = "")
  }

  # Empty crosswalk with v0.2.0 schema
  empty_cw <- tibble::tibble(
    concept_id       = character(),
    year             = integer(),
    state_var        = character(),
    is_calculated    = integer(),
    calculation_yaml = character(),
    domain           = character(),
    subdomain        = character(),
    unverified       = integer(),
    notes            = character()
  )
  readr::write_csv(empty_cw, file.path(cfg, "crosswalk.csv"), na = "")

  # config.yaml
  config_yaml <- list(
    state = state,
    init_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    init_source = if (is_demo) "package_demo" else "user_codebook",
    state_codebook_source = if (!is.null(state_codebook_path))
                              normalizePath(state_codebook_path) else NA,
    schema_version = "0.2.0",
    package_version = as.character(utils::packageVersion("brfssTools"))
  )
  .write_simple_yaml(config_yaml, file.path(cfg, "config.yaml"))

  message(sprintf(
    "Initialized empty config for state '%s'. Config dir: %s",
    state, cfg))

  # Optionally draft crosswalk immediately
  if (draft) {
    pool <- tryCatch(brfss_pool_status(), error = function(e) NULL)
    has_pool <- !is.null(pool) && nrow(pool) > 0L &&
                state %in% pool$dataset
    if (!has_pool) {
      stop("`draft = TRUE` requires a pool registered for dataset '",
           state, "'.\nRun brfss_set_pool('", state, "', path) first, ",
           "then re-run brfss_init_state() with draft = TRUE.\n",
           "(Or skip drafting and call brfss_draft_crosswalk() later.)",
           call. = FALSE)
    }
    message("Drafting crosswalk from registered pool...")
    brfss_draft_crosswalk(dataset = state, path = cfg,
                          overwrite_existing = TRUE)
  } else {
    message(
      "To draft the crosswalk: register a pool ",
      "(brfss_set_pool('", state, "', '/path/to/data')) ",
      "then call brfss_draft_crosswalk()."
    )
  }

  invisible(cfg)
}

#' States with a built-in sample shipped in the package
#' @return Character vector of state codes.
#' @export
sampled_states <- function() {
  pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
  if (is.null(pkg)) return(character(0))
  if (file.exists(file.path(pkg, "state_codebook_demo.csv"))) {
    return("DEMO")
  }
  character(0)
}
