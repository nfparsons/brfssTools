# R/init_state.R
#
# brfss_init_state() — initialize a user's brfssTools config directory.
# Two modes:
#   - state = "OR" (or any state we ship a sample for): copies the package's
#     shipped sample data as a starting point.
#   - state = anything else: requires state_codebook_path; initializes empty
#     crosswalk and state_only files.

#' Initialize a brfssTools config for a state
#'
#' Sets up `tools::R_user_dir("brfssTools", "config")` (or a custom path)
#' with the files needed to use the crosswalk editor and `brfss_pull()`.
#' After this runs once, all other brfssTools functions can find your state
#' codebook and crosswalk automatically.
#'
#' Two modes of operation:
#'
#' \strong{Demo state.} `brfss_init_state("DEMO")` copies a tiny example
#' state codebook (~15 fake variables across 3 years) into the config dir,
#' along with the seeded CDC crosswalk. Useful for trying the workflow,
#' exploring the editor, or as a reference example.
#'
#' \strong{Real state.} `brfss_init_state("OR", state_codebook_path = "...")`
#' (or any other state code) reads and validates the user-supplied state
#' codebook, copies it in, and seeds the crosswalk from the CDC reference.
#'
#' \strong{The seeded crosswalk.} By default (`seed_from_cdc = TRUE`), the
#' crosswalk starts pre-populated with one row per CDC variable per year
#' (~4,200 rows across 2012-2024). Each seeded row has `state_var = NA`,
#' `source = "cdc_seed"`, and `unverified = 1`. Your job is to walk
#' through the crosswalk in the editor and:
#' \enumerate{
#'   \item Fill in the `state_var` for variables your state collects
#'   \item Clear the `unverified` flag once you've confirmed the mapping
#'   \item Add new rows for state-only variables
#' }
#' Pass `seed_from_cdc = FALSE` to start with a completely empty crosswalk.
#'
#' @param state Two-letter state postal code, or `"DEMO"` for the shipped
#'   example. Used in config metadata; the crosswalk file is named
#'   `crosswalk.csv` regardless of state.
#' @param state_codebook_path Path to the user's state codebook CSV. Required
#'   unless `state == "DEMO"`. The codebook must have at minimum the columns
#'   `year`, `raw_var_name`, and `label`. See
#'   [brfss_validate_state_codebook()] for full schema requirements.
#' @param path Optional explicit config path. Defaults to
#'   `tools::R_user_dir("brfssTools", "config")`.
#' @param overwrite Logical. If FALSE (default), errors when the config dir
#'   already exists and is non-empty. Set TRUE to clobber an existing
#'   config. Use with caution: this REPLACES the current config dir.
#' @param seed_from_cdc Logical. If TRUE (default), populate `crosswalk.csv`
#'   with the shipped CDC seed (one row per CDC variable per year, all
#'   marked `unverified = 1`). If FALSE, start with an empty crosswalk.
#' @return The resolved config path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Try out the workflow with the shipped demo:
#' brfss_init_state("DEMO")
#'
#' # Initialize a real state with your own codebook (CDC-seeded crosswalk):
#' brfss_init_state(
#'   state = "OR",
#'   state_codebook_path = "~/data/oregon_brfss_codebook.csv"
#' )
#'
#' # Same, but with an empty crosswalk (no CDC seeding):
#' brfss_init_state(
#'   state = "OR",
#'   state_codebook_path = "~/data/oregon_brfss_codebook.csv",
#'   seed_from_cdc = FALSE
#' )
#'
#' # Project-local config (for git-tracked epi projects):
#' brfss_init_state(
#'   state = "WA",
#'   state_codebook_path = "~/data/wa_codebook.csv",
#'   path  = "~/projects/aces-paper/brfss_config"
#' )
#' }
brfss_init_state <- function(state,
                             state_codebook_path = NULL,
                             path = NULL,
                             overwrite = FALSE,
                             seed_from_cdc = TRUE) {
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

  if (is_demo) {
    .copy_demo_state(pkg_extdata, cfg)
    if (seed_from_cdc) {
      .seed_crosswalk_from_cdc(pkg_extdata, cfg)
      msg <- paste0(
        "Initialized DEMO config with seeded CDC crosswalk. ",
        "All CDC variables are present with state_var = NA and unverified = 1. ",
        "Open the editor to start mapping your state codebook columns."
      )
    } else {
      msg <- paste0(
        "Initialized DEMO config with empty crosswalk. ",
        "Pass seed_from_cdc = TRUE to start with all CDC variables pre-seeded."
      )
    }
  } else {
    if (!file.exists(state_codebook_path)) {
      stop("state_codebook_path does not exist: ", state_codebook_path,
           call. = FALSE)
    }
    cb <- readr::read_csv(state_codebook_path, show_col_types = FALSE,
                          locale = readr::locale(encoding = "UTF-8"))
    cb <- brfss_validate_state_codebook(cb)
    readr::write_csv(cb, file.path(cfg, "state_codebook.csv"), na = "")

    empty_so <- tibble::tibble(
      concept_id = character(),
      year       = integer(),
      state_var  = character(),
      source     = character(),
      notes      = character()
    )
    readr::write_csv(empty_so, file.path(cfg, "state_only.csv"), na = "")

    if (seed_from_cdc) {
      .seed_crosswalk_from_cdc(pkg_extdata, cfg)
      msg <- sprintf(
        "Initialized config for state '%s' with seeded CDC crosswalk. All CDC variables are present with state_var = NA and unverified = 1. Open brfss_crosswalk_editor() to map your state codebook columns.",
        state
      )
    } else {
      empty_cw <- tibble::tibble(
        concept_id = character(),
        year       = integer(),
        state_var  = character(),
        cdc_var    = character(),
        is_primary = integer(),
        source     = character(),
        score      = numeric(),
        notes      = character(),
        unverified = integer()
      )
      readr::write_csv(empty_cw, file.path(cfg, "crosswalk.csv"), na = "")
      msg <- sprintf(
        "Initialized empty config for state '%s'. Open brfss_crosswalk_editor() to start mapping variables.",
        state
      )
    }
  }

  dir.create(file.path(cfg, "transformations"),
             recursive = TRUE, showWarnings = FALSE)

  config_yaml <- list(
    state = state,
    init_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    init_source = if (is_demo) "package_demo" else "user_codebook",
    state_codebook_source = if (!is.null(state_codebook_path))
                              normalizePath(state_codebook_path) else NA,
    package_version = as.character(utils::packageVersion("brfssTools"))
  )
  .write_simple_yaml(config_yaml, file.path(cfg, "config.yaml"))

  message(msg)
  message("Config dir: ", cfg)
  invisible(cfg)
}

#' States with a built-in sample shipped in the package
#'
#' Currently only `"DEMO"` ships with the package. Real states require a
#' user-supplied codebook via `state_codebook_path` in
#' [brfss_init_state()].
#'
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

# Copy the package-shipped DEMO codebook into the config dir.
# Note: does NOT write crosswalk.csv. The caller is expected to either
# call .seed_crosswalk_from_cdc() or write an empty crosswalk explicitly.
.copy_demo_state <- function(pkg_extdata, cfg) {
  src <- file.path(pkg_extdata, "state_codebook_demo.csv")
  dst <- file.path(cfg, "state_codebook.csv")
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
  } else {
    stop("DEMO state codebook missing from package extdata. ",
         "Reinstall brfssTools.", call. = FALSE)
  }

  empty_so <- tibble::tibble(
    concept_id = character(),
    year       = integer(),
    state_var  = character(),
    source     = character(),
    notes      = character()
  )
  readr::write_csv(empty_so, file.path(cfg, "state_only.csv"), na = "")

  invisible(TRUE)
}

# Copy the shipped CDC seed crosswalk into the config dir as crosswalk.csv.
# All rows ship with state_var = NA, source = "cdc_seed", unverified = 1.
.seed_crosswalk_from_cdc <- function(pkg_extdata, cfg) {
  src <- file.path(pkg_extdata, "cdc_seed.csv")
  dst <- file.path(cfg, "crosswalk.csv")
  if (!file.exists(src)) {
    stop("CDC seed crosswalk missing from package extdata: ", src, "\n",
         "Reinstall brfssTools.", call. = FALSE)
  }
  file.copy(src, dst, overwrite = TRUE)
  invisible(TRUE)
}

# Tiny YAML writer that doesn't add a yaml dependency. Handles the simple
# flat key:value structure we use for config.yaml only.
.write_simple_yaml <- function(x, path) {
  lines <- character(length(x))
  for (i in seq_along(x)) {
    val <- x[[i]]
    if (is.na(val)) {
      lines[i] <- sprintf("%s: ~", names(x)[i])
    } else {
      # Quote strings with spaces or special chars; leave numbers bare
      v <- as.character(val)
      if (grepl("[ :#'\"]", v) || v == "") v <- sprintf('"%s"', gsub('"', '\\"', v))
      lines[i] <- sprintf("%s: %s", names(x)[i], v)
    }
  }
  writeLines(lines, path)
  invisible(path)
}

# Read the config.yaml back. Returns a named list. Returns NULL if file
# doesn't exist. Uses a simple parser to avoid yaml-package dependency.
.read_simple_yaml <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  out <- list()
  for (ln in lines) {
    ln <- trimws(ln)
    if (!nzchar(ln) || startsWith(ln, "#")) next
    parts <- regmatches(ln, regexec("^([^:]+):\\s*(.*)$", ln))[[1]]
    if (length(parts) < 3) next
    key <- trimws(parts[2])
    val <- trimws(parts[3])
    if (val == "~" || val == "") {
      val <- NA
    } else if (grepl('^".*"$', val)) {
      val <- sub('^"(.*)"$', '\\1', val)
    }
    out[[key]] <- val
  }
  out
}
