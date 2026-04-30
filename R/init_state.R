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
#' Sets up `tools::R_user_dir("brfssTools", "config")` (or a custom path) with
#' the files needed to use the crosswalk editor and `brfss_pull()`. After
#' this runs once, all other brfssTools functions can find your state
#' codebook and crosswalk automatically.
#'
#' Two modes of operation:
#'
#' \strong{Sample state.} `brfss_init_state("OR")` copies the package's
#' shipped Oregon sample (full crosswalk, state codebook, state_only,
#' taxonomy) into the config dir as a starting point. Useful for trying the
#' workflow or as a reference example.
#'
#' \strong{New state.} `brfss_init_state("WA", state_codebook_path = "...")`
#' reads and validates the user-supplied state codebook, copies it in, and
#' creates empty `crosswalk.csv` and `state_only.csv` files for the user to
#' populate via the editor.
#'
#' @param state Two-letter state postal code (used in config metadata only;
#'   the crosswalk file is named `crosswalk.csv` regardless).
#' @param state_codebook_path Path to the user's state codebook CSV. Required
#'   unless `state %in% sampled_states()`.
#' @param path Optional explicit config path. Defaults to
#'   `tools::R_user_dir("brfssTools", "config")`.
#' @param overwrite Logical. If FALSE (default), errors when the config dir
#'   already exists. Set TRUE to clobber an existing config.
#' @return The resolved config path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Initialize using the package's shipped Oregon sample:
#' brfss_init_state("OR")
#'
#' # Initialize a new state with your own codebook:
#' brfss_init_state(
#'   state = "WA",
#'   state_codebook_path = "~/data/wa_brfss_codebook.csv"
#' )
#'
#' # Project-local config (for git-tracked epi projects):
#' brfss_init_state(
#'   state = "OR",
#'   path = "~/projects/aces-paper/brfss_config"
#' )
#' }
brfss_init_state <- function(state,
                             state_codebook_path = NULL,
                             path = NULL,
                             overwrite = FALSE) {
  if (missing(state) || !is.character(state) || length(state) != 1L ||
      !nzchar(state)) {
    stop("`state` must be a two-letter state postal code, e.g. \"OR\".",
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

  is_sample_state <- state %in% sampled_states()

  if (!is_sample_state && is.null(state_codebook_path)) {
    stop(
      "State '", state, "' is not a built-in sample. ",
      "Provide `state_codebook_path` pointing at your state's codebook CSV.\n",
      "Built-in samples: ", paste(sampled_states(), collapse = ", "),
      call. = FALSE
    )
  }

  pkg_extdata <- .brfss_package_extdata()

  if (is_sample_state && is.null(state_codebook_path)) {
    # Copy the package's shipped sample for this state.
    .copy_sample_state(state, pkg_extdata, cfg)
    msg <- sprintf("Initialized config for sample state '%s' from package data.", state)

  } else {
    # User supplied a codebook. Validate, copy, init empty crosswalk + state_only.
    if (!file.exists(state_codebook_path)) {
      stop("state_codebook_path does not exist: ", state_codebook_path,
           call. = FALSE)
    }

    cb <- readr::read_csv(state_codebook_path, show_col_types = FALSE,
                          locale = readr::locale(encoding = "UTF-8"))
    cb <- brfss_validate_state_codebook(cb)
    readr::write_csv(cb, file.path(cfg, "state_codebook.csv"), na = "")

    # Empty crosswalk + state_only with proper schemas
    empty_cw <- tibble::tibble(
      concept_id = character(),
      year       = integer(),
      state_var  = character(),
      cdc_var    = character(),
      is_primary = integer(),
      source     = character(),
      score      = numeric(),
      notes      = character()
    )
    empty_so <- tibble::tibble(
      concept_id = character(),
      year       = integer(),
      state_var  = character(),
      source     = character(),
      notes      = character()
    )
    readr::write_csv(empty_cw, file.path(cfg, "crosswalk.csv"), na = "")
    readr::write_csv(empty_so, file.path(cfg, "state_only.csv"), na = "")

    msg <- sprintf("Initialized empty config for state '%s'. Use brfss_crosswalk_editor() to map variables.", state)
  }

  # Always ensure a transformations/ subdir exists for user functions
  dir.create(file.path(cfg, "transformations"),
             recursive = TRUE, showWarnings = FALSE)

  # Stamp config.yaml
  config_yaml <- list(
    state = state,
    init_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    init_source = if (is_sample_state) "package_sample" else "user_codebook",
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
#' @return Character vector of state postal codes.
#' @export
sampled_states <- function() {
  pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
  if (is.null(pkg)) return(character(0))
  # Sample = a state for which we ship crosswalk + state_codebook + state_only
  # Currently OR is hard-coded since that's what the package was built around.
  if (file.exists(file.path(pkg, "crosswalk.csv")) &&
      file.exists(file.path(pkg, "state_codebook.csv"))) {
    return("OR")
  }
  character(0)
}

# Copy package-shipped sample state files into the config dir.
.copy_sample_state <- function(state, pkg_extdata, cfg) {
  # Currently OR is the only sample. If we ship others later (WA, etc.),
  # this could look in inst/extdata/samples/<state>/ instead.
  files_to_copy <- c(
    "crosswalk.csv",
    "state_codebook.csv",
    "state_only.csv",
    "pending.csv"
  )
  for (f in files_to_copy) {
    src <- file.path(pkg_extdata, f)
    if (file.exists(src)) {
      file.copy(src, file.path(cfg, f), overwrite = TRUE)
    }
  }
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
