# R/paths.R
#
# Single helper for resolving where the user's brfssTools config lives.
# All functions that read or write user-owned data go through here.

#' Resolve the brfssTools config path
#'
#' Priority order (highest first):
#' 1. Explicit `path` argument
#' 2. R option `brfssTools.config_path`
#' 3. Environment variable `BRFSSTOOLS_CONFIG_PATH`
#' 4. `tools::R_user_dir("brfssTools", which = "config")`
#'
#' If `must_exist = TRUE` (default), errors when no candidate path exists
#' on disk. If `must_exist = FALSE`, returns the resolved path even if it
#' doesn't exist yet (useful for `brfss_init_state()`).
#'
#' Use `package_extdata()` (separate function) for read-only access to the
#' package's shipped CDC codebook and templates.
#'
#' @param path Optional explicit path. If supplied, takes priority.
#' @param must_exist Logical. If TRUE, error when resolved path doesn't exist.
#' @return Character path.
#' @export
brfss_config_path <- function(path = NULL, must_exist = TRUE) {
  candidate <- .brfss_resolve_config_path(path)
  if (must_exist && !dir.exists(candidate)) {
    stop(
      "brfssTools config dir not found at: ", candidate, "\n",
      "Run brfss_init_state(state = ..., state_codebook_path = ...) first, ",
      "or pass an explicit `path =` argument.",
      call. = FALSE
    )
  }
  candidate
}

# Internal resolver. Returns a path string; doesn't check existence.
.brfss_resolve_config_path <- function(path = NULL) {
  if (!is.null(path) && nzchar(path)) return(normalizePath(path, mustWork = FALSE))

  opt <- getOption("brfssTools.config_path", default = NULL)
  if (!is.null(opt) && nzchar(opt)) return(normalizePath(opt, mustWork = FALSE))

  env <- Sys.getenv("BRFSSTOOLS_CONFIG_PATH", unset = "")
  if (nzchar(env)) return(normalizePath(env, mustWork = FALSE))

  tools::R_user_dir("brfssTools", which = "config")
}

# Locate the package's installed extdata. Falls back to source tree when
# running under devtools::load_all().
.brfss_package_extdata <- function() {
  p <- system.file("extdata", package = "brfssTools")
  if (nzchar(p) && dir.exists(p)) return(p)

  pkg_root <- tryCatch(find.package("brfssTools"), error = function(e) NULL)
  if (!is.null(pkg_root)) {
    candidate <- file.path(pkg_root, "inst", "extdata")
    if (dir.exists(candidate)) return(candidate)
  }
  stop(
    "Could not locate brfssTools' shipped extdata. ",
    "Reinstall the package, or load with devtools::load_all() from a ",
    "directory containing inst/extdata/.",
    call. = FALSE
  )
}

# Where does a particular file live, given the resolution rules?
# - cdc_codebook.csv, cdc_calculated_vars.csv, taxonomy.csv: package extdata
# - everything else: user config dir
# Returns NULL if the file doesn't exist anywhere we can find it.
.brfss_locate_file <- function(fname, config_path = NULL) {
  package_files <- c(
    "cdc_codebook.csv",
    "cdc_calculated_vars.csv",
    "cdc_only.csv",
    "taxonomy.csv"
  )

  if (fname %in% package_files) {
    pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
    if (!is.null(pkg)) {
      fp <- file.path(pkg, fname)
      if (file.exists(fp)) return(fp)
    }
  }

  # Try config dir for everything else (and for package files that the user
  # might have copied locally to override)
  cfg <- tryCatch(brfss_config_path(config_path, must_exist = FALSE),
                  error = function(e) NULL)
  if (!is.null(cfg)) {
    fp <- file.path(cfg, fname)
    if (file.exists(fp)) return(fp)
  }

  # User-local override of a package file: try config dir even for
  # package_files that weren't in extdata
  if (fname %in% package_files && !is.null(cfg)) {
    fp <- file.path(cfg, fname)
    if (file.exists(fp)) return(fp)
  }

  NULL
}
