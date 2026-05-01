# R/editor_app.R
#
# Launches the Shiny crosswalk editor against a registered pool's
# crosswalk file.

#' Launch the interactive crosswalk editor
#'
#' Opens the Shiny editor for one registered pool's crosswalk. The
#' editor reads from and writes to the crosswalk path registered by
#' \code{brfss_set_pool()}.
#'
#' Requires \code{shiny}, \code{bslib}, and \code{htmltools}; will use
#' \code{shinyAce} for syntax-highlighted YAML editing if installed.
#'
#' @param dataset Character. Registered dataset name. If NULL and
#'   exactly one pool is registered, uses that one.
#' @param launch.browser Logical, passed to \code{shiny::runApp()}.
#'   Default TRUE.
#' @param port Optional port override.
#' @return Invisible NULL after the app exits.
#' @export
brfss_crosswalk_editor <- function(dataset = NULL,
                                   launch.browser = TRUE,
                                   port = NULL) {
  for (pkg in c("shiny", "bslib", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("brfss_crosswalk_editor() requires the '", pkg,
            "' package. Install it with: install.packages(\"", pkg, "\")",
            call. = FALSE)
    }
  }

  ds <- .brfss_resolve_dataset(dataset)
  cfg <- .brfss_get_pool(ds)

  if (!file.exists(cfg$crosswalk)) {
    stop("Crosswalk does not exist at: ", cfg$crosswalk, "\n",
          "Run brfss_draft_crosswalk(dataset = \"", ds, "\") first.",
          call. = FALSE)
  }

  app_dir <- system.file("shiny", "editor", package = "brfssTools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    pkg_root <- tryCatch(find.package("brfssTools"),
                          error = function(e) NULL)
    if (!is.null(pkg_root)) {
      candidate <- file.path(pkg_root, "inst", "shiny", "editor")
      if (dir.exists(candidate)) app_dir <- candidate
    }
  }
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Editor app missing. Reinstall brfssTools.", call. = FALSE)
  }

  # Pass dataset through env var. (Function-scoped state is fragile
  # across Shiny's evaluation contexts.)
  Sys.setenv(BRFSSTOOLS_EDITOR_DATASET = ds)
  on.exit(Sys.unsetenv("BRFSSTOOLS_EDITOR_DATASET"), add = TRUE)

  args <- list(appDir = app_dir, launch.browser = launch.browser)
  if (!is.null(port)) args$port <- port

  do.call(shiny::runApp, args)
  invisible(NULL)
}
