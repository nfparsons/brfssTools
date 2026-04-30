# R/editor_app.R
#
# Launches the Shiny crosswalk editor. The app itself lives in
# inst/shiny/editor/ and uses R/crosswalk_io.R as its data layer.

#' Launch the interactive crosswalk editor
#'
#' Opens a local Shiny app for browsing the BRFSS crosswalk by domain and
#' editing pairs. Edits write directly to the package's `inst/extdata/`
#' files (with a `.bak` rotation before each save).
#'
#' Shiny, DT, and bslib must be installed. They are listed in `Suggests:`.
#'
#' @param path Directory containing the canonical crosswalk files. Defaults
#'   to the package's installed `inst/extdata/`.
#' @param launch.browser Passed to `shiny::runApp()`. Default TRUE.
#' @param port           Optional port; default chosen by Shiny.
#'
#' @return Invisible NULL after the app exits.
#' @export
brfss_crosswalk_editor <- function(path = NULL,
                                   launch.browser = TRUE,
                                   port = NULL) {
  for (pkg in c("shiny", "DT", "bslib", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "brfss_crosswalk_editor() requires the '", pkg, "' package. ",
        "Install it with: install.packages(\"", pkg, "\")",
        call. = FALSE
      )
    }
  }

  # Locate the bundled Shiny app. system.file() works when installed; falls
  # back to looking under the loaded package's source directory when running
  # via devtools::load_all().
  app_dir <- system.file("shiny", "editor", package = "brfssTools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # devtools::load_all leaves a 'package:brfssTools' attached but doesn't
    # populate the install dir. find.package() returns the source dir.
    pkg_root <- tryCatch(find.package("brfssTools"), error = function(e) NULL)
    if (!is.null(pkg_root)) {
      candidate <- file.path(pkg_root, "inst", "shiny", "editor")
      if (dir.exists(candidate)) app_dir <- candidate
    }
  }
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Editor app missing. Reinstall brfssTools, or check that ",
         "inst/shiny/editor/ exists in the source tree.", call. = FALSE)
  }

  # Pass the path through an environment variable that the Shiny app reads
  # at startup. (Alternatives like assigning to a package-internal env are
  # fragile across Shiny's evaluation contexts.)
  # Resolve the config path: explicit > option > env > R_user_dir > package.
  # If we end up at the package extdata fallback, the editor opens read-only —
  # we can browse but not save. Saves error with a clear message via cw_save().
  if (is.null(path)) {
    path <- tryCatch(
      brfssTools:::`.cw_default_path`(),
      error = function(e) NULL
    )
  }
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    stop(
      "No brfssTools config found.\n",
      "Run brfss_init_state(state = ..., state_codebook_path = ...) first, ",
      "or pass an explicit `path =` argument.",
      call. = FALSE
    )
  }
  Sys.setenv(BRFSSTOOLS_EDITOR_PATH = path)
  # Note: do NOT unset on.exit. shiny::runApp() returns when the browser
  # opens, but the server function reads the env var lazily on each session.
  # Unsetting here would break the server's bundle load.

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port
  )
}
