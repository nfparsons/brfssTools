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

  app_dir <- system.file("shiny", "editor", package = "brfssTools")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Editor app missing. Reinstall brfssTools.", call. = FALSE)
  }

  # Pass the path through an environment variable that the Shiny app reads
  # at startup. (Alternatives like assigning to a package-internal env are
  # fragile across Shiny's evaluation contexts.)
  if (is.null(path)) path <- system.file("extdata", package = "brfssTools")
  Sys.setenv(BRFSSTOOLS_EDITOR_PATH = path)
  on.exit(Sys.unsetenv("BRFSSTOOLS_EDITOR_PATH"), add = TRUE)

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port
  )
}
