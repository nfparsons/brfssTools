# R/config_io.R
#
# Export and import a brfssTools config directory as a portable zip file.
# Useful for moving work between machines, archiving snapshots, or sharing
# a curated crosswalk with collaborators.

#' Export the brfssTools config directory as a portable zip
#'
#' Bundles `crosswalk.csv`, `state_codebook.csv`, `state_only.csv`,
#' `pending.csv`, `config.yaml`, and the entire `transformations/`
#' subdirectory into a single zip file. Useful for moving work between
#' machines, archiving snapshots, or sharing a curated crosswalk with
#' collaborators.
#'
#' Things NOT included:
#' \itemize{
#'   \item `.bak` backup files (kept locally only)
#'   \item Anything in the package's `inst/extdata/` (the CDC codebook,
#'     taxonomy — these come from the package install on the destination
#'     machine, not from this archive)
#'   \item Cached data files (see [brfss_cache_dir()] for those)
#' }
#'
#' @param zip_path Where to write the zip file. Defaults to
#'   `brfssTools_config_<date>.zip` in the current working directory.
#' @param path     Optional config dir override. Defaults to
#'   `brfss_config_path()`.
#' @return The zip file path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Snapshot current config before a major refactor:
#' brfss_export_config("~/snapshots/brfss_2026-04-30.zip")
#'
#' # Move work to a new machine:
#' brfss_export_config("/share/oregon_crosswalk_v1.zip")
#' # ... copy the zip over, then on the new machine:
#' brfss_import_config("/share/oregon_crosswalk_v1.zip")
#' }
brfss_export_config <- function(zip_path = NULL, path = NULL) {
  cfg <- brfss_config_path(path)

  if (is.null(zip_path)) {
    zip_path <- file.path(
      getwd(),
      sprintf("brfssTools_config_%s.zip", format(Sys.Date(), "%Y-%m-%d"))
    )
  }
  zip_path <- normalizePath(zip_path, mustWork = FALSE)

  # Files to include (relative to config dir)
  include <- c(
    "crosswalk.csv",
    "state_codebook.csv",
    "state_only.csv",
    "pending.csv",
    "config.yaml",
    "concept_taxonomy.csv"
  )
  files <- include[file.exists(file.path(cfg, include))]

  # The transformations subdir, recursively (skip any .bak files)
  trans_dir <- file.path(cfg, "transformations")
  if (dir.exists(trans_dir)) {
    trans_files <- list.files(trans_dir, recursive = TRUE,
                              all.files = FALSE, no.. = TRUE)
    trans_files <- trans_files[!grepl("\\.bak$", trans_files)]
    files <- c(files, file.path("transformations", trans_files))
  }

  if (length(files) == 0L) {
    stop("Nothing to export. Config dir appears empty: ", cfg, call. = FALSE)
  }

  # Use utils::zip with relative paths so the archive unpacks cleanly
  old_wd <- setwd(cfg)
  on.exit(setwd(old_wd), add = TRUE)

  result <- tryCatch(
    utils::zip(zipfile = zip_path, files = files, flags = "-r9Xq"),
    error = function(e) e
  )
  if (inherits(result, "error") || result != 0L) {
    msg <- if (inherits(result, "error")) conditionMessage(result) else
             sprintf("zip returned code %s", result)
    stop("Failed to create zip: ", msg, call. = FALSE)
  }

  message(sprintf("Exported %d file(s) to:\n  %s", length(files), zip_path))
  invisible(zip_path)
}

#' Import a brfssTools config from an exported zip
#'
#' Unpacks a zip created by [brfss_export_config()] into the target config
#' directory. By default unpacks into the user's standard config dir; pass
#' `path` to install into a project-local location instead.
#'
#' If the destination already has files, this errors unless `overwrite = TRUE`.
#'
#' @param zip_path Path to the zip file to import.
#' @param path     Optional config dir override. Defaults to
#'   `brfss_config_path(must_exist = FALSE)`.
#' @param overwrite Logical. If TRUE, replaces existing files in the
#'   destination. Default FALSE.
#' @return The destination config path, invisibly.
#' @export
brfss_import_config <- function(zip_path, path = NULL, overwrite = FALSE) {
  if (missing(zip_path) || !is.character(zip_path) ||
      length(zip_path) != 1L || !nzchar(zip_path)) {
    stop("`zip_path` must be a single non-empty string.", call. = FALSE)
  }
  zip_path <- normalizePath(zip_path, mustWork = FALSE)
  if (!file.exists(zip_path)) {
    stop("Zip file not found: ", zip_path, "\n",
         "Did you run brfss_export_config() first?",
         call. = FALSE)
  }

  cfg <- .brfss_resolve_config_path(path)

  if (dir.exists(cfg) && length(list.files(cfg)) > 0 && !overwrite) {
    stop(
      "Destination config dir is not empty:\n  ", cfg, "\n",
      "Set overwrite = TRUE to replace, or pass a different `path =`.",
      call. = FALSE
    )
  }

  dir.create(cfg, recursive = TRUE, showWarnings = FALSE)

  # Extract
  result <- tryCatch(
    utils::unzip(zip_path, exdir = cfg, overwrite = overwrite),
    error = function(e) e
  )
  if (inherits(result, "error") || length(result) == 0L) {
    msg <- if (inherits(result, "error")) conditionMessage(result) else
             "unzip produced no files"
    stop("Failed to unpack zip: ", msg, call. = FALSE)
  }

  # Validate the import landed something usable
  if (!file.exists(file.path(cfg, "crosswalk.csv"))) {
    warning("Import completed, but no crosswalk.csv found at the destination. ",
            "Did the zip come from a brfssTools export?",
            call. = FALSE)
  }

  message(sprintf("Imported %d file(s) into:\n  %s", length(result), cfg))
  invisible(cfg)
}
