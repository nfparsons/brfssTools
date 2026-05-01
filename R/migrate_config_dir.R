# R/migrate_config_dir.R
#
# One-shot helper for users who have a v0.2.0-with-config-dir state and
# want to move to the path-based architecture.

#' Migrate from a config-dir install to the path-based architecture
#'
#' Earlier v0.2.0 development used a persistent config dir at
#' \code{tools::R_user_dir("brfssTools", "config")}. The released v0.2.0
#' drops the config dir in favor of explicit paths registered via
#' \code{brfss_set_pool()}. If you have a config dir from previous use,
#' this function copies its contents to a target directory of your
#' choice — typically \code{<your_data_path>/documentation/} or a
#' project-local folder.
#'
#' Copies (if present): the crosswalk, the state codebook (legacy
#' format), and a backup of any \code{config.yaml}.
#'
#' @param target_dir Directory to copy contents into. Will be created
#'   if needed.
#' @param dataset Dataset name; used to rename \code{crosswalk.csv} to
#'   \code{<dataset>_crosswalk.csv}.
#' @param config_dir Optional path to the source config dir. Defaults
#'   to \code{tools::R_user_dir("brfssTools", "config")}.
#' @param remove_source Logical. If TRUE (default FALSE), removes the
#'   source config dir after a successful copy.
#' @return The target_dir, invisibly.
#' @export
#' @examples
#' \dontrun{
#' brfss_migrate_config_dir(
#'   target_dir = "C:/projects/oregon/documentation",
#'   dataset    = "OR"
#' )
#' }
brfss_migrate_config_dir <- function(target_dir, dataset,
                                     config_dir = NULL,
                                     remove_source = FALSE) {
  if (missing(target_dir) || !is.character(target_dir) ||
      length(target_dir) != 1L) {
    stop("`target_dir` is required.", call. = FALSE)
  }
  if (missing(dataset) || !is.character(dataset) ||
      length(dataset) != 1L) {
    stop("`dataset` is required.", call. = FALSE)
  }

  if (is.null(config_dir)) {
    config_dir <- tools::R_user_dir("brfssTools", which = "config")
  }
  if (!dir.exists(config_dir)) {
    stop(sprintf("Config dir not found at: %s\n", config_dir),
          "Nothing to migrate.", call. = FALSE)
  }

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Copy crosswalk.csv -> <dataset>_crosswalk.csv
  src_cw <- file.path(config_dir, "crosswalk.csv")
  if (file.exists(src_cw)) {
    dest_cw <- file.path(target_dir,
                          sprintf("%s_crosswalk.csv", dataset))
    file.copy(src_cw, dest_cw, overwrite = TRUE)
    message(sprintf("Copied: %s\n      \u2192 %s", src_cw, dest_cw))
  } else {
    message("No crosswalk.csv in source config dir; skipping.")
  }

  # Copy state_codebook.csv if present (legacy format; user may want it)
  src_sc <- file.path(config_dir, "state_codebook.csv")
  if (file.exists(src_sc)) {
    dest_sc <- file.path(target_dir,
                          sprintf("%s_state_codebook_legacy.csv", dataset))
    file.copy(src_sc, dest_sc, overwrite = TRUE)
    message(sprintf("Copied: %s\n      \u2192 %s", src_sc, dest_sc))
    message("  (Legacy state-codebook format; convert to per-year ",
             "codebook CSVs if you want editor codebook context.)")
  }

  # Copy any .bak files
  baks <- list.files(config_dir, pattern = "\\.bak$", full.names = TRUE)
  for (bak in baks) {
    file.copy(bak, file.path(target_dir, basename(bak)),
               overwrite = TRUE)
  }
  if (length(baks) > 0L) {
    message(sprintf("Copied %d backup file(s).", length(baks)))
  }

  message("\nNext: register a pool against your data and the new ",
           "documentation location:")
  message(sprintf(
    '  brfss_set_pool(dataset = "%s", data_path = "<your data dir>",\n               codebook_path = "%s",\n               crosswalk = "%s")',
    dataset, target_dir,
    file.path(target_dir, sprintf("%s_crosswalk.csv", dataset))
  ))

  if (remove_source) {
    unlink(config_dir, recursive = TRUE)
    message(sprintf("\nRemoved source config dir: %s", config_dir))
  } else {
    message(sprintf("\nSource config dir preserved at: %s", config_dir))
    message("Pass remove_source = TRUE to delete it after verification.")
  }

  invisible(target_dir)
}
