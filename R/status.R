# R/status.R
#
# brfss_status() — show where everything lives (config, pools, cache).
# brfss_reset()  — wipe back to DEMO with automatic backup.

#' Show where brfssTools is reading and writing
#'
#' Prints a summary of the active config directory, the registered state,
#' what files are present, which pools are registered, and where the cache
#' lives. Run this any time you're not sure where the package is looking.
#'
#' @param path Optional config dir override. Defaults to the resolved
#'   config path (`brfss_config_path()`).
#' @return Invisibly, a list of the surfaced information.
#' @export
#' @examples
#' \dontrun{
#' brfss_status()
#' }
brfss_status <- function(path = NULL) {
  cfg <- tryCatch(brfss_config_path(path, must_exist = FALSE),
                  error = function(e) NULL)
  cfg_exists <- !is.null(cfg) && dir.exists(cfg)

  # Read config.yaml if present
  state <- NA_character_
  init_date <- NA_character_
  init_source <- NA_character_
  if (cfg_exists) {
    yml <- tryCatch(.read_simple_yaml(file.path(cfg, "config.yaml")),
                    error = function(e) NULL)
    if (!is.null(yml)) {
      state <- yml$state %||% NA_character_
      init_date <- yml$init_date %||% NA_character_
      init_source <- yml$init_source %||% NA_character_
    }
  }

  # Files in config dir
  config_files <- list()
  if (cfg_exists) {
    file_summary <- function(fp, count_rows = TRUE) {
      if (!file.exists(fp)) return(NULL)
      info <- file.info(fp)
      if (count_rows && grepl("\\.csv$", fp, ignore.case = TRUE)) {
        n <- tryCatch({
          length(readLines(fp, warn = FALSE)) - 1L
        }, error = function(e) NA_integer_)
        list(size = info$size, rows = n)
      } else {
        list(size = info$size, rows = NA_integer_)
      }
    }
    config_files <- list(
      crosswalk      = file_summary(file.path(cfg, "crosswalk.csv")),
      state_codebook = file_summary(file.path(cfg, "state_codebook.csv")),
      config_yaml    = file_summary(file.path(cfg, "config.yaml"),
                                     count_rows = FALSE)
    )
  }

  # Registered pools
  pool_status <- tryCatch(brfss_pool_status(), error = function(e) NULL)
  pool_summary <- if (is.null(pool_status) || nrow(pool_status) == 0L) {
    "(none registered)"
  } else {
    sprintf("%s (%d files, years %d-%d)",
            pool_status$dataset, length(pool_status$file),
            min(pool_status$year, na.rm = TRUE),
            max(pool_status$year, na.rm = TRUE))
  }

  # Cache dir
  cache_dir <- tryCatch(brfss_cache_dir(), error = function(e) NA_character_)
  cache_n_files <- if (!is.na(cache_dir) && dir.exists(cache_dir)) {
    length(list.files(cache_dir, pattern = "\\.(xpt|csv)$",
                      ignore.case = TRUE))
  } else 0L

  # ------ Print ------
  cat("\nbrfssTools status\n")
  cat("-----------------\n")

  cat("\nConfig directory:\n")
  if (cfg_exists) {
    cat("  ", cfg, "\n", sep = "")
    cat("  state:       ", state, "\n", sep = "")
    cat("  initialized: ", init_date, " (", init_source, ")\n", sep = "")
  } else if (!is.null(cfg)) {
    cat("  ", cfg, " (does not exist yet)\n", sep = "")
    cat("  Run brfss_init_state() to set up.\n")
  } else {
    cat("  (none resolved)\n")
  }

  cat("\nConfig files:\n")
  if (cfg_exists) {
    for (nm in names(config_files)) {
      info <- config_files[[nm]]
      if (is.null(info)) {
        cat(sprintf("  %-18s missing\n", nm))
      } else if (!is.na(info$rows)) {
        cat(sprintf("  %-18s %d rows  (%s)\n",
                    nm, info$rows, .format_size(info$size)))
      } else {
        cat(sprintf("  %-18s (%s)\n", nm, .format_size(info$size)))
      }
    }
  } else {
    cat("  (n/a)\n")
  }

  cat("\nRegistered data pools:\n")
  if (length(pool_summary) == 1L && pool_summary == "(none registered)") {
    cat("  (none registered)\n")
    cat("  Use brfss_set_pool() to register.\n")
  } else {
    for (s in pool_summary) cat("  ", s, "\n", sep = "")
  }

  cat("\nDownload cache:\n")
  if (!is.na(cache_dir)) {
    cat("  ", cache_dir, "\n", sep = "")
    cat("  ", cache_n_files, " file(s)\n", sep = "")
  } else {
    cat("  (n/a)\n")
  }
  cat("\n")

  invisible(list(
    config_dir   = cfg,
    state        = state,
    init_date    = init_date,
    config_files = config_files,
    pools        = pool_status,
    cache_dir    = cache_dir,
    cache_n_files = cache_n_files
  ))
}

# Format a byte size as a human-readable string
.format_size <- function(bytes) {
  if (is.na(bytes)) return("?")
  if (bytes < 1024) return(sprintf("%d B", bytes))
  if (bytes < 1024^2) return(sprintf("%.1f KB", bytes / 1024))
  if (bytes < 1024^3) return(sprintf("%.1f MB", bytes / 1024^2))
  sprintf("%.2f GB", bytes / 1024^3)
}

# ============================================================================
# brfss_reset
# ============================================================================

#' Reset the config directory back to the DEMO state
#'
#' Wipes the active config directory and reinitializes it with the shipped
#' DEMO state codebook (15 fake variables, 3 years), an empty crosswalk,
#' and an empty transformations folder.
#'
#' Before wiping, automatically creates a timestamped backup of the current
#' config dir at `<config>_backup_<timestamp>` (alongside the config dir),
#' so nothing is lost. The function prints the backup location.
#'
#' This is a destructive operation. Use it when:
#' \itemize{
#'   \item You want to start over from scratch with a clean demo
#'   \item You're testing the new-user experience
#'   \item Your crosswalk has accumulated edits you want to throw away
#' }
#'
#' Your real work can also be saved before resetting via
#' [brfss_export_config()] — that produces a portable zip you can later
#' restore with [brfss_import_config()].
#'
#' @param path Optional config dir override.
#' @param confirm Logical. If TRUE (default in non-interactive sessions),
#'   skip the interactive confirmation prompt. Mostly for scripts.
#' @return The reset config path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Reset to DEMO; this prompts for confirmation in interactive sessions:
#' brfss_reset()
#'
#' # Skip prompt (e.g., in a test script):
#' brfss_reset(confirm = TRUE)
#' }
brfss_reset <- function(path = NULL, confirm = !interactive()) {
  cfg <- brfss_config_path(path, must_exist = FALSE)

  if (!dir.exists(cfg)) {
    message("Config dir does not exist; initializing DEMO at:\n  ", cfg)
    return(invisible(brfss_init_state("DEMO", path = path, overwrite = FALSE)))
  }

  # Show what's about to be lost
  cat("\nThis will RESET the config directory back to DEMO:\n")
  cat("  ", cfg, "\n\n", sep = "")

  # Count what's there
  n_files <- length(list.files(cfg, recursive = TRUE))
  cat("  Currently contains: ", n_files, " file(s)\n", sep = "")

  # Backup target
  backup <- paste0(cfg, "_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  cat("  Will be backed up to: ", backup, "\n\n", sep = "")

  if (!confirm && interactive()) {
    ans <- readline("Continue? (y/N): ")
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Cancelled. Nothing changed.")
      return(invisible(NULL))
    }
  }

  # Backup
  parent_dir <- dirname(backup)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- file.copy(cfg, dirname(backup), recursive = TRUE,
                   copy.date = TRUE)
  # file.copy of a dir copies it INTO the destination; need to rename
  copied_to <- file.path(dirname(backup), basename(cfg))
  if (file.exists(copied_to) && copied_to != backup) {
    file.rename(copied_to, backup)
  }
  if (!dir.exists(backup)) {
    stop("Backup failed; aborting reset to avoid data loss.", call. = FALSE)
  }
  message("Backup written to:\n  ", backup)

  # Wipe and reinit
  unlink(cfg, recursive = TRUE, force = TRUE)
  brfss_init_state("DEMO", path = path, overwrite = TRUE)

  message("Reset complete. Config is now DEMO.")
  message("To restore your previous work:")
  message("  brfss_import_config(<some_zip>)  # if you exported beforehand")
  message("  ... or copy files manually from:")
  message("  ", backup)

  invisible(cfg)
}
