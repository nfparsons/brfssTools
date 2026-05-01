# R/status.R
#
# v0.2.0: brfss_status() reports on the registered pools. There is no
# config dir.

#' Show registered pools and their resolved paths
#'
#' Prints a summary of every registered pool, including the data path,
#' codebook path, crosswalk path, and how many data files are indexed
#' per dataset.
#'
#' @return Invisibly, the structured status (one entry per registered
#'   pool).
#' @export
brfss_status <- function() {
  pool_names <- .brfss_list_pools()

  cat("\nbrfssTools status\n")
  cat("-----------------\n\n")

  if (length(pool_names) == 0L) {
    cat("No pools registered.\n\n")
    cat("Get started:\n")
    cat("  brfss_set_pool(dataset = \"OR\", data_path = \"/path/to/data\")\n\n")
    return(invisible(list()))
  }

  out <- list()
  for (ds in pool_names) {
    cfg <- .brfss_get_pool(ds)
    cat(sprintf("Dataset: %s\n", ds))
    cat(sprintf("  data_path:     %s\n", cfg$data_path))

    cb_status <- if (dir.exists(cfg$codebook_path)) {
      pattern <- sprintf("^%s_[0-9]{4}_codebook\\.csv$", ds)
      n <- length(list.files(cfg$codebook_path, pattern = pattern))
      sprintf("(%d codebook file(s))", n)
    } else {
      "(directory does not yet exist)"
    }
    cat(sprintf("  codebook_path: %s  %s\n", cfg$codebook_path, cb_status))

    cw_status <- if (file.exists(cfg$crosswalk)) {
      n <- tryCatch({
        length(readLines(cfg$crosswalk, warn = FALSE)) - 1L
      }, error = function(e) NA_integer_)
      if (is.na(n)) "(unreadable)" else sprintf("(%d rows)", n)
    } else {
      "(does not yet exist; run brfss_draft_crosswalk())"
    }
    cat(sprintf("  crosswalk:     %s  %s\n", cfg$crosswalk, cw_status))

    n_files <- length(cfg$files)
    if (n_files == 0L) {
      cat("  data files:    (none indexed)\n")
    } else {
      yrs <- sort(as.integer(names(cfg$files)))
      cat(sprintf("  data files:    %d file(s), years %d-%d\n",
                   n_files, min(yrs), max(yrs)))
    }
    cat("\n")

    out[[ds]] <- cfg
  }

  invisible(out)
}
