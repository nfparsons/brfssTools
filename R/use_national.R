# R/use_national.R
#
# Convenience wrapper for working with CDC's public National BRFSS data.
# Downloads CDC LLCP files to a cache, registers them as a pool. The
# pre-built National crosswalk that would let users skip the drafting
# step is planned for a future release.

#' Use CDC's public National BRFSS data
#'
#' Convenience function for users who want to work with CDC's public
#' BRFSS files. Downloads the requested years' LLCP `.xpt` files to a
#' local cache, then registers them as a pool with sensible defaults.
#'
#' For a National-data user, the typical flow is:
#' \preformatted{
#'   brfss_use_national(years = 2018:2024)
#'   brfss_draft_crosswalk(dataset = "National")
#'   brfss_crosswalk_editor(dataset = "National")
#' }
#'
#' (A pre-built National crosswalk is planned for a future release; once
#' shipped, the draft step will be optional.)
#'
#' Files cache to \code{tools::R_user_dir("brfssTools", "cache")} by
#' default. Subsequent runs are fast — already-downloaded files are
#' reused.
#'
#' @param years Integer vector of years to download. Defaults to
#'   \code{2018:current_year}.
#' @param data_path Optional override for where to store the downloaded
#'   files. If NULL (default), uses the package cache directory.
#' @param codebook_path Optional override; defaults to
#'   \code{<data_path>/documentation/}.
#' @param crosswalk Optional override for the crosswalk path; defaults
#'   to \code{<codebook_path>/National_crosswalk.csv}.
#' @param overwrite Logical. If TRUE, re-download files even if cached.
#' @return Invisibly, the resolved pool config.
#' @export
brfss_use_national <- function(years = NULL,
                               data_path = NULL,
                               codebook_path = NULL,
                               crosswalk = NULL,
                               overwrite = FALSE) {
  if (is.null(years)) {
    years <- 2018:as.integer(format(Sys.Date(), "%Y"))
  }

  if (is.null(data_path)) {
    data_path <- brfss_cache_dir()
  }
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
  }

  message(sprintf("Downloading %d year(s) of National BRFSS data...",
                   length(years)))
  brfss_download(year = years, dir = data_path, overwrite = overwrite)

  # brfss_download already registered the pool with default codebook_path
  # and crosswalk. If user passed overrides, re-register with them.
  if (!is.null(codebook_path) || !is.null(crosswalk)) {
    brfss_set_pool(
      dataset       = "National",
      data_path     = data_path,
      codebook_path = codebook_path,
      crosswalk     = crosswalk
    )
  }

  cfg <- .brfss_get_pool("National")

  # Helpful next-step message
  if (!file.exists(cfg$crosswalk)) {
    message("\nNo crosswalk file at: ", cfg$crosswalk)
    message("Run: brfss_draft_crosswalk(dataset = \"National\")")
    message("(A pre-built National crosswalk is planned for a future ",
             "release.)")
  }

  invisible(cfg)
}
