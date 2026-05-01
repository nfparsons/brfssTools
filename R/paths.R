# R/paths.R
#
# Path helpers. v0.2.0 dropped the user-config-dir concept; the only
# helper remaining here is for locating the package's installed
# extdata, which holds shipped reference data (CDC codebook, taxonomy,
# etc.).

#' Locate the installed package's extdata directory
#' @keywords internal
.brfss_package_extdata <- function() {
  fp <- system.file("extdata", package = "brfssTools")
  if (!nzchar(fp) || !dir.exists(fp)) {
    # devtools::load_all path
    pkg_root <- tryCatch(find.package("brfssTools"),
                          error = function(e) NULL)
    if (!is.null(pkg_root)) {
      candidate <- file.path(pkg_root, "inst", "extdata")
      if (dir.exists(candidate)) return(candidate)
    }
    stop("Could not locate brfssTools/extdata. Reinstall the package.",
         call. = FALSE)
  }
  fp
}
