# R/pull.R
#
# brfss_pull() is being rewritten for the v0.2.0 path-based architecture.
# The previous implementation is archived at inst/archive/R/pull_v01.R.

#' Pull harmonized data
#'
#' @description
#' \strong{Not implemented in v0.2.0.} The crosswalk-building workflow
#' is the focus of this release; loading harmonized data using the
#' v0.2.0 schema is planned for v0.2.x.
#'
#' To inspect your crosswalk programmatically, use [cw_load()] /
#' [cw_save()] and the \code{cw_*} CRUD helpers.
#'
#' @param ... Reserved for future API; ignored.
#' @return Errors with a message about the deferred implementation.
#' @export
brfss_pull <- function(...) {
  stop(
    "brfss_pull() is not implemented for the v0.2.0 path-based ",
    "architecture yet. The crosswalk-building workflow ",
    "(brfss_set_pool, brfss_draft_crosswalk, brfss_crosswalk_editor) ",
    "is the focus of v0.2.0. Pulling harmonized data using the new ",
    "schema is planned for v0.2.x.",
    call. = FALSE
  )
}
