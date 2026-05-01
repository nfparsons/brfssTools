# R/crosswalk.R
#
# v0.1.0 brfss_search / brfss_lookup are deeply tied to the v0.1.0
# crosswalk schema (cdc_var, is_primary, etc.). Archived at
# inst/archive/R/crosswalk_v01.R; will be reimplemented for the
# v0.2.0 schema in a follow-up release.

#' Search the crosswalk by regex
#'
#' @description \strong{Not implemented in v0.2.0.} Reimplementation
#' for the new schema is planned for v0.2.x.
#'
#' For now, inspect the crosswalk directly:
#' \preformatted{
#'   cw <- cw_load(dataset = "OR")
#'   subset(cw$crosswalk, grepl("MENT", concept_id, ignore.case = TRUE))
#' }
#'
#' @param ... Reserved for future API; ignored.
#' @return Errors with a message about the deferred implementation.
#' @export
brfss_search <- function(...) {
  stop("brfss_search() is not implemented for v0.2.0 yet. ",
       "Inspect the crosswalk directly via cw_load() and standard ",
       "subset operations.", call. = FALSE)
}

#' Look up specific concept_ids in the crosswalk
#'
#' @description \strong{Not implemented in v0.2.0.} Reimplementation
#' for the new schema is planned for v0.2.x.
#'
#' @param ... Reserved for future API; ignored.
#' @return Errors with a message about the deferred implementation.
#' @export
brfss_lookup <- function(...) {
  stop("brfss_lookup() is not implemented for v0.2.0 yet. ",
       "Inspect the crosswalk directly via cw_load() and standard ",
       "subset operations.", call. = FALSE)
}
