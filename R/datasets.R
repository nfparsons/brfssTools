# R/datasets.R
#
# Registry of datasets known to brfssTools. This is the single source of
# truth for what `dataset = "..."` values are valid, and what each one
# means. To add support for a new state (or any new BRFSS-derived
# dataset), edit `.brfss_dataset_registry()` below — no other code needs
# to change.

# Internal: the registry. Returned as a tibble for ergonomic filtering.
.brfss_dataset_registry <- function() {
  tibble::tribble(
    ~dataset,    ~description,                                                ~format, ~download_supported, ~state_filter_supported,
    "National",  "Public CDC combined landline + cell phone LLCP file",      "xpt",   TRUE,                TRUE,
    "OR",        "Oregon BRFSS (restricted; bring your own files)",          "csv",   FALSE,               FALSE
    # Add new datasets here, e.g.:
    # "WA",      "Washington State BRFSS (restricted; BYOD)",                 "csv",   FALSE,               FALSE,
    # "CA",      "California BRFSS (restricted; BYOD)",                       "csv",   FALSE,               FALSE
  )
}

#' List datasets known to brfssTools
#'
#' Returns a tibble describing every dataset the package knows about,
#' along with metadata indicating which features are available
#' (`brfss_download()` support, state-level filtering, etc.). The
#' `dataset` column lists the canonical identifiers that
#' `brfss_pull(dataset = ...)`, `brfss_set_pool(dataset, ...)`, and
#' `brfss_crosswalk(dataset = ...)` accept.
#'
#' To add a new dataset to the registry, edit
#' `.brfss_dataset_registry()` in `R/datasets.R`. No other code changes
#' are required — `brfss_pull()` and friends pick up new datasets
#' automatically.
#'
#' @return A tibble with columns `dataset`, `description`, `format`,
#'   `download_supported`, and `state_filter_supported`. The current
#'   pool-registration status is added by [brfss_pool_status()].
#' @export
#' @examples
#' brfss_datasets()
brfss_datasets <- function() {
  .brfss_dataset_registry()
}

# Internal: validate a dataset name against the registry.
# Warns rather than errors so users can experiment with unregistered
# pool names (e.g., during development of a new state's support).
.validate_dataset <- function(dataset) {
  known <- .brfss_dataset_registry()$dataset
  if (!(dataset %in% known)) {
    warning(sprintf(
      paste0(
        "Dataset '%s' is not in the brfssTools registry. ",
        "Known datasets: %s. Proceeding anyway, but consider adding ",
        "'%s' to .brfss_dataset_registry() in R/datasets.R if this is ",
        "a long-term addition."
      ),
      dataset, paste(known, collapse = ", "), dataset
    ), call. = FALSE)
  }
  invisible(NULL)
}
