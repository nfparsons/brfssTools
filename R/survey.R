# R/survey.R
#
# Survey-design integration. Converts the wide output of brfss_pull()
# into a srvyr `tbl_svy` for design-aware estimation.

#' Convert a wide BRFSS pull result into a survey design
#'
#' @description
#' Wraps a wide-format tibble produced by [brfss_pull()] in a
#' [srvyr::as_survey_design()] object, ready for design-aware estimation.
#'
#' Handles two real-world quirks:
#'
#' - **Strata may be absent.** Oregon BRFSS files do not carry a stratum
#'   variable for most years. If the `strata` column is missing entirely,
#'   or is present but contains only `NA`, this function falls back to a
#'   single-stratum design (no `strata` argument passed to srvyr).
#' - **Harmonized values are character.** The recode engine returns
#'   character vectors (because recode rules can map to non-numeric
#'   labels). For the design columns we coerce: `survey_weight` becomes
#'   numeric. `psu` and `strata` stay as-is — character/factor values
#'   are valid stratum and PSU identifiers.
#'
#' Rows with `NA` or non-positive weights are dropped, since survey
#' design objects require positive weights.
#'
#' @param data A wide tibble produced by [brfss_pull()] with
#'   `output = "wide"`. Must contain a numeric-coercible weight column.
#' @param weights Name of the weight column. Default `"survey_weight"`.
#' @param ids Name of the PSU / cluster column. Default `"psu"`. If the
#'   column is missing, the design uses `~1` (no clustering).
#' @param strata Name of the stratum column. Default `"strata"`. If the
#'   column is missing or all-NA, the design is built without
#'   stratification.
#' @param nest Passed through to [srvyr::as_survey_design()]. Default
#'   `TRUE`, the right choice when PSUs are nested within strata
#'   (BRFSS national). Forced to `FALSE` if strata are absent.
#' @return A `tbl_svy` object from the `srvyr` package.
#' @seealso [brfss_pull()] to produce the input.
#' @export
#' @examples
#' \dontrun{
#' library(srvyr)
#'
#' or_data <- brfss_pull(
#'   cw, c("ASTHNOW", "GENHLTH"),
#'   dataset = "OR", output = "wide"
#' )
#'
#' or_svy <- brfss_as_svy(or_data)
#'
#' or_svy |>
#'   group_by(GENHLTH) |>
#'   summarize(prev = survey_mean(ASTHNOW == "1", na.rm = TRUE,
#'                                vartype = "ci"))
#' }
brfss_as_svy <- function(data,
                         weights = "survey_weight",
                         ids     = "psu",
                         strata  = "strata",
                         nest    = TRUE) {

  rlang::check_installed("srvyr",
                         reason = "to build survey-design objects.")

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame (typically from brfss_pull()).",
         call. = FALSE)
  }
  if (!(weights %in% names(data))) {
    stop(sprintf(
      "Weight column '%s' not found. Did you run brfss_pull() with `with_design = TRUE` and `output = \"wide\"`?",
      weights
    ), call. = FALSE)
  }

  # Coerce weights to numeric. brfss_pull returns character because
  # the recode engine works on strings.
  wt_num <- suppressWarnings(as.numeric(data[[weights]]))
  bad <- is.na(wt_num) | wt_num <= 0
  if (all(bad)) {
    stop(sprintf("All values in `%s` are NA, non-numeric, or non-positive.",
                 weights), call. = FALSE)
  }
  if (any(bad)) {
    message(sprintf(
      "Dropping %d row(s) with missing or non-positive weights.",
      sum(bad)
    ))
  }
  data <- data[!bad, , drop = FALSE]
  data[[weights]] <- wt_num[!bad]

  # Decide stratification and clustering. srvyr accepts formula
  # arguments, which is the cleanest way to pass column names from
  # variables (no NSE games).
  use_strata <- !is.null(strata) &&
    strata %in% names(data) &&
    !all(is.na(data[[strata]]))
  use_ids <- !is.null(ids) && ids %in% names(data)

  weights_form <- stats::as.formula(paste0("~", weights))
  ids_form     <- if (use_ids) {
    stats::as.formula(paste0("~", ids))
  } else {
    stats::as.formula("~1")
  }

  if (use_strata) {
    strata_form <- stats::as.formula(paste0("~", strata))
    srvyr::as_survey_design(
      data,
      weights = weights_form,
      ids     = ids_form,
      strata  = strata_form,
      nest    = isTRUE(nest)
    )
  } else {
    srvyr::as_survey_design(
      data,
      weights = weights_form,
      ids     = ids_form
    )
  }
}
