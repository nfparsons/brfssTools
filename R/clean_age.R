# R/clean_age.R
#
# brfss_clean_age(): derive a single, consistent age grouping from
# whichever combination of age source columns is available.
#
# Source columns recognized (in waterfall priority order):
#   age_continuous - top-coded continuous age (best; derives any scheme)
#   age_5yr        - 13-level _AGEG5YR (1=18-24 ... 13=80+, 14=DK/Refused)
#   age_6group     - 6-level _AGE_G   (1=18-24 ... 6=65+)
#   age_65plus     - binary _AGE65YR  (1=18-64, 2=65+)
#
# Each row uses the BEST available source. If a row's higher-priority
# source is NA, the function falls through to the next available source.

#' Derive a standardized age grouping from BRFSS age variables
#'
#' @description
#' BRFSS exposes age in several drift-prone forms across years
#' (continuous top-coded, 13-level five-year groups, six-level groups,
#' and a 65+ binary). `brfss_clean_age()` picks from a menu of common
#' groupings and uses whichever source column is available for each
#' row, in a waterfall: continuous if present, else 5-year groups,
#' else 6-group, else 65+ binary.
#'
#' Operates on the wide tibble produced by [brfss_pull()]. Looks for
#' columns named `age_continuous`, `age_5yr`, `age_6group`, `age_65plus`
#' (the canonical concept IDs). At least one usable source must exist
#' for the requested scheme.
#'
#' @param df A wide tibble produced by [brfss_pull()].
#' @param scheme One of:
#'   - `"5yr"` — 13 categories: 18-24, 25-29, ..., 75-79, 80+.
#'   - `"6group"` — 6 categories: 18-24, 25-34, 35-44, 45-54, 55-64, 65+.
#'   - `"10yr"` — 7 categories: 18-24, 25-34, ..., 65-74, 75+.
#'   - `"65plus"` — binary: 18-64 / 65+.
#'   - `"samhsa"` — 18-25, 26-34, 35-49, 50+ (NSDUH/SAMHSA convention,
#'     requires continuous age).
#'   - `"suicide"` — 18-24, 25-44, 45-64, 65+ (suicide-surveillance
#'     convention).
#'   - `"custom"` — user-supplied `breaks` and `labels` (requires
#'     continuous age).
#' @param breaks For `scheme = "custom"` only. Numeric vector of break
#'   points passed to [base::cut()] (left-closed, right-open intervals).
#' @param labels For `scheme = "custom"` only. Character vector of
#'   category labels, length `length(breaks) - 1`.
#' @param output_col Name of the column to add to `df`. Default
#'   `"age_grp"`. Call multiple times with different `output_col` values
#'   to attach more than one grouping to the same dataset.
#' @return The input tibble with one new factor column whose levels are
#'   ordered according to the requested scheme.
#' @export
#' @examples
#' \dontrun{
#' demos <- brfss_pull(cw, c("age_continuous", "age_5yr"),
#'                     dataset = "OR", output = "wide")
#'
#' demos |> brfss_clean_age(scheme = "5yr")
#' demos |> brfss_clean_age(scheme = "65plus", output_col = "is_65plus")
#'
#' # Custom breaks for an analysis-specific cut
#' demos |> brfss_clean_age(
#'   scheme = "custom",
#'   breaks = c(18, 30, 50, 70, Inf),
#'   labels = c("18-29", "30-49", "50-69", "70+")
#' )
#' }
brfss_clean_age <- function(df,
                            scheme = c("5yr", "6group", "10yr", "65plus",
                                       "samhsa", "suicide", "custom"),
                            breaks = NULL,
                            labels = NULL,
                            output_col = "age_grp") {
  scheme <- match.arg(scheme)

  if (scheme == "custom") {
    if (is.null(breaks) || is.null(labels)) {
      stop("`scheme = \"custom\"` requires both `breaks` and `labels`.",
           call. = FALSE)
    }
    if (length(labels) != length(breaks) - 1L) {
      stop("`labels` must have length one less than `breaks`.",
           call. = FALSE)
    }
  }

  # Which source columns can produce this scheme?
  scheme_compat <- list(
    "5yr"     = c("age_continuous", "age_5yr"),
    "6group"  = c("age_continuous", "age_5yr", "age_6group"),
    "10yr"    = c("age_continuous", "age_5yr"),
    "65plus"  = c("age_continuous", "age_5yr", "age_6group", "age_65plus"),
    "samhsa"  = c("age_continuous"),
    "suicide" = c("age_continuous", "age_5yr", "age_6group"),
    "custom"  = c("age_continuous")
  )[[scheme]]

  available <- intersect(scheme_compat, names(df))

  if (length(available) == 0L) {
    found <- intersect(
      c("age_continuous", "age_5yr", "age_6group", "age_65plus"),
      names(df)
    )
    stop(sprintf(
      "Cannot derive scheme '%s'. Need one of: %s. Found: %s.",
      scheme,
      paste(scheme_compat, collapse = ", "),
      if (length(found)) paste(found, collapse = ", ") else "(none)"
    ), call. = FALSE)
  }

  # Apply waterfall: each source fills in only the rows still NA.
  result <- rep(NA_character_, nrow(df))
  for (src in available) {
    vec <- df[[src]]
    fill <- is.na(result) & !is.na(vec)
    if (!any(fill)) next
    result[fill] <- .map_age_source(vec[fill], src, scheme,
                                    breaks = breaks, labels = labels)
  }

  df[[output_col]] <- factor(result, levels = .scheme_levels(scheme, labels))
  df
}

# ----------------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------------

# Ordered category levels for each scheme.
.scheme_levels <- function(scheme, labels = NULL) {
  switch(
    scheme,
    "5yr"     = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
    "6group"  = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    "10yr"    = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
    "65plus"  = c("18-64", "65+"),
    "samhsa"  = c("18-25", "26-34", "35-49", "50+"),
    "suicide" = c("18-24", "25-44", "45-64", "65+"),
    "custom"  = labels
  )
}

# Map a vector of source values to scheme labels. Dispatch on source.
.map_age_source <- function(vec, src, scheme, breaks = NULL, labels = NULL) {
  switch(
    src,
    "age_continuous" = .cut_continuous(
      suppressWarnings(as.numeric(vec)), scheme, breaks, labels
    ),
    "age_5yr" = {
      code <- suppressWarnings(as.integer(vec))
      code[code < 1L | code > 13L] <- NA_integer_   # 14 = DK/Refused
      .collapse_5yr(code, scheme)
    },
    "age_6group" = {
      code <- suppressWarnings(as.integer(vec))
      code[code < 1L | code > 6L] <- NA_integer_
      .collapse_6group(code, scheme)
    },
    "age_65plus" = {
      code <- suppressWarnings(as.integer(vec))
      code[code < 1L | code > 2L] <- NA_integer_
      .collapse_65plus(code, scheme)
    },
    stop(sprintf("Unknown age source: %s", src), call. = FALSE)
  )
}

# Cut continuous age into the requested scheme.
# Intervals are left-closed, right-open: 18 inclusive to 25 exclusive.
.cut_continuous <- function(age, scheme, breaks = NULL, labels = NULL) {
  cut_args <- function(b, l) {
    as.character(cut(age, breaks = b, labels = l, right = FALSE))
  }
  switch(
    scheme,
    "5yr" = cut_args(
      c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
      c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    ),
    "6group" = cut_args(
      c(18, 25, 35, 45, 55, 65, Inf),
      c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ),
    "10yr" = cut_args(
      c(18, 25, 35, 45, 55, 65, 75, Inf),
      c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ),
    "65plus" = cut_args(
      c(18, 65, Inf),
      c("18-64", "65+")
    ),
    "samhsa" = cut_args(
      c(18, 26, 35, 50, Inf),
      c("18-25", "26-34", "35-49", "50+")
    ),
    "suicide" = cut_args(
      c(18, 25, 45, 65, Inf),
      c("18-24", "25-44", "45-64", "65+")
    ),
    "custom" = as.character(
      cut(age, breaks = breaks, labels = labels,
          right = FALSE, include.lowest = TRUE)
    )
  )
}

# Collapse 13-level _AGEG5YR codes to the requested scheme.
.collapse_5yr <- function(code, scheme) {
  lookup <- switch(
    scheme,
    "5yr"     = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
    "6group"  = c("18-24", "25-34", "25-34", "35-44", "35-44", "45-54", "45-54",
                  "55-64", "55-64", "65+",   "65+",   "65+",   "65+"),
    "10yr"    = c("18-24", "25-34", "25-34", "35-44", "35-44", "45-54", "45-54",
                  "55-64", "55-64", "65-74", "65-74", "75+",   "75+"),
    "65plus"  = c("18-64", "18-64", "18-64", "18-64", "18-64", "18-64", "18-64",
                  "18-64", "18-64", "65+",   "65+",   "65+",   "65+"),
    "suicide" = c("18-24", "25-44", "25-44", "25-44", "25-44", "45-64", "45-64",
                  "45-64", "45-64", "65+",   "65+",   "65+",   "65+"),
    stop(sprintf("Cannot collapse age_5yr to scheme '%s'.", scheme),
         call. = FALSE)
  )
  lookup[code]
}

# Collapse 6-level _AGE_G codes to the requested scheme.
.collapse_6group <- function(code, scheme) {
  lookup <- switch(
    scheme,
    "6group"  = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    "65plus"  = c("18-64", "18-64", "18-64", "18-64", "18-64", "65+"),
    "suicide" = c("18-24", "25-44", "25-44", "45-64", "45-64", "65+"),
    stop(sprintf("Cannot collapse age_6group to scheme '%s'.", scheme),
         call. = FALSE)
  )
  lookup[code]
}

# Collapse binary _AGE65YR codes.
.collapse_65plus <- function(code, scheme) {
  if (scheme != "65plus") {
    stop(sprintf("Cannot derive scheme '%s' from age_65plus alone.", scheme),
         call. = FALSE)
  }
  c("18-64", "65+")[code]
}
