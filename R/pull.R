# R/pull.R
#
# brfss_pull(): the workhorse. Pulls one or more concepts from a registered
# pool, applying the historical recode rules and producing a long or wide
# tibble.

# ----------------------------------------------------------------------------
# Internal: FIPS <-> USPS state crosswalk
# ----------------------------------------------------------------------------
# BRFSS files use FIPS in `_STATE`. Includes the 50 states + DC + the three
# territories that appear in public LLCP files (GU, PR, VI).
.brfss_state_fips <- function() {
  tibble::tribble(
    ~fips, ~usps, ~name,
    1L,   "AL",  "Alabama",
    2L,   "AK",  "Alaska",
    4L,   "AZ",  "Arizona",
    5L,   "AR",  "Arkansas",
    6L,   "CA",  "California",
    8L,   "CO",  "Colorado",
    9L,   "CT",  "Connecticut",
    10L,  "DE",  "Delaware",
    11L,  "DC",  "District of Columbia",
    12L,  "FL",  "Florida",
    13L,  "GA",  "Georgia",
    15L,  "HI",  "Hawaii",
    16L,  "ID",  "Idaho",
    17L,  "IL",  "Illinois",
    18L,  "IN",  "Indiana",
    19L,  "IA",  "Iowa",
    20L,  "KS",  "Kansas",
    21L,  "KY",  "Kentucky",
    22L,  "LA",  "Louisiana",
    23L,  "ME",  "Maine",
    24L,  "MD",  "Maryland",
    25L,  "MA",  "Massachusetts",
    26L,  "MI",  "Michigan",
    27L,  "MN",  "Minnesota",
    28L,  "MS",  "Mississippi",
    29L,  "MO",  "Missouri",
    30L,  "MT",  "Montana",
    31L,  "NE",  "Nebraska",
    32L,  "NV",  "Nevada",
    33L,  "NH",  "New Hampshire",
    34L,  "NJ",  "New Jersey",
    35L,  "NM",  "New Mexico",
    36L,  "NY",  "New York",
    37L,  "NC",  "North Carolina",
    38L,  "ND",  "North Dakota",
    39L,  "OH",  "Ohio",
    40L,  "OK",  "Oklahoma",
    41L,  "OR",  "Oregon",
    42L,  "PA",  "Pennsylvania",
    44L,  "RI",  "Rhode Island",
    45L,  "SC",  "South Carolina",
    46L,  "SD",  "South Dakota",
    47L,  "TN",  "Tennessee",
    48L,  "TX",  "Texas",
    49L,  "UT",  "Utah",
    50L,  "VT",  "Vermont",
    51L,  "VA",  "Virginia",
    53L,  "WA",  "Washington",
    54L,  "WV",  "West Virginia",
    55L,  "WI",  "Wisconsin",
    56L,  "WY",  "Wyoming",
    66L,  "GU",  "Guam",
    72L,  "PR",  "Puerto Rico",
    78L,  "VI",  "U.S. Virgin Islands"
  )
}

# Internal: coerce a `states` argument (FIPS ints OR USPS strings) to FIPS ints.
.resolve_states <- function(states) {
  if (is.null(states)) return(NULL)
  xref <- .brfss_state_fips()

  if (is.numeric(states)) {
    fips <- as.integer(states)
    bad <- setdiff(fips, xref$fips)
    if (length(bad)) {
      stop(sprintf("Unknown FIPS code(s): %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    return(fips)
  }

  if (is.character(states)) {
    up <- toupper(states)
    fips <- xref$fips[match(up, xref$usps)]
    bad <- up[is.na(fips)]
    if (length(bad)) {
      stop(sprintf("Unknown USPS code(s): %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    return(as.integer(fips))
  }

  stop("`states` must be a numeric (FIPS) or character (USPS) vector.",
       call. = FALSE)
}

# Internal: filter a yearly data frame to a set of FIPS state codes.
# `_STATE` is the canonical column; we accept several casings.
.filter_states <- function(df, fips) {
  if (is.null(fips)) return(df)
  cand <- c("_STATE", "X_STATE", "_state", "x_state")
  hit <- intersect(cand, names(df))
  if (length(hit) == 0L) {
    warning(
      "Requested state filter, but no `_STATE` column was found in the file. ",
      "Returning all rows.",
      call. = FALSE
    )
    return(df)
  }
  state_col <- df[[hit[1]]]
  keep <- as.integer(state_col) %in% fips
  df[keep, , drop = FALSE]
}

# ----------------------------------------------------------------------------
# brfss_pull()
# ----------------------------------------------------------------------------

#' Pull one or more concepts from a registered dataset
#'
#' @description
#' The workhorse. Looks up the requested concepts in the master crosswalk,
#' fetches the matching yearly files from the dataset's pool, applies the
#' recode rules, and returns a tidy tibble.
#'
#' For `dataset = "National"`, only rules tagged `source == "core"` are
#' applied. For state datasets like `dataset = "OR"`, both `"core"` and the
#' state's own rules (`"OR"`) are applied — so state-added items show up
#' alongside the LLCP core.
#'
#' @param cw A loaded crosswalk (from [brfss_crosswalk()]).
#' @param concept_ids Character vector of concept IDs to pull.
#' @param dataset Single string. The dataset to pull from
#'   (e.g., `"National"`, `"OR"`). Must match a registered pool — see
#'   [brfssTools::brfss_set_pool()] or [brfssTools::brfss_download()].
#' @param years Optional integer vector restricting which years are pulled.
#'   Defaults to all years available in both the crosswalk and the pool.
#' @param states Optional vector of FIPS codes (integer) or USPS abbreviations
#'   (character) to filter the data to. Most useful when
#'   `dataset = "National"`. Ignored if no `_STATE` column exists.
#' @param data Optional named list of in-memory data frames keyed by
#'   `"<dataset>_<year>"` (e.g., `"OR_2022"`). When supplied, this list is
#'   the sole source of data - the registered pool is not consulted, even
#'   for years not present in the list (those years are simply skipped).
#'   Primarily useful for testing.
#' @param id_cols Character vector of column names to preserve from the raw
#'   file (e.g., `"SEQNO"`).
#' @param harmonize If `TRUE` (default), apply recode rules.
#' @param keep_raw If `TRUE`, keep the raw value alongside the harmonized one.
#' @param output `"long"` (default) or `"wide"`.
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' brfss_set_pool("OR", "Z:/secure/oregon_brfss")
#' cw <- brfss_crosswalk(dataset = "OR", years = 2018:2023)
#' brfss_pull(cw, c("survey_weight", "fmd"),
#'            dataset = "OR",
#'            years   = 2018:2023,
#'            id_cols = "SEQNO",
#'            output  = "wide")
#'
#' # National example
#' brfss_download(2022:2023)
#' cw_us <- brfss_crosswalk(dataset = "National")
#' brfss_pull(cw_us, c("survey_weight", "fmd"),
#'            dataset = "National",
#'            states  = c("OR", "WA", "ID"))
#' }
brfss_pull <- function(cw,
                       concept_ids,
                       dataset,
                       years = NULL,
                       states = NULL,
                       data = NULL,
                       id_cols = character(),
                       harmonize = TRUE,
                       keep_raw = FALSE,
                       output = c("long", "wide")) {

  output <- match.arg(output)

  if (missing(dataset) || !is.character(dataset) || length(dataset) != 1L) {
    stop("`dataset` is required and must be a single string ",
         "(e.g., 'National' or 'OR').", call. = FALSE)
  }

  fips <- .resolve_states(states)

  # Decide which `source` values are applicable for this dataset.
  # core    -> applies to every dataset
  # <ds>    -> applies only when pulling that dataset
  # The rule is uniform: include "core" plus the dataset's own source tag.
  # (National-only calculated variables, if any, can be tagged
  # `source = "National"` in the concept_map.)
  applicable_sources <- unique(c("core", dataset))

  cm <- cw$concept_map |>
    dplyr::filter(
      .data$concept_id %in% .env$concept_ids,
      .data$source %in% .env$applicable_sources
    )

  if (!is.null(years)) {
    cm <- dplyr::filter(cm, .data$year %in% .env$years)
  }

  if (nrow(cm) == 0L) {
    warning("No concept_map rows match the requested concepts/dataset/years.",
            call. = FALSE)
    return(tibble::tibble())
  }

  # Pre-compute, per (source, year, raw_var_name), the set of "missing" raw
  # codes so we can NA them out.
  miss_by_var <- cw$values |>
    dplyr::filter(.data$is_missing) |>
    dplyr::group_by(.data$source, .data$year, .data$raw_var_name) |>
    dplyr::summarize(missing_codes = list(unique(.data$code)), .groups = "drop")

  # We pre-fetch each (year) file once and reuse it across concepts.
  unique_years <- sort(unique(cm$year))
  file_cache <- list()

  for (yr in unique_years) {
    key <- sprintf("%s_%d", dataset, yr)

    # When `data` is supplied, it is the SOLE source - we never fall back
    # to the pool. A NULL value (or missing key) for a given year just
    # means "skip that year".
    df <- if (is.null(data)) {
      .fetch_from_pool(dataset, yr)
    } else {
      data[[key]]
    }

    if (is.null(df)) next
    df <- .filter_states(df, fips)
    file_cache[[as.character(yr)]] <- df
  }

  out_parts <- list()

  for (i in seq_len(nrow(cm))) {
    row <- cm[i, ]
    df  <- file_cache[[as.character(row$year)]]
    if (is.null(df)) next
    if (!(row$raw_var_name %in% names(df))) next

    raw_vec <- df[[row$raw_var_name]]
    n <- length(raw_vec)

    part <- tibble::tibble(
      concept_id   = rep(row$concept_id,   n),
      dataset      = rep(dataset,          n),
      source       = rep(row$source,       n),
      year         = rep(row$year,         n),
      raw_var_name = rep(row$raw_var_name, n)
    )
    for (col in id_cols) {
      if (col %in% names(df)) {
        part[[col]] <- df[[col]]
      }
    }

    if (keep_raw || !harmonize) part$raw_value <- as.character(raw_vec)

    if (harmonize) {
      mc <- miss_by_var |>
        dplyr::filter(
          .data$source       == row$source,
          .data$year         == row$year,
          .data$raw_var_name == row$raw_var_name
        ) |>
        dplyr::pull(.data$missing_codes)
      mc <- if (length(mc) == 0L) character() else mc[[1]]

      part$harmonized_value <- brfss_harmonize_vec(
        raw_vec,
        recode_type      = row$recode_type,
        recode_rule      = row$recode_rule,
        is_missing_codes = mc
      )
    }
    out_parts[[length(out_parts) + 1L]] <- part
  }

  long_df <- dplyr::bind_rows(out_parts)

  if (output == "wide" && nrow(long_df) > 0L) {
    val_cols <- if (keep_raw && harmonize) {
      c("harmonized_value", "raw_value")
    } else if (harmonize) {
      "harmonized_value"
    } else {
      "raw_value"
    }

    wide_df <- long_df |>
      dplyr::select(dataset, year, dplyr::all_of(id_cols),
                    concept_id, dplyr::all_of(val_cols)) |>
      tidyr::pivot_wider(
        names_from  = "concept_id",
        values_from = dplyr::all_of(val_cols)
      )

    if (length(val_cols) == 1L) {
      names(wide_df) <- gsub(paste0("^", val_cols, "_"), "", names(wide_df))
    }
    return(wide_df)
  }

  long_df
}
