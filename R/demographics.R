# R/demographics.R

#' Derive Standardized Race/Ethnicity from BRFSS Data
#'
#' @description
#' Operates on a wide tibble produced by [brfss_pull()] containing
#' `race_array_*` and (optionally) `hisp_array_*` columns. The function
#' interprets race-array values as **MRACE codes** — the BRFSS standard
#' collapsed encoding that ships in OR's analyst-facing files and CDC's
#' submitted records. MRACE codes are:
#'
#' - 10 White (also receives REX MENA codes 80-82 via CDC's collapse)
#' - 20 Black or African American
#' - 30 American Indian or Alaska Native
#' - 40 Asian umbrella, 41-47 Asian subgroups
#'   (41 Asian Indian, 42 Chinese, 43 Filipino, 44 Japanese,
#'    45 Korean, 46 Vietnamese, 47 Other Asian)
#' - 50 PI umbrella, 51-54 PI subgroups
#'   (51 Native Hawaiian, 52 Guamanian/Chamorro,
#'    53 Samoan, 54 Other Pacific Islander)
#' - 60 Other, 88 No additional choices, 99 Refused
#'
#' Hispanic-array values are interpreted as HISPANC3 codes
#' (1 Mexican, 2 Puerto Rican, 3 Cuban, 4 Other Hispanic, 5 No,
#' 7 DK, 9 Refused). Hispanic detection scans all `hisp_array_*` slots
#' for any code 1-4.
#'
#' @section Granularity caveat:
#' MRACE encoding is the maximum granularity available in standard CDC
#' BRFSS files. Categories collapsed by CDC's REX-to-MRACE derivation
#' (Slavic, Eastern European, Afro-Caribbean, Ethiopian, Somali, Hmong,
#' Marshallese, Middle Eastern, North African) are not separable here.
#' Recovering them requires the raw REX1-REX10 columns, which are not
#' typically distributed with analyst-facing files.
#'
#' @param df A wide dataframe produced by [brfss_pull()].
#' @param standard String: `"fewest"`, `"cdc"`, or `"reald"`.
#'   - `"fewest"` produces a single column `race_eth_fewest` with three
#'     levels: Hispanic/Latino, White Non-Hispanic, BIPOC/Global Majority.
#'   - `"cdc"` produces a single column `race_eth_cdc` with eight
#'     mutually-exclusive levels.
#'   - `"reald"` produces a set of `reald_*` binary indicator columns
#'     for each Asian and Pacific Islander subgroup separable in MRACE.
#'
#' @return The input dataframe with the requested column(s) appended.
#' @export
#' @examples
#' \dontrun{
#' demos <- brfss_pull(
#'   cw,
#'   c(paste0("race_array_", 1:14), paste0("hisp_array_", 1:4)),
#'   dataset = "OR", output = "wide"
#' )
#' demos |> brfss_clean_race(standard = "cdc")
#' demos |> brfss_clean_race(standard = "fewest")
#' demos |> brfss_clean_race(standard = "reald")
#' }
brfss_clean_race <- function(df, standard = c("fewest", "cdc", "reald")) {

  standard <- match.arg(standard)

  # Internal helper: scan all columns matching `prefix` for any of `codes`.
  # Compares as character to be tolerant of numeric/character mixing.
  # Always cbinds the per-column hit lists into a matrix so that rowSums
  # works correctly whether there's one column or many.
  scan_array <- function(data, prefix, codes) {
    cols <- names(data)[grepl(prefix, names(data))]
    if (length(cols) == 0) return(rep(FALSE, nrow(data)))

    hits <- lapply(data[cols], function(col) {
      as.character(col) %in% as.character(codes)
    })
    rowSums(do.call(cbind, hits), na.rm = TRUE) > 0
  }

  # ---- Hispanic detection (HISPANC3 codes 1-4 = Hispanic origin) ----
  is_hisp <- scan_array(df, "^hisp_array_", c(1L, 2L, 3L, 4L))

  # ---- Race detection (MRACE code system) ----
  has_race <- function(codes) scan_array(df, "^race_array_", codes)

  is_white <- has_race(10:14)
  is_black <- has_race(20:28)
  is_aian  <- has_race(30:38)
  is_asian <- has_race(40:47)
  is_nhpi  <- has_race(50:58)
  is_other <- has_race(c(60, 85))

  race_count <- is_white + is_black + is_aian + is_asian + is_nhpi + is_other

  # ----- Standard A: CDC 8-level mutually exclusive -----
  if (standard == "cdc") {
    df$race_eth_cdc <- dplyr::case_when(
      is_hisp                       ~ "Hispanic",
      race_count > 1                ~ "Multiracial Non-Hispanic",
      is_white & race_count == 1    ~ "White Only Non-Hispanic",
      is_black & race_count == 1    ~ "Black Only Non-Hispanic",
      is_aian  & race_count == 1    ~ "AI/AN Only Non-Hispanic",
      is_asian & race_count == 1    ~ "Asian Only Non-Hispanic",
      is_nhpi  & race_count == 1    ~ "NH/PI Only Non-Hispanic",
      is_other & race_count == 1    ~ "Other Non-Hispanic",
      TRUE                          ~ NA_character_
    )
    return(df)
  }

  # ----- Standard B: fewest categories -----
  if (standard == "fewest") {
    df$race_eth_fewest <- dplyr::case_when(
      is_hisp                      ~ "Hispanic/Latino",
      is_white & race_count == 1   ~ "White Non-Hispanic",
      race_count >= 1              ~ "BIPOC / Global Majority",
      TRUE                         ~ NA_character_
    )
    return(df)
  }

  # ----- Standard C: REAL-D-style detail (Asian and PI subgroups only) -----
  if (standard == "reald") {
    # Asian subgroups
    df$reald_asian_indian    <- has_race(41)
    df$reald_chinese         <- has_race(42)
    df$reald_filipino        <- has_race(43)
    df$reald_japanese        <- has_race(44)
    df$reald_korean          <- has_race(45)
    df$reald_vietnamese      <- has_race(46)
    df$reald_other_asian     <- has_race(47)
    # Pacific Islander subgroups
    df$reald_native_hawaiian <- has_race(51)
    df$reald_chamorro        <- has_race(52)
    df$reald_samoan          <- has_race(53)
    df$reald_other_pi        <- has_race(54)
    return(df)
  }
}

#' Derive Standardized Race/Ethnicity (deprecated)
#'
#' Use [brfss_clean_race()] instead. Kept as an alias for backwards
#' compatibility.
#'
#' @inheritParams brfss_clean_race
#' @return The dataframe with the requested demographic standard appended.
#' @export
#' @keywords internal
brfss_race <- function(df, standard = c("fewest", "cdc", "reald")) {
  .Deprecated("brfss_clean_race")
  brfss_clean_race(df, standard = standard)
}
