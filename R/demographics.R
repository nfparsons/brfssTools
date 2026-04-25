#' Derive Standardized Race/Ethnicity from BRFSS Data
#'
#' Handles the longitudinal shift from basic summary columns (pre-2022)
#' to the expansive REAL-D arrays (2022+).
#'
#' @param df A wide dataframe produced by `brfss_pull()`.
#' @param standard String: "fewest", "cdc", or "reald".
#' @return The dataframe appended with the requested demographic standard.
#' @export
brfss_race <- function(df, standard = c("fewest", "cdc", "reald")) {

  standard <- match.arg(standard)

  # Helper: Scan all race_array columns for specific REAL-D codes
  # Assumes concept map maps REX, WHITECAT, etc., to names starting with "race_array_"
  has_code <- function(data, codes) {
    cols <- names(data)[grepl("^race_array_", names(data))]
    if (length(cols) == 0) return(rep(FALSE, nrow(data)))

    rowSums(
      sapply(data[cols], function(col) as.character(col) %in% as.character(codes)),
      na.rm = TRUE
    ) > 0
  }

  # 1. ESTABLISH THE BASE REAL-D FLAGS (Using 2022-2024 codeblocks)
  is_hisp  <- has_code(df, c(1:9))          # Hispanic / Latino origins
  is_white <- has_code(df, c(10:19))        # White / Slavic / European
  is_black <- has_code(df, c(20:29))        # Black / African / Somali
  is_aian  <- has_code(df, c(30:39))        # AI/AN / Indigenous
  is_asian <- has_code(df, c(40:49, 61:63, 68)) # Asian origins
  is_nhpi  <- has_code(df, c(50:58))        # NH/PI origins
  is_mena  <- has_code(df, c(80:82))        # Middle Eastern / North African
  is_other <- has_code(df, c(60, 85))       # Something else

  # If we have legacy data (pre-2022), fall back to mapping the old summary columns
  # to the base flags to ensure backwards compatibility.
  if ("race_legacy_summary" %in% names(df)) {
    leg <- as.character(df$race_legacy_summary)
    is_hisp  <- is_hisp  | leg %in% c("66", "8", "9") # using historical Hispanic codes
    is_white <- is_white | leg %in% c("1", "10")
    is_black <- is_black | leg %in% c("2", "20")
    is_aian  <- is_aian  | leg %in% c("3", "30")
    is_asian <- is_asian | leg %in% c("4", "40")
    is_nhpi  <- is_nhpi  | leg %in% c("5", "50")
    is_other <- is_other | leg %in% c("6", "60")
  }

  race_count <- is_white + is_black + is_aian + is_asian + is_nhpi + is_mena + is_other

  # ---------------------------------------------------------
  # PATH A: CDC STANDARD (8-Level Mutually Exclusive)
  # ---------------------------------------------------------
  if (standard == "cdc") {
    df <- df |>
      dplyr::mutate(
        race_eth_cdc = dplyr::case_when(
          is_hisp ~ "Hispanic",
          race_count > 1 ~ "Multiracial Non-Hispanic",
          is_white & race_count == 1 ~ "White Only Non-Hispanic",
          is_black & race_count == 1 ~ "Black Only Non-Hispanic",
          is_aian & race_count == 1 ~ "AI/AN Only Non-Hispanic",
          is_asian & race_count == 1 ~ "Asian Only Non-Hispanic",
          is_nhpi & race_count == 1 ~ "NH/PI Only Non-Hispanic",
          (is_mena | is_other) & race_count == 1 ~ "Other Non-Hispanic",
          TRUE ~ NA_character_
        )
      )
    return(df)
  }

  # ---------------------------------------------------------
  # PATH B: FEWEST CATEGORIES
  # ---------------------------------------------------------
  if (standard == "fewest") {
    df <- df |>
      dplyr::mutate(
        race_eth_fewest = dplyr::case_when(
          is_hisp ~ "Hispanic/Latino",
          is_white & race_count == 1 ~ "White Non-Hispanic",
          # Collapse all non-white and multiracial into a single reporting bucket
          # (Adapt this based on your specific county reporting standards)
          race_count >= 1 ~ "BIPOC / Global Majority",
          TRUE ~ NA_character_
        )
      )
    return(df)
  }

  # ---------------------------------------------------------
  # PATH C: REAL-D GRANULARITY
  # ---------------------------------------------------------
  if (standard == "reald") {
    # Generate binary indicator columns for the lowest common denominator
    # Note: Pre-2022 rows will automatically resolve to FALSE/NA because
    # those specific code numbers (e.g., '11' for Slavic) won't exist in the data.

    df <- df |>
      dplyr::mutate(
        # Detailed White
        reald_slavic = has_code(df, "11"),
        reald_eastern_euro = has_code(df, "12"),

        # Detailed Black
        reald_afro_caribbean = has_code(df, "22"),
        reald_ethiopian = has_code(df, "23"),
        reald_somali = has_code(df, "24"),

        # Detailed Asian
        reald_asian_indian = has_code(df, "41"),
        reald_cambodian = has_code(df, "42"),
        reald_chinese = has_code(df, "43"),
        reald_filipino = has_code(df, "45"),
        reald_hmong = has_code(df, "46"),
        reald_japanese = has_code(df, "47"),
        reald_korean = has_code(df, "48"),
        reald_vietnamese = has_code(df, "63"),

        # Detailed NH/PI
        reald_chamorro = has_code(df, "52"),
        reald_marshallese = has_code(df, "54"),
        reald_samoan = has_code(df, "56"),

        # MENA
        reald_middle_eastern = has_code(df, "81"),
        reald_north_african = has_code(df, "82")
      )

    # Coerce FALSE to NA if the row is from a year prior to 2022
    # (Assuming you pulled 'year' in cw_pull)
    if ("year" %in% names(df)) {
      reald_cols <- names(df)[grepl("^reald_", names(df))]
      df[df$year < 2022, reald_cols] <- NA
    }

    return(df)
  }
}
