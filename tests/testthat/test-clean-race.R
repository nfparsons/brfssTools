# Tests for brfss_clean_race() and the deprecated brfss_race() alias.
#
# All tests use MRACE codes (10/20/30/40/41-47/50/51-54/60) for race_array_*
# and HISPANC3 codes (1-5/7/9) for hisp_array_*.

# ---------------------------------------------------------------------------
# Hispanic detection (from hisp_array_*)
# ---------------------------------------------------------------------------

test_that("Hispanic flag fires when any hisp_array slot has codes 1-4", {
  df <- tibble::tibble(
    race_array_1 = c("10", "10", "10", "10", "10"),
    hisp_array_1 = c("1",  "5",  "5",  "7",  NA),
    hisp_array_2 = c(NA,   "2",  NA,   NA,   NA),
    hisp_array_3 = c(NA,   NA,   NA,   NA,   NA),
    hisp_array_4 = c(NA,   NA,   NA,   NA,   NA)
  )
  out <- brfss_clean_race(df, standard = "fewest")
  expect_equal(
    out$race_eth_fewest,
    c("Hispanic/Latino",      # slot 1 = Mexican
      "Hispanic/Latino",      # slot 2 = Puerto Rican; slot 1 = No
      "White Non-Hispanic",   # all slots = No
      "White Non-Hispanic",   # slot 1 = DK -> not Hispanic; race wins
      "White Non-Hispanic")   # all NA -> not Hispanic
  )
})

test_that("Hispanic detection works with no hisp_array columns at all", {
  # When hisp_array is absent, is_hisp is all FALSE and classification
  # falls back to race only.
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "41")
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_equal(
    out$race_eth_cdc,
    c("White Only Non-Hispanic",
      "Black Only Non-Hispanic",
      "Asian Only Non-Hispanic")
  )
})

# ---------------------------------------------------------------------------
# CDC standard
# ---------------------------------------------------------------------------

test_that("CDC standard uses MRACE codes correctly", {
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "30", "40", "41", "50", "51", "60"),
    hisp_array_1 = NA_character_
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_equal(
    out$race_eth_cdc,
    c("White Only Non-Hispanic",
      "Black Only Non-Hispanic",
      "AI/AN Only Non-Hispanic",
      "Asian Only Non-Hispanic",     # 40 = Asian umbrella
      "Asian Only Non-Hispanic",     # 41 = Asian Indian (still Asian)
      "NH/PI Only Non-Hispanic",     # 50 = PI umbrella
      "NH/PI Only Non-Hispanic",     # 51 = Native Hawaiian
      "Other Non-Hispanic")
  )
})

test_that("CDC standard distinguishes single-race from multiracial NH", {
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "10"),
    race_array_2 = c(NA,    NA,   "20"),
    race_array_3 = c(NA,    NA,   NA)
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_equal(out$race_eth_cdc,
               c("White Only Non-Hispanic",
                 "Black Only Non-Hispanic",
                 "Multiracial Non-Hispanic"))
})

test_that("CDC standard tags Hispanic regardless of race codes", {
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "10"),
    race_array_2 = c("20", NA,    NA),
    hisp_array_1 = c("1",  "1",   "5")     # Mexican, Mexican, No
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_equal(out$race_eth_cdc,
               c("Hispanic", "Hispanic", "White Only Non-Hispanic"))
})

# ---------------------------------------------------------------------------
# Fewest-categories standard
# ---------------------------------------------------------------------------

test_that("fewest collapses non-white non-Hispanic to BIPOC bucket", {
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "41", "10"),
    hisp_array_1 = c(NA,   NA,   NA,   "1")   # last row is Hispanic
  )
  out <- brfss_clean_race(df, standard = "fewest")
  expect_equal(out$race_eth_fewest,
               c("White Non-Hispanic",
                 "BIPOC / Global Majority",
                 "BIPOC / Global Majority",
                 "Hispanic/Latino"))
})

# ---------------------------------------------------------------------------
# REAL-D standard (Asian and PI subgroups only — MRACE limit)
# ---------------------------------------------------------------------------

test_that("REAL-D fires correct Asian subgroup indicators per MRACE codes", {
  df <- tibble::tibble(
    race_array_1 = c("41", "42", "43", "44", "45", "46", "47"),
    race_array_2 = NA_character_
  )
  out <- brfss_clean_race(df, standard = "reald")
  expect_true(out$reald_asian_indian[1])
  expect_true(out$reald_chinese[2])
  expect_true(out$reald_filipino[3])
  expect_true(out$reald_japanese[4])
  expect_true(out$reald_korean[5])
  expect_true(out$reald_vietnamese[6])
  expect_true(out$reald_other_asian[7])
  # Cross-checks: chinese row should NOT be flagged as filipino, etc.
  expect_false(out$reald_filipino[2])
  expect_false(out$reald_chinese[3])
})

test_that("REAL-D fires correct Pacific Islander subgroups per MRACE codes", {
  df <- tibble::tibble(
    race_array_1 = c("51", "52", "53", "54"),
    race_array_2 = NA_character_
  )
  out <- brfss_clean_race(df, standard = "reald")
  expect_true(out$reald_native_hawaiian[1])
  expect_true(out$reald_chamorro[2])
  expect_true(out$reald_samoan[3])
  expect_true(out$reald_other_pi[4])
})

# ---------------------------------------------------------------------------
# Robustness: single-column race array (regression test for sapply quirk)
# ---------------------------------------------------------------------------

test_that("single race_array column works (rowSums needs matrix input)", {
  # Earlier sapply-based approach broke with a single column because
  # sapply collapsed to a vector. The current implementation uses
  # do.call(cbind, ...) to guarantee a matrix.
  df <- tibble::tibble(
    race_array_1 = c("10", "20", "41")
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_equal(
    out$race_eth_cdc,
    c("White Only Non-Hispanic",
      "Black Only Non-Hispanic",
      "Asian Only Non-Hispanic")
  )
})

# ---------------------------------------------------------------------------
# Edge case: all missing race info
# ---------------------------------------------------------------------------

test_that("all-NA race_array yields NA classification", {
  df <- tibble::tibble(
    race_array_1 = NA_character_,
    race_array_2 = NA_character_,
    hisp_array_1 = NA_character_
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_true(is.na(out$race_eth_cdc))
})

test_that("MRACE refusal/missing codes (77/88/99) do not match any race", {
  # Code 88 ("no additional choices") and 99 ("refused") should not
  # trigger any race indicator; respondents end up NA.
  df <- tibble::tibble(
    race_array_1 = c("88", "99", "77"),
    hisp_array_1 = NA_character_
  )
  out <- brfss_clean_race(df, standard = "cdc")
  expect_true(all(is.na(out$race_eth_cdc)))
})

# ---------------------------------------------------------------------------
# Deprecation alias
# ---------------------------------------------------------------------------

test_that("brfss_race() warns and dispatches to brfss_clean_race()", {
  df <- tibble::tibble(
    race_array_1 = "10",
    race_array_2 = NA_character_
  )
  expect_warning(
    out <- brfss_race(df, standard = "fewest"),
    "deprecated"
  )
  expect_equal(out$race_eth_fewest, "White Non-Hispanic")
})
