# Tests for brfss_clean_age()
#
# Conventions in tests:
# - We pass character-typed source columns since brfss_pull() returns
#   character. The function coerces.
# - Each scheme/source pairing gets a focused round-trip test.

# ---------------------------------------------------------------------------
# scheme = "5yr" from each source
# ---------------------------------------------------------------------------

test_that("5yr from continuous covers boundary cases", {
  df <- tibble::tibble(age_continuous = c("18", "24", "25", "79", "80", "100", "17"))
  out <- brfss_clean_age(df, scheme = "5yr")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "18-24", "25-29", "75-79", "80+", "80+", NA_character_)
  )
})

test_that("5yr from age_5yr is a direct lookup, with 14 -> NA", {
  df <- tibble::tibble(age_5yr = c("1", "7", "13", "14", "99"))
  out <- brfss_clean_age(df, scheme = "5yr")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "50-54", "80+", NA_character_, NA_character_)
  )
})

test_that("5yr fails when only age_6group is available", {
  df <- tibble::tibble(age_6group = c("1", "2"))
  expect_error(
    brfss_clean_age(df, scheme = "5yr"),
    "Cannot derive scheme '5yr'"
  )
})

# ---------------------------------------------------------------------------
# scheme = "6group" from each source
# ---------------------------------------------------------------------------

test_that("6group from continuous", {
  df <- tibble::tibble(age_continuous = c("22", "30", "44", "55", "70"))
  out <- brfss_clean_age(df, scheme = "6group")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "25-34", "35-44", "55-64", "65+")
  )
})

test_that("6group from age_5yr collapses correctly", {
  # 1=18-24->"18-24", 3=30-34->"25-34", 4=35-39->"35-44",
  # 7=50-54->"45-54", 9=60-64->"55-64", 10=65-69->"65+"
  df <- tibble::tibble(age_5yr = c("1", "3", "4", "7", "9", "10"))
  out <- brfss_clean_age(df, scheme = "6group")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
})

test_that("6group from age_6group is a direct lookup", {
  df <- tibble::tibble(age_6group = c("1", "2", "3", "4", "5", "6"))
  out <- brfss_clean_age(df, scheme = "6group")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
})

# ---------------------------------------------------------------------------
# scheme = "65plus"
# ---------------------------------------------------------------------------

test_that("65plus from each source", {
  df1 <- tibble::tibble(age_continuous = c("64", "65", "30"))
  df2 <- tibble::tibble(age_5yr        = c("9", "10", "1"))   # 9=60-64, 10=65-69
  df3 <- tibble::tibble(age_6group     = c("5", "6", "1"))    # 5=55-64, 6=65+
  df4 <- tibble::tibble(age_65plus     = c("1", "2", "1"))

  expect_equal(as.character(brfss_clean_age(df1, scheme = "65plus")$age_grp),
               c("18-64", "65+", "18-64"))
  expect_equal(as.character(brfss_clean_age(df2, scheme = "65plus")$age_grp),
               c("18-64", "65+", "18-64"))
  expect_equal(as.character(brfss_clean_age(df3, scheme = "65plus")$age_grp),
               c("18-64", "65+", "18-64"))
  expect_equal(as.character(brfss_clean_age(df4, scheme = "65plus")$age_grp),
               c("18-64", "65+", "18-64"))
})

# ---------------------------------------------------------------------------
# scheme = "samhsa" requires continuous; errors otherwise
# ---------------------------------------------------------------------------

test_that("samhsa from continuous", {
  df <- tibble::tibble(age_continuous = c("18", "25", "26", "34", "35", "49", "50"))
  out <- brfss_clean_age(df, scheme = "samhsa")
  expect_equal(
    as.character(out$age_grp),
    c("18-25", "18-25", "26-34", "26-34", "35-49", "35-49", "50+")
  )
})

test_that("samhsa errors when only categorical sources are present", {
  df <- tibble::tibble(age_5yr = c("1", "2"))
  expect_error(
    brfss_clean_age(df, scheme = "samhsa"),
    "Cannot derive scheme 'samhsa'"
  )
})

# ---------------------------------------------------------------------------
# scheme = "suicide"
# ---------------------------------------------------------------------------

test_that("suicide from continuous", {
  df <- tibble::tibble(age_continuous = c("20", "30", "50", "70"))
  out <- brfss_clean_age(df, scheme = "suicide")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "25-44", "45-64", "65+")
  )
})

test_that("suicide from age_5yr", {
  df <- tibble::tibble(age_5yr = c("1", "2", "5", "6", "9", "10"))
  # 1=18-24->"18-24", 2=25-29->"25-44", 5=40-44->"25-44",
  # 6=45-49->"45-64", 9=60-64->"45-64", 10=65-69->"65+"
  out <- brfss_clean_age(df, scheme = "suicide")
  expect_equal(
    as.character(out$age_grp),
    c("18-24", "25-44", "25-44", "45-64", "45-64", "65+")
  )
})

# ---------------------------------------------------------------------------
# Custom scheme
# ---------------------------------------------------------------------------

test_that("custom scheme requires breaks AND labels", {
  df <- tibble::tibble(age_continuous = "30")
  expect_error(brfss_clean_age(df, scheme = "custom"),
               "requires both `breaks` and `labels`")
  expect_error(
    brfss_clean_age(df, scheme = "custom", breaks = c(18, 50, Inf)),
    "requires both `breaks` and `labels`"
  )
  expect_error(
    brfss_clean_age(df, scheme = "custom",
                    breaks = c(18, 30, 50, Inf),
                    labels = c("young", "old")),    # wrong length
    "labels.*length one less"
  )
})

test_that("custom scheme produces expected categories", {
  df <- tibble::tibble(age_continuous = c("20", "40", "60", "80"))
  out <- brfss_clean_age(
    df,
    scheme = "custom",
    breaks = c(18, 30, 50, 70, Inf),
    labels = c("18-29", "30-49", "50-69", "70+")
  )
  expect_equal(
    as.character(out$age_grp),
    c("18-29", "30-49", "50-69", "70+")
  )
  # Levels are preserved in the order given
  expect_equal(levels(out$age_grp),
               c("18-29", "30-49", "50-69", "70+"))
})

# ---------------------------------------------------------------------------
# Waterfall: best source wins
# ---------------------------------------------------------------------------

test_that("waterfall: continuous beats age_5yr when both present", {
  df <- tibble::tibble(
    age_continuous = c("25", NA,  "70"),
    age_5yr        = c("1",  "5", "13")    # 1=18-24, 5=40-44, 13=80+
  )
  out <- brfss_clean_age(df, scheme = "5yr")
  expect_equal(
    as.character(out$age_grp),
    c("25-29", "40-44", "70-74")    # row 2 fell through to age_5yr
  )
})

test_that("waterfall: 5yr falls through to 6group when 5yr is NA", {
  df <- tibble::tibble(
    age_5yr    = c("3", NA, NA),    # 3 = 30-34
    age_6group = c("4", "5", NA)    # 4 = 45-54, 5 = 55-64
  )
  out <- brfss_clean_age(df, scheme = "6group")
  expect_equal(
    as.character(out$age_grp),
    c("25-34", "55-64", NA_character_)
  )
})

# ---------------------------------------------------------------------------
# Output column behavior
# ---------------------------------------------------------------------------

test_that("output_col controls the name of the new column", {
  df <- tibble::tibble(age_continuous = c("22", "70"))
  out <- brfss_clean_age(df, scheme = "65plus", output_col = "is_senior")
  expect_true("is_senior" %in% names(out))
  expect_false("age_grp" %in% names(out))
})

test_that("calling twice with different output_col attaches both", {
  df <- tibble::tibble(age_continuous = c("22", "30", "70"))
  out <- df |>
    brfss_clean_age(scheme = "5yr",    output_col = "age_5yr_grp") |>
    brfss_clean_age(scheme = "65plus", output_col = "age_65")
  expect_true(all(c("age_5yr_grp", "age_65") %in% names(out)))
  expect_equal(as.character(out$age_5yr_grp), c("18-24", "30-34", "70-74"))
  expect_equal(as.character(out$age_65),       c("18-64", "18-64", "65+"))
})

# ---------------------------------------------------------------------------
# Output type: ordered factor
# ---------------------------------------------------------------------------

test_that("output is a factor with levels in scheme order", {
  df <- tibble::tibble(age_continuous = c("70", "22"))   # reversed input order
  out <- brfss_clean_age(df, scheme = "6group")
  expect_s3_class(out$age_grp, "factor")
  expect_equal(levels(out$age_grp),
               c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
})

test_that("error when no usable source columns are present", {
  df <- tibble::tibble(SEQNO = "x", GENHLTH = "1")
  expect_error(
    brfss_clean_age(df, scheme = "5yr"),
    "Cannot derive scheme '5yr'"
  )
})
