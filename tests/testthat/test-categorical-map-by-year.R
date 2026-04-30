test_that("categorical_map: year-aware inputs route to per-year columns", {
  yaml_text <- '
output_column: sex
type: categorical_map

inputs:
  sx: SEX

by_year:
  "2018-2024":
    inputs:
      sx: SEXVAR

levels:
  - value: 1
    label: "Male"
    when: sx == 1
  - value: 2
    label: "Female"
    when: sx == 2
  - value: 9
    label: "Refused/DK"
    when: TRUE
'
  fp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, fp)
  on.exit(unlink(fp))

  spec <- brfssTools:::brfss_load_transformation_spec(fp)
  expect_equal(spec$type, "categorical_map")
  expect_equal(spec$output_column, "sex")

  # 2015 data has SEX, no SEXVAR
  d_old <- data.frame(SEX = c(1L, 2L, 9L), stringsAsFactors = FALSE)
  out <- brfssTools:::.brfss_apply_categorical_map(d_old, spec, year = 2015L)
  expect_equal(out$sex, c(1L, 2L, 9L))

  # 2020 data has SEXVAR, no SEX
  d_new <- data.frame(SEXVAR = c(1L, 2L, 9L), stringsAsFactors = FALSE)
  out <- brfssTools:::.brfss_apply_categorical_map(d_new, spec, year = 2020L)
  expect_equal(out$sex, c(1L, 2L, 9L))
})

test_that("categorical_map: year-aware levels override top-level levels", {
  # INCOME-style scenario: same column name (INCOME) but codes mean
  # different things in different years.
  yaml_text <- '
output_column: income_band
type: categorical_map

inputs:
  inc: INCOME

# Pre-2021: 4 codes (low/mid/high/refused). Post-2021: 5 codes.
levels:
  - value: 1
    label: "Low"
    when: inc == 1
  - value: 2
    label: "Mid"
    when: inc == 2
  - value: 3
    label: "High"
    when: inc == 3
  - value: 9
    label: "Refused"
    when: TRUE

by_year:
  "2021-2024":
    levels:
      - value: 1
        label: "Low"
        when: inc == 1
      - value: 2
        label: "LowMid"
        when: inc == 2
      - value: 3
        label: "MidHigh"
        when: inc == 3
      - value: 4
        label: "VeryHigh"
        when: inc == 4
      - value: 9
        label: "Refused"
        when: TRUE
'
  fp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, fp)
  on.exit(unlink(fp))

  spec <- brfssTools:::brfss_load_transformation_spec(fp)
  expect_true(!is.null(spec$by_year))
  expect_equal(length(spec$by_year[["2021-2024"]]$levels), 5L)

  # Pre-2021 data: 4 codes
  d_old <- data.frame(INCOME = c(1L, 2L, 3L, 4L), stringsAsFactors = FALSE)
  out_old <- brfssTools:::.brfss_apply_categorical_map(d_old, spec,
                                                       year = 2019L)
  expect_equal(out_old$income_band, c(1L, 2L, 3L, 9L))
  # Code 4 falls through to the catch-all (9) because top-level levels has
  # only 1/2/3 + refused.

  # Post-2021 data: 5 codes
  d_new <- data.frame(INCOME = c(1L, 2L, 3L, 4L, 5L), stringsAsFactors = FALSE)
  out_new <- brfssTools:::.brfss_apply_categorical_map(d_new, spec,
                                                       year = 2022L)
  expect_equal(out_new$income_band, c(1L, 2L, 3L, 4L, 9L))
  # Code 4 now maps to value 4 ("VeryHigh") via the by_year override.
})

test_that("categorical_map: by_year with no levels falls back to top-level", {
  # by_year overrides inputs only; top-level levels still apply.
  yaml_text <- '
output_column: out
type: categorical_map

inputs:
  v: COL_DEFAULT

by_year:
  "2022":
    inputs:
      v: COL_2022

levels:
  - value: 1
    when: v == 1
  - value: 9
    when: TRUE
'
  fp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, fp)
  on.exit(unlink(fp))

  spec <- brfssTools:::brfss_load_transformation_spec(fp)
  d_2022 <- data.frame(COL_2022 = c(1L, 2L, 1L), stringsAsFactors = FALSE)
  out <- brfssTools:::.brfss_apply_categorical_map(d_2022, spec, year = 2022L)
  expect_equal(out$out, c(1L, 9L, 1L))
})

test_that("categorical_map: validator rejects missing levels (none anywhere)", {
  yaml_text <- '
output_column: out
type: categorical_map

inputs:
  v: SOMETHING
'
  fp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, fp)
  on.exit(unlink(fp))

  expect_error(
    brfssTools:::brfss_load_transformation_spec(fp),
    "must define at least one level"
  )
})

test_that("categorical_map: validator accepts levels-only-in-by_year", {
  yaml_text <- '
output_column: out
type: categorical_map

inputs:
  v: SOMETHING

by_year:
  "2024":
    levels:
      - value: 1
        when: v == 1
      - value: 9
        when: TRUE
'
  fp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, fp)
  on.exit(unlink(fp))

  spec <- brfssTools:::brfss_load_transformation_spec(fp)
  expect_equal(spec$type, "categorical_map")

  # 2024 data works
  d <- data.frame(SOMETHING = c(1L, 2L), stringsAsFactors = FALSE)
  out <- brfssTools:::.brfss_apply_categorical_map(d, spec, year = 2024L)
  expect_equal(out$out, c(1L, 9L))

  # 2020 data: no levels apply, output is NA with a warning
  expect_warning(
    out <- brfssTools:::.brfss_apply_categorical_map(d, spec, year = 2020L),
    "no levels for year 2020"
  )
  expect_true(all(is.na(out$out)))
})
