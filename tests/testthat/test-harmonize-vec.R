test_that("brfss_harmonize_vec identity passes through with NA for missing", {
  out <- brfss_harmonize_vec(
    x = c("1", "2", "9"),
    recode_type = "identity",
    is_missing_codes = "9"
  )
  expect_equal(out, c("1", "2", NA_character_))
})

test_that("brfss_harmonize_vec inline applies a Yes/No-style mapping", {
  out <- brfss_harmonize_vec(
    x = c("1", "2", "7", "9"),
    recode_type = "inline",
    recode_rule = "1=1; 2=0; 7=NA; 9=NA"
  )
  expect_equal(out, c("1", "0", NA_character_, NA_character_))
})

test_that("brfss_harmonize_vec inline normalizes numeric-looking codes", {
  # "01" and "1" should both match a key of "1"
  out <- brfss_harmonize_vec(
    x = c("01", "1", "2"),
    recode_type = "inline",
    recode_rule = "1=Yes; 2=No"
  )
  expect_equal(out, c("Yes", "Yes", "No"))
})

test_that("brfss_harmonize_vec errors on unknown recode_type", {
  expect_error(
    brfss_harmonize_vec("1", recode_type = "magic"),
    "Unknown recode_type"
  )
})

test_that("brfss_harmonize_vec function dispatch finds exported recode helpers", {
  # Stub a recode function in the global env so .get_recode_fn() can find it
  assign("test_double_it",
         function(x) as.character(as.numeric(x) * 2),
         envir = globalenv())
  on.exit(rm("test_double_it", envir = globalenv()), add = TRUE)

  out <- brfss_harmonize_vec(
    x = c("1", "2", "3"),
    recode_type = "function",
    recode_rule = "test_double_it"
  )
  expect_equal(out, c("2", "4", "6"))
})
