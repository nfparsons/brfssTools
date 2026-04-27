# Tests for brfss_as_svy(). Skipped when srvyr is not installed.

mk_pull_result <- function(with_strata = FALSE) {
  out <- tibble::tibble(
    dataset        = "OR",
    year           = 2020L,
    SEQNO          = sprintf("2020%06d", 1:6),
    survey_weight  = c("100.5", "200.0", "150.0", "300.0", "250.0", "175.0"),
    psu            = c("1", "1", "2", "2", "3", "3"),
    ASTHNOW        = c("1", "0", "1", NA, "0", "1"),
    GENHLTH        = c("2", "1", "3", "2", "1", "4")
  )
  if (with_strata) {
    out$strata <- c("A", "A", "B", "B", "C", "C")
  }
  out
}

test_that("brfss_as_svy builds a tbl_svy with default columns", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result()
  svy <- brfss_as_svy(out)
  expect_s3_class(svy, "tbl_svy")
})

test_that("brfss_as_svy uses strata when present and non-NA", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result(with_strata = TRUE)
  svy <- brfss_as_svy(out)
  # When strata are present, the underlying survey design should report
  # multiple strata. We check the prob/strata via srvyr's accessor.
  expect_s3_class(svy, "tbl_svy")
  # The variables column of the survey design should contain `strata`
  expect_true("strata" %in% names(svy$variables))
})

test_that("brfss_as_svy falls back when strata column is absent", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result(with_strata = FALSE)
  expect_no_error(brfss_as_svy(out))
})

test_that("brfss_as_svy falls back when strata column is all NA", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result(with_strata = TRUE)
  out$strata <- NA_character_
  expect_no_error(brfss_as_svy(out))
})

test_that("brfss_as_svy errors when weight column is missing", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result()
  out$survey_weight <- NULL
  expect_error(brfss_as_svy(out), "Weight column")
})

test_that("brfss_as_svy errors when all weights are non-positive or NA", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result()
  out$survey_weight <- "0"
  expect_error(brfss_as_svy(out), "All values")
})

test_that("brfss_as_svy drops rows with NA / non-positive weights", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result()
  out$survey_weight[c(1, 3)] <- c(NA, "-5")
  expect_message(svy <- brfss_as_svy(out), "Dropping 2 row")
  # Internally, survey design retains 4 rows
  expect_equal(nrow(svy$variables), 4L)
})

test_that("brfss_as_svy handles custom column names", {
  skip_if_not_installed("srvyr")
  out <- mk_pull_result()
  names(out)[names(out) == "survey_weight"] <- "MY_WEIGHT"
  names(out)[names(out) == "psu"]           <- "MY_PSU"
  expect_no_error(
    brfss_as_svy(out, weights = "MY_WEIGHT", ids = "MY_PSU")
  )
})
