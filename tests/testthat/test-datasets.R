test_that("brfss_datasets returns a tibble with required columns", {
  out <- brfss_datasets()
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("dataset", "description", "format",
                    "download_supported", "state_filter_supported")
                  %in% names(out)))
  expect_true(nrow(out) >= 2L)
})

test_that("brfss_datasets includes National and OR", {
  ds <- brfss_datasets()$dataset
  expect_true("National" %in% ds)
  expect_true("OR" %in% ds)
})

test_that(".validate_dataset is silent for known datasets", {
  expect_silent(brfssTools:::.validate_dataset("National"))
  expect_silent(brfssTools:::.validate_dataset("OR"))
})

test_that(".validate_dataset warns on unknown datasets", {
  expect_warning(
    brfssTools:::.validate_dataset("Atlantis"),
    "not in the brfssTools registry"
  )
})
