test_that(".coerce_source renames legacy `survey` to `source`", {
  legacy <- tibble::tibble(
    concept_id   = c("a", "b"),
    survey       = c("BRFSS", "BRFSS"),
    year         = c(2020L, 2021L),
    raw_var_name = c("X1", "X2")
  )
  out <- brfssTools:::.coerce_source(legacy)
  expect_true("source" %in% names(out))
  expect_false("survey" %in% names(out))
  expect_equal(out$source, c("core", "core"))
})

test_that(".coerce_source preserves non-BRFSS legacy values", {
  legacy <- tibble::tibble(
    concept_id = "x",
    survey     = "OR",
    year       = 2022L
  )
  out <- brfssTools:::.coerce_source(legacy)
  expect_equal(out$source, "OR")
})

test_that(".coerce_source is a no-op on already-migrated data", {
  current <- tibble::tibble(
    concept_id = "x",
    source     = "core",
    year       = 2022L
  )
  out <- brfssTools:::.coerce_source(current)
  expect_identical(current, out)
})

test_that(".coerce_source handles empty data frames", {
  empty <- tibble::tibble()
  expect_identical(brfssTools:::.coerce_source(empty), empty)
})
