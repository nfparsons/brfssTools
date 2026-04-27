test_that("brfss_pool_status returns empty tibble when no pools registered", {
  brfss_clear_pool()
  out <- brfss_pool_status()
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("dataset", "year", "file") %in% names(out)))
})

test_that("brfss_pool_status reports a registered pool", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_pool_status_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Two CSV files for different years
  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp, "BRFSS_2020.csv"))
  writeLines(c("SEQNO,X", "2021000001,1"),
             file.path(tmp, "BRFSS_2021.csv"))

  suppressMessages(brfss_set_pool("OR", tmp))

  out <- brfss_pool_status()
  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$year), c(2020L, 2021L))
  expect_true(all(out$dataset == "OR"))
})

test_that("brfss_pool_status sorts by dataset then year", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp_or <- file.path(tempdir(), "brfss_status_or")
  tmp_nat <- file.path(tempdir(), "brfss_status_nat")
  dir.create(tmp_or, showWarnings = FALSE)
  dir.create(tmp_nat, showWarnings = FALSE)
  on.exit({
    unlink(tmp_or, recursive = TRUE)
    unlink(tmp_nat, recursive = TRUE)
  }, add = TRUE)

  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp_or, "OR_2020.csv"))
  writeLines(c("SEQNO,X", "2021000001,1"),
             file.path(tmp_nat, "LLCP2021.csv"))

  suppressMessages(brfss_set_pool("OR", tmp_or))
  suppressMessages(brfss_set_pool("National", tmp_nat))

  out <- brfss_pool_status()
  # National sorts before OR alphabetically
  expect_equal(out$dataset, c("National", "OR"))
})

test_that("brfss_clear_pool with no arg clears all pools", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_clear_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp, "BRFSS_2020.csv"))

  suppressMessages(brfss_set_pool("OR", tmp))
  expect_equal(nrow(brfss_pool_status()), 1L)

  cleared <- brfss_clear_pool()
  expect_equal(cleared, "OR")
  expect_equal(nrow(brfss_pool_status()), 0L)
})

test_that("brfss_clear_pool with named dataset clears only that one", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp_or <- file.path(tempdir(), "brfss_clear_or")
  tmp_nat <- file.path(tempdir(), "brfss_clear_nat")
  dir.create(tmp_or, showWarnings = FALSE)
  dir.create(tmp_nat, showWarnings = FALSE)
  on.exit({
    unlink(tmp_or, recursive = TRUE)
    unlink(tmp_nat, recursive = TRUE)
  }, add = TRUE)

  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp_or, "OR_2020.csv"))
  writeLines(c("SEQNO,X", "2021000001,1"),
             file.path(tmp_nat, "LLCP2021.csv"))
  suppressMessages(brfss_set_pool("OR", tmp_or))
  suppressMessages(brfss_set_pool("National", tmp_nat))

  cleared <- brfss_clear_pool("OR")
  expect_equal(cleared, "OR")

  remaining <- brfss_pool_status()
  expect_equal(unique(remaining$dataset), "National")
})

test_that("brfss_set_pool warns on unknown dataset name", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_unknown_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp, "BRFSS_2020.csv"))

  expect_warning(
    suppressMessages(brfss_set_pool("Atlantis", tmp)),
    "not in the brfssTools registry"
  )
})
