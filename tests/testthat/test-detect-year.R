test_that("brfss_detect_year parses year from LLCP filename", {
  tmp <- file.path(tempdir(), "LLCP2020.XPT")
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  # haven won't be needed because filename match wins before peeking
  expect_equal(brfss_detect_year(tmp), 2020L)
})

test_that("brfss_detect_year handles BRFSS_2018.csv pattern", {
  tmp <- file.path(tempdir(), "BRFSS_2018.csv")
  writeLines(c("SEQNO,X", "2018000001,1"), tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(brfss_detect_year(tmp), 2018L)
})

test_that("brfss_detect_year falls back to SEQNO peek when filename has no year", {
  tmp <- file.path(tempdir(), "oregon_data.csv")
  writeLines(c("SEQNO,X", "2019000123,1"), tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(brfss_detect_year(tmp), 2019L)
})

test_that("brfss_detect_year returns NULL for missing file", {
  expect_null(brfss_detect_year("/no/such/file.csv"))
})

test_that("brfss_detect_year ignores out-of-range numeric matches", {
  # 1999 in filename body shouldn't pass the 1984..2100 sanity check
  # (OK 1999 IS in range, so we use 1850 to test rejection)
  tmp <- file.path(tempdir(), "report_1850_special.csv")
  writeLines(c("SEQNO,X", "2020000001,1"), tmp)
  on.exit(unlink(tmp), add = TRUE)
  # Should fall back to SEQNO peek -> 2020
  expect_equal(brfss_detect_year(tmp), 2020L)
})
