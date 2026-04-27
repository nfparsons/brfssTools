test_that("brfss_inventory_pool returns inventory schema for CSV pool", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_inv_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c(
    "SEQNO,ASTHNOW,GENHLTH,_AGEG5YR",
    "2020000001,1,2,5",
    "2020000002,2,1,3"
  ), file.path(tmp, "BRFSS_2020.csv"))

  suppressMessages(suppressWarnings(brfss_set_pool("OR", tmp)))

  inv <- brfss_inventory_pool("OR")

  expect_s3_class(inv, "tbl_df")
  expect_setequal(
    names(inv),
    c("source", "year", "raw_var_name", "position", "var_kind",
      "question_text", "module", "missing_values_raw", "source_file",
      "parse_notes")
  )
  expect_equal(nrow(inv), 4L)  # 4 columns in the file
  expect_equal(unique(inv$year), 2020L)
  expect_equal(unique(inv$source), "OR")
  expect_equal(inv$position, 1:4)
})

test_that("brfss_inventory_pool source override works", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_inv_source_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c("SEQNO,X", "2020000001,1"),
             file.path(tmp, "BRFSS_2020.csv"))
  suppressMessages(suppressWarnings(brfss_set_pool("OR", tmp)))

  inv <- brfss_inventory_pool("OR", source = "core")
  expect_equal(unique(inv$source), "core")
})

test_that("brfss_inventory_pool errors when pool not registered", {
  brfss_clear_pool()
  expect_error(
    brfss_inventory_pool("Nowhere"),
    "No pool registered"
  )
})

test_that("brfss_inventory_pool spans multiple years", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_inv_multi")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c("SEQNO,A,B", "2020000001,1,2"),
             file.path(tmp, "BRFSS_2020.csv"))
  writeLines(c("SEQNO,A,C", "2021000001,1,3"),  # var C, no B
             file.path(tmp, "BRFSS_2021.csv"))
  suppressMessages(suppressWarnings(brfss_set_pool("OR", tmp)))

  inv <- brfss_inventory_pool("OR")
  expect_equal(sort(unique(inv$year)), c(2020L, 2021L))
  # 2020 has SEQNO,A,B = 3; 2021 has SEQNO,A,C = 3
  expect_equal(nrow(inv), 6L)
})

test_that("brfss_values_pool returns frequency-counted values", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_vals_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c(
    "SEQNO,ASTHNOW",
    "2020000001,1",
    "2020000002,1",
    "2020000003,2",
    "2020000004,9"
  ), file.path(tmp, "BRFSS_2020.csv"))
  suppressMessages(suppressWarnings(brfss_set_pool("OR", tmp)))

  vals <- brfss_values_pool("OR", vars = "ASTHNOW")

  expect_s3_class(vals, "tbl_df")
  expect_setequal(
    names(vals),
    c("source", "year", "raw_var_name", "code", "label", "is_missing", "n")
  )
  expect_equal(unique(vals$raw_var_name), "ASTHNOW")
  # Three distinct codes
  expect_equal(nrow(vals), 3L)
  # Frequencies match
  by_code <- setNames(vals$n, vals$code)
  expect_equal(by_code[["1"]], 2L)
  expect_equal(by_code[["2"]], 1L)
  expect_equal(by_code[["9"]], 1L)
})

test_that("brfss_values_pool skips high-cardinality columns", {
  brfss_clear_pool()
  on.exit(brfss_clear_pool(), add = TRUE)

  tmp <- file.path(tempdir(), "brfss_vals_card")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # 60 unique values in the X column (continuous-like)
  writeLines(
    c("SEQNO,X", paste0("2020", sprintf("%06d", 1:60), ",", 1:60)),
    file.path(tmp, "BRFSS_2020.csv")
  )
  suppressMessages(suppressWarnings(brfss_set_pool("OR", tmp)))

  vals <- brfss_values_pool("OR", max_unique = 50L)
  # X has 60 unique values -> skipped. SEQNO has 60 too -> also skipped.
  expect_equal(nrow(vals), 0L)
})

test_that(".infer_var_kind classifies basic R types", {
  expect_equal(brfssTools:::.infer_var_kind(1:10),                 "numeric")
  expect_equal(brfssTools:::.infer_var_kind(c(1.1, 2.2)),          "numeric")
  expect_equal(brfssTools:::.infer_var_kind(letters),              "character")
  expect_equal(brfssTools:::.infer_var_kind(c(TRUE, FALSE)),       "logical")
  expect_equal(brfssTools:::.infer_var_kind(factor(c("a", "b"))),  "categorical")
})

test_that(".extract_label returns NA for unlabeled, label otherwise", {
  expect_true(is.na(brfssTools:::.extract_label(1:5)))

  x <- 1:5
  attr(x, "label") <- "Currently has asthma"
  expect_equal(brfssTools:::.extract_label(x), "Currently has asthma")
})
