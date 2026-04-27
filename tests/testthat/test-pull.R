# Integration-style test that exercises brfss_pull() end-to-end without
# touching disk. We pass `data = list(...)` to bypass the pool fetch.

mk_cw <- function() {
  list(
    inventory = tibble::tibble(
      source = c("core", "core"),
      year = c(2020L, 2021L),
      raw_var_name = c("ASTHNOW", "ASTHNOW"),
      question_text = c("Do you currently have asthma?",
                        "Do you currently have asthma?"),
      position = NA_integer_, var_kind = NA_character_,
      module = NA_character_, missing_values_raw = NA_character_,
      source_file = NA_character_, parse_notes = NA_character_
    ),
    values = tibble::tibble(
      source = "core", year = 2020L, raw_var_name = "ASTHNOW",
      code = "9", label = "Refused", is_missing = TRUE
    ),
    concepts = tibble::tibble(
      concept_id = "asthma_current",
      concept_label = "Currently has asthma",
      domain = "respiratory", subdomain = NA_character_,
      construct_notes = NA_character_, canonical_response_type = "yes_no"
    ),
    concept_values = tibble::tibble(
      concept_id = character(), code = character(), label = character()
    ),
    concept_map = tibble::tibble(
      concept_id = c("asthma_current", "asthma_current"),
      source = c("core", "core"),
      year = c(2020L, 2021L),
      raw_var_name = c("ASTHNOW", "ASTHNOW"),
      question_text_drift = c("identical", "identical"),
      recode_type = c("inline", "inline"),
      recode_rule = c("1=1; 2=0", "1=1; 2=0"),
      notes = NA_character_
    ),
    lake = "brfss"
  )
}

# Variant of mk_cw with survey-design concepts mapped, used to exercise
# the with_design auto-include behavior.
mk_cw_with_design <- function() {
  cw <- mk_cw()
  design_rows <- tibble::tibble(
    concept_id = c("survey_weight", "psu",
                   "survey_weight", "psu"),
    source     = c("core", "core", "core", "core"),
    year       = c(2020L, 2020L, 2021L, 2021L),
    raw_var_name = c("_LLCPWT", "_PSU",
                     "_LLCPWT", "_PSU"),
    question_text_drift = "identical",
    recode_type = "identity",
    recode_rule = NA_character_,
    notes = NA_character_
  )
  cw$concept_map <- dplyr::bind_rows(cw$concept_map, design_rows)
  cw
}

test_that("brfss_pull returns wide tibble with state filtering on National", {
  cw <- mk_cw()
  data <- list(
    National_2020 = tibble::tibble(
      SEQNO = c("2020000001", "2020000002", "2020000003"),
      `_STATE` = c(41L, 53L, 6L),  # OR, WA, CA
      ASTHNOW = c("1", "2", "9")
    ),
    National_2021 = tibble::tibble(
      SEQNO = c("2021000001", "2021000002"),
      `_STATE` = c(41L, 53L),
      ASTHNOW = c("2", "1")
    )
  )

  out <- brfss_pull(
    cw, "asthma_current",
    dataset = "National",
    states  = "OR",
    data    = data,
    id_cols = "SEQNO",
    output  = "wide"
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("dataset", "year", "SEQNO", "asthma_current") %in% names(out)))
  expect_equal(nrow(out), 2L)  # one OR row per year
  expect_equal(out$asthma_current, c("1", "0"))  # 2020=1->Yes, 2021=2->No
})

test_that("brfss_pull respects years filter", {
  cw <- mk_cw()
  data <- list(
    OR_2020 = tibble::tibble(
      SEQNO = "2020000001", ASTHNOW = "1"
    ),
    OR_2021 = tibble::tibble(
      SEQNO = "2021000001", ASTHNOW = "2"
    )
  )

  out <- brfss_pull(
    cw, "asthma_current",
    dataset = "OR",
    years   = 2021,
    data    = data,
    id_cols = "SEQNO"
  )

  expect_equal(unique(out$year), 2021L)
})

test_that("brfss_pull returns long format by default", {
  cw <- mk_cw()
  data <- list(
    OR_2020 = tibble::tibble(SEQNO = "2020000001", ASTHNOW = "1")
  )
  out <- brfss_pull(cw, "asthma_current", dataset = "OR", data = data,
                   id_cols = "SEQNO")
  expect_true("harmonized_value" %in% names(out))
  expect_true("concept_id" %in% names(out))
})

test_that("brfss_pull errors when dataset arg is missing", {
  cw <- mk_cw()
  expect_error(
    brfss_pull(cw, "asthma_current"),
    "`dataset` is required"
  )
})

test_that("brfss_pull warns and returns empty tibble for unknown concept", {
  cw <- mk_cw()
  expect_warning(
    out <- brfss_pull(cw, "no_such_concept", dataset = "OR",
                     data = list(OR_2020 = tibble::tibble(SEQNO = "x"))),
    "No concept_map rows"
  )
  expect_equal(nrow(out), 0L)
})

test_that("brfss_pull does NOT fall back to pool when data arg is supplied", {
  # Regression: previously, a partial `data` list would silently invoke
  # .fetch_from_pool() for missing years, which is wrong. `data`, when
  # supplied, must be the sole source.
  cw <- mk_cw()  # concept_map covers BOTH 2020 and 2021
  data <- list(
    OR_2020 = tibble::tibble(SEQNO = "2020000001", ASTHNOW = "1")
    # OR_2021 deliberately omitted
  )
  # Should NOT error, even though no pool is registered for "OR".
  # The 2021 row should simply be skipped.
  out <- brfss_pull(cw, "asthma_current", dataset = "OR",
                   data = data, id_cols = "SEQNO")
  expect_equal(unique(out$year), 2020L)
  expect_equal(nrow(out), 1L)
})

test_that("brfss_pull defaults id_cols to SEQNO", {
  cw <- mk_cw()
  data <- list(
    OR_2020 = tibble::tibble(SEQNO = "2020000001", ASTHNOW = "1")
  )
  out <- brfss_pull(cw, "asthma_current", dataset = "OR", data = data)
  expect_true("SEQNO" %in% names(out))
  expect_equal(out$SEQNO, "2020000001")
})

test_that("brfss_pull silently skips id_cols that don't exist in the file", {
  cw <- mk_cw()
  data <- list(
    OR_2020 = tibble::tibble(ASTHNOW = "1")  # no SEQNO
  )
  out <- brfss_pull(cw, "asthma_current", dataset = "OR", data = data)
  expect_false("SEQNO" %in% names(out))
})

test_that("brfss_pull with_design auto-includes design concepts when present", {
  cw <- mk_cw_with_design()
  data <- list(
    OR_2020 = tibble::tibble(
      SEQNO    = "2020000001",
      ASTHNOW  = "1",
      `_LLCPWT` = "150.5",
      `_PSU`   = "12345"
    )
  )
  out <- brfss_pull(cw, "asthma_current", dataset = "OR",
                   data = data, output = "wide")
  expect_true(all(c("survey_weight", "psu", "asthma_current")
                  %in% names(out)))
  # strata not in cw -> not added, no warning
  expect_false("strata" %in% names(out))
})

test_that("brfss_pull with_design = FALSE skips design auto-include", {
  cw <- mk_cw_with_design()
  data <- list(
    OR_2020 = tibble::tibble(
      SEQNO    = "2020000001",
      ASTHNOW  = "1",
      `_LLCPWT` = "150.5",
      `_PSU`   = "12345"
    )
  )
  out <- brfss_pull(cw, "asthma_current", dataset = "OR",
                   data = data, output = "wide",
                   with_design = FALSE)
  expect_false("survey_weight" %in% names(out))
  expect_false("psu" %in% names(out))
  expect_true("asthma_current" %in% names(out))
})

test_that("brfss_pull with_design silently skips concepts not in crosswalk", {
  # mk_cw has none of the design concepts. with_design = TRUE shouldn't warn.
  cw <- mk_cw()
  data <- list(
    OR_2020 = tibble::tibble(SEQNO = "2020000001", ASTHNOW = "1")
  )
  expect_no_warning(
    out <- brfss_pull(cw, "asthma_current", dataset = "OR",
                     data = data, with_design = TRUE)
  )
  expect_true("SEQNO" %in% names(out))
})
