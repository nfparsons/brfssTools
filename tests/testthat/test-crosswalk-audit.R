# Tests for brfss_crosswalk_audit().
#
# Strategy: register a tiny CSV-backed "National" pool with known
# variables, build a minimal mock crosswalk with a few concept_map rows,
# and check that the audit correctly classifies each row as
# in/out of reference.

# ---- Shared fixture builder ----
mk_fixture <- function() {
  brfss_clear_pool()

  tmp <- file.path(tempdir(), "brfss_audit_fixture")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  dir.create(tmp, showWarnings = FALSE)

  # National 2020 file: has GENHLTH, _LLCPWT, _PSU, _STATE
  writeLines(c(
    "SEQNO,GENHLTH,_LLCPWT,_PSU,_STATE",
    "2020000001,1,1.5,1,41",
    "2020000002,2,2.0,1,41"
  ), file.path(tmp, "LLCP2020.csv"))

  # National 2021 file: GENHLTH only (not _LLCPWT — used to test year-strict)
  writeLines(c(
    "SEQNO,GENHLTH,_STATE",
    "2021000001,3,41"
  ), file.path(tmp, "LLCP2021.csv"))

  suppressMessages(suppressWarnings(
    brfss_set_pool("National", tmp)
  ))

  list(tmp = tmp)
}

# ---- Build a mock crosswalk object ----
mk_cw <- function(rows) {
  list(
    concept_map = tibble::as_tibble(rows),
    inventory = NULL, values = NULL, concepts = NULL,
    concept_values = NULL, lake = NULL
  )
}

# ---------------------------------------------------------------------------
# Strict year matching (default)
# ---------------------------------------------------------------------------

test_that("strict year match flags rows whose (year, var) miss in reference", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,    ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health",   "core",  2020L, "GENHLTH",     "identical",          "identity",   NA, NA,   # in 2020 ref
    "gen_health",   "core",  2021L, "GENHLTH",     "identical",          "identity",   NA, NA,   # in 2021 ref
    "survey_weight","core",  2020L, "_LLCPWT",     "identical",          "identity",   NA, NA,   # in 2020 ref
    "survey_weight","core",  2021L, "_LLCPWT",     "identical",          "identity",   NA, NA,   # NOT in 2021 ref
    "or_weight",    "core",  2020L, "wtrk_all",    "identical",          "identity",   NA, NA    # not in any year
  ))

  audit <- brfss_crosswalk_audit(cw, reference_dataset = "National")

  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 5L)
  expect_equal(audit$in_reference, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(audit$needs_action, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(audit$suggested_source,
               c("core", "core", "core", "OR", "OR"))
})

test_that("reference_years_seen lists every year a var appears in reference", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health", "core",  2020L, "GENHLTH",     "identical",          "identity",   NA, NA,
    "or_weight",  "core",  2020L, "wtrk_all",    "identical",          "identity",   NA, NA
  ))
  audit <- brfss_crosswalk_audit(cw, reference_dataset = "National")

  # GENHLTH is in both 2020 and 2021 fixture files
  expect_equal(audit$reference_years_seen[1], "2020,2021")
  # wtrk_all is in neither
  expect_true(is.na(audit$reference_years_seen[2]))
})

# ---------------------------------------------------------------------------
# Relaxed (any-year) matching
# ---------------------------------------------------------------------------

test_that("relaxed match treats var as in_reference if seen in any year", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  # _LLCPWT is in 2020 but not 2021; relaxed match should still flag both
  # rows as in_reference
  cw <- mk_cw(tibble::tribble(
    ~concept_id,    ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "survey_weight","core",  2020L, "_LLCPWT",     "identical",          "identity",   NA, NA,
    "survey_weight","core",  2021L, "_LLCPWT",     "identical",          "identity",   NA, NA
  ))

  strict  <- brfss_crosswalk_audit(cw, reference_dataset = "National",
                                   strict_year_match = TRUE)
  relaxed <- brfss_crosswalk_audit(cw, reference_dataset = "National",
                                   strict_year_match = FALSE)

  expect_equal(strict$in_reference,  c(TRUE, FALSE))   # _LLCPWT not in 2021
  expect_equal(relaxed$in_reference, c(TRUE, TRUE))    # exists in some year
})

# ---------------------------------------------------------------------------
# Case sensitivity
# ---------------------------------------------------------------------------

test_that("ignore_case = TRUE matches across upper/lower case", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health", "core",  2020L, "genhlth",     "identical",          "identity",   NA, NA  # lower-cased
  ))

  strict_default  <- brfss_crosswalk_audit(cw, reference_dataset = "National",
                                           ignore_case = FALSE)
  strict_relaxed  <- brfss_crosswalk_audit(cw, reference_dataset = "National",
                                           ignore_case = TRUE)

  expect_false(strict_default$in_reference[1])
  expect_true(strict_relaxed$in_reference[1])
})

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("non-core source rows are excluded by default", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health", "core",  2020L, "GENHLTH",     "identical",          "identity",   NA, NA,
    "or_weight",  "OR",    2020L, "wtrk_all",    "identical",          "identity",   NA, NA   # already OR
  ))
  audit <- brfss_crosswalk_audit(cw, reference_dataset = "National")
  expect_equal(nrow(audit), 1L)
  expect_equal(audit$concept_id, "gen_health")
})

test_that("custom current_source filters audit input correctly", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health", "core",  2020L, "GENHLTH",     "identical",          "identity",   NA, NA,
    "or_weight",  "OR",    2020L, "wtrk_all",    "identical",          "identity",   NA, NA
  ))
  audit_or <- brfss_crosswalk_audit(cw, reference_dataset = "National",
                                    current_source = "OR",
                                    suggested_source = "core")
  expect_equal(nrow(audit_or), 1L)
  expect_equal(audit_or$concept_id, "or_weight")
  expect_false(audit_or$in_reference)
})

test_that("audit returns empty tibble when no rows match current_source", {
  fx <- mk_fixture()
  on.exit({ brfss_clear_pool(); unlink(fx$tmp, recursive = TRUE) }, add = TRUE)

  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "or_weight",  "OR",    2020L, "wtrk_all",    "identical",          "identity",   NA, NA
  ))

  expect_message(
    brfss_crosswalk_audit(cw, reference_dataset = "National"),
    "nothing to audit"
  )
  audit <- suppressMessages(
    brfss_crosswalk_audit(cw, reference_dataset = "National")
  )
  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 0L)
})

test_that("audit errors informatively when reference pool is not registered", {
  brfss_clear_pool()
  cw <- mk_cw(tibble::tribble(
    ~concept_id,  ~source, ~year, ~raw_var_name, ~question_text_drift, ~recode_type, ~recode_rule, ~notes,
    "gen_health", "core",  2020L, "GENHLTH",     "identical",          "identity",   NA, NA
  ))
  expect_error(
    brfss_crosswalk_audit(cw, reference_dataset = "National"),
    "registered|pool"
  )
})

test_that("audit errors when cw is not a crosswalk object", {
  expect_error(
    brfss_crosswalk_audit(list(foo = "bar")),
    "crosswalk"
  )
})
