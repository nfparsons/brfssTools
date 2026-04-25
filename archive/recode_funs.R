# R/recode_funs.R
#
# Named recode functions referenced by concept_map.csv rows where
# `recode_type == "function"`.
#
# CONTRACT
# --------
# Each function must:
#   1. Accept a single character vector `x` (raw codes, already normalized
#      and with missing codes set to NA by cw_harmonize_vec).
#   2. Return a character vector of the same length, with harmonized values.
#
# Keep these functions pure — no side effects, no global state, no package
# calls other than base R unless you're sure the package is loaded.
#
# HOW TO ADD A FUNCTION
# ---------------------
#   1. Define it below with a comment explaining the mapping it performs.
#   2. In concept_map.csv, set `recode_type` = "function" and `recode_rule`
#      = "<your_function_name>".
#   3. Source this file before calling cw_pull() or cw_harmonize_vec().
#
# Note: source("R/crosswalk.R") does NOT source this file automatically.
# You must source both.

# --------------------------------------------------------------------------
# recode_ordinal_count_to_binary
#
# Collapses an ordinal count scale to a binary "any / none" indicator.
# Used for `suicide_attempt_any_past_year` in OHT 2011 through SHS 2020,
# where the raw variable (suiatt12) has codes:
#   1 = 0 times
#   2 = 1 time
#   3 = 2 or 3 times
#   4 = 4 or 5 times
#   5 = 6 or more times
# and the harmonized concept wants:
#   0 = none  (raw code 1)
#   1 = any   (raw codes 2 through 5)
#
# Inputs already normalized by cw_harmonize_vec so codes are plain digit
# strings ("1" through "5"); NA is preserved.
# --------------------------------------------------------------------------
recode_ordinal_count_to_binary <- function(x) {
  xn <- suppressWarnings(as.numeric(x))
  out <- dplyr::case_when(
    is.na(xn)   ~ NA_character_,
    xn == 1     ~ "0",       # zero times
    xn >= 2     ~ "1",       # one or more times
    TRUE        ~ NA_character_
  )
  out
}
