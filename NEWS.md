# brfssTools 0.0.0.9004

## Breaking changes

* `brfss_clean_race()` is rewritten for **MRACE-encoded race arrays**, the
  encoding actually used in OR's analyst-facing BRFSS files (and CDC
  submissions). The previous version was calibrated for raw REX REAL-D
  codes, which means it would have silently produced wrong answers on
  MRACE-encoded data (e.g., flagging Filipinos as Chinese, since REX 43
  = Chinese but MRACE 43 = Filipino). The new version:
  - Reads race from `race_array_*` columns interpreting MRACE codes
    (10/20/30/40/41-47/50/51-54/60).
  - Reads Hispanic origin from a separate `hisp_array_*` column family
    interpreting HISPANC3 codes (1-4 = Hispanic origin, 5 = No,
    7/9 = unknown).
  - Drops the `race_legacy_summary` fallback (no longer needed; legacy
    OR data fits the same MRACE model).
  - Drops REX-only `reald_*` indicators (Slavic, Eastern European,
    Afro-Caribbean, Ethiopian, Somali, Hmong, Marshallese, Middle
    Eastern, North African) — these aren't recoverable from MRACE
    encoding. The `reald` standard now produces only the Asian and PI
    subgroup indicators that MRACE preserves.
  - Drops the year-aware NA coercion of `reald_*` columns (no longer
    needed; MRACE codes are stable across years).

* `hisp_flag` concept is renamed to `hisp_array_1` to fit the array
  naming convention. The crosswalk migration handles this automatically.

## New features

* Crosswalk now ships with canonical `survey_weight`, `strata`,
  `age_continuous`, `hisp_array_1`–`hisp_array_4`, and
  `race_array_1`–`race_array_14` concepts, mapped to OR's per-year
  raw column conventions:
  - `survey_weight`: `wtrk_ABCD_abocd` (2012-2015), `wtrk_AC_aoc` (2016),
    `wtrk_all` (2017-2024), all `source = "OR"`.
  - `strata`: `STSTR` (2024 only, `source = "OR"`).
  - `hisp_array_*`: `HISPANC3_*` (2017-2021), `HISPANC3_0*` (2022-2024).
  - `race_array_*`: `RACE*` (2013-2016, `source = "core"`),
    `MRACE1_*` (2017-2021, `source = "OR"`),
    `MRACE2_0*` (2022-2024, `source = "OR"`).

# brfssTools 0.0.0.9003

## New features

* `brfss_clean_age()`: derive a standardized age grouping from
  whichever combination of age source columns is available
  (`age_continuous`, `age_5yr`, `age_6group`, `age_65plus`). Built-in
  schemes: `"5yr"` (13-level CDC), `"6group"` (CDC default), `"10yr"`,
  `"65plus"` binary, `"samhsa"` (NSDUH/behavioral-health), `"suicide"`
  (suicide-surveillance). Plus `"custom"` accepting user-supplied
  `breaks`/`labels`. Each row uses the best available source via
  waterfall; output is an ordered factor.

## Breaking changes

* `brfss_race()` is renamed to `brfss_clean_race()` for naming
  consistency with the new `brfss_clean_*` family. The old name remains
  as a deprecated alias that warns once and dispatches to the new
  function. Update calls at your convenience.

# brfssTools 0.0.0.9002

## New features

* `brfss_as_svy()`: convert a wide pull result into a `srvyr::tbl_svy`
  for design-aware estimation. Handles the OR no-strata case
  automatically (falls back to a single-stratum design when the
  `strata` column is missing or all-NA), and coerces the character-typed
  weight column back to numeric.

* `brfss_pull()` now pulls survey design variables by default. New
  `with_design = TRUE` argument silently auto-includes
  `survey_weight`, `psu`, and `strata` in the requested concepts —
  but only those that actually exist in the crosswalk, so the missing
  OR strata don't trigger a warning. Set `with_design = FALSE` to opt
  out.

* `brfss_pull()` defaults `id_cols` to `"SEQNO"`. Columns that don't
  exist in the raw file are silently skipped, so this is safe for files
  with non-standard ID conventions.

## Internal changes

* `srvyr` added to Suggests (gated by `rlang::check_installed()`).

# brfssTools 0.0.0.9001

## New features

* `vignette("brfssTools")`: comprehensive getting-started walkthrough.
  Covers the three-piece architecture (pool / crosswalk / pull), a
  self-contained runnable example built on synthetic data, the source-
  tag system that lets datasets coexist in one master crosswalk, the
  REAL-D race derivation, and the workflow for onboarding a new state.

* `brfss_datasets()`: lists every dataset the package knows about along
  with metadata (description, file format, whether
  [brfssTools::brfss_download()] supports it, whether `_STATE` filtering
  is meaningful). Single source of truth — adding a new dataset means
  editing one tibble in `R/datasets.R`, with no other code changes
  required.

* `brfss_pool_status()`: returns a tibble showing which raw data files
  are currently registered, by dataset and year. Useful in multi-pool
  sessions and as a diagnostic before pulls.

* `brfss_clear_pool(dataset = NULL)`: removes one or all registered
  pools. Mainly for testing and for switching folders mid-session.

* `brfss_inventory_pool(dataset)`: scans every file in a registered pool
  and returns a tibble in the shape of `raw_inventory_brfss.csv` —
  variable names, positions, types, and (for XPT files) embedded SAS
  labels. The primary onboarding tool when adding a new state dataset to
  the master crosswalk.

* `brfss_values_pool(dataset)`: scans every file in a registered pool
  and returns the unique observed values for each variable, with
  frequency counts, in the shape of `raw_values_brfss.csv`. Skips
  high-cardinality variables (defaults to `max_unique = 50`) on the
  assumption they're continuous or free-text.

## Internal changes

* `brfss_set_pool()` now warns when called with a dataset name not in
  the registry — a soft signal that the name might be a typo, while
  still allowing experimentation with new datasets ahead of formal
  registration.

* `brfss_pull()` and `brfss_crosswalk()` no longer special-case
  `"National"` as core-only. The applicable-source filter is now uniform
  across datasets: `source %in% c("core", dataset)`. National-only
  calculated variables (e.g., `_LLCPWT`) can be tagged `source =
  "National"` in the concept_map if desired.

* Refactor: `.read_pool_file()` and `.peek_pool_file()` extracted as
  shared file-reading helpers, used by both the pool fetch path and the
  inventory generators.

# brfssTools 0.0.0.9000

## Breaking changes

* The `survey` column in the bundled crosswalk CSVs is renamed to `source`,
  and its values now distinguish where a rule applies: `"core"` (universal)
  or a state code such as `"OR"` (state-added items only). A backwards-
  compat shim auto-renames `survey` to `source` and converts legacy
  `"BRFSS"` values to `"core"` on load, so legacy CSVs continue to work
  without manual migration.

* `brfss_set_pool()` now takes two arguments: `dataset` and `path`. Pools
  are registered per-dataset, replacing the single global option-stash.
  Pool state lives in a private package environment; nothing leaks into
  `options()`.

* `brfss_pull()` gains a required `dataset` argument and optional `years`
  and `states` arguments. It also gains a `dataset` column in its output
  (alongside the existing `source` column) so multi-dataset analyses are
  unambiguous. When `data` is passed in-memory, the registered pool is
  no longer used as a fallback for missing years.

* `brfss_crosswalk()` gains a `dataset` argument that filters the
  `concept_map` to applicable rows (`"core"` plus the named dataset's
  state-added items).

## New features

* `brfss_download(years)`: downloads the public combined landline + cell
  phone CDC LLCP files for the requested years (2011 onward), extracts the
  `LLCP{YYYY}.XPT` files into the user cache directory (`brfss_cache_dir()`),
  and registers the cache as the `"National"` pool. After this runs,
  `brfss_pull(..., dataset = "National")` works immediately.

* `brfss_cache_dir()`: returns the OS-appropriate user cache path used
  for national downloads.

* `states` argument on `brfss_pull()` accepts either FIPS codes (integer)
  or USPS abbreviations (character). Filters via the `_STATE` column;
  ignored when no `_STATE` column is present.

* `brfss_set_pool()` and `brfss_detect_year()` now handle SAS XPT files
  in addition to CSVs, dispatching on extension. XPT support requires the
  `haven` package (a Suggest, gated by `rlang::check_installed()`).

## Internal changes

* Tidyverse hygiene pass: `.data$`/`.env$` pronouns in dplyr verbs,
  `globalVariables()` for bare names inside `select()`, removed `library()`
  calls from package source.

* Source split: `brfss_set_pool()` and `brfss_detect_year()` moved to
  `R/pool.R`, `brfss_download()` to `R/download.R`, `brfss_pull()` to
  `R/pull.R`. `R/crosswalk.R` now contains only the loader, search,
  lookup, and `brfss_harmonize_vec()`.

* Test suite added under `tests/testthat/`.
