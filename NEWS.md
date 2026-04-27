# brfssTools 0.0.0.9009

## Breaking changes

* `brfss_state_tag_audit()` renamed to `brfss_crosswalk_audit()`. The
  old name suggested the function only audits state tags, but it
  audits the crosswalk against a reference pool's variable inventory
  more generally. The new name is the family name; future audits of
  other crosswalk aspects (value labels, drift annotations) can sit
  alongside it. File `R/state_tag_audit.R` renamed to
  `R/crosswalk_audit.R`; test file `test-state-tag-audit.R` renamed
  to `test-crosswalk-audit.R`. Function arguments and behavior are
  unchanged.

# brfssTools 0.0.0.9008

## Breaking changes

* The `years` parameter is renamed to `year` in `brfss_crosswalk()`,
  `brfss_pull()`, and `brfss_download()`. This matches the tidycensus
  convention (a single argument that accepts either a scalar year or
  a vector of years) and eliminates the partial-match warning that
  fired when callers wrote `year = ...` against the old `years`
  signature. Update calling code:
  - `brfss_crosswalk(dataset = "OR", years = 2018:2023)` →
    `brfss_crosswalk(dataset = "OR", year = 2018:2023)`
  - `brfss_pull(cw, ..., years = 2021)` → `brfss_pull(cw, ..., year = 2021)`
  - `brfss_download(years = 2020:2023)` →
    `brfss_download(year = 2020:2023)` (or just `brfss_download(2020:2023)`,
    since `year` is the first positional argument)

# brfssTools 0.0.0.9007

## Bug fixes

* `brfss_as_svy()`: switch from `dplyr::all_of()` to the `!!sym()`
  injection pattern to inject column names. The `all_of()` approach
  triggered a tidyselect 1.2 deprecation warning and worked
  inconsistently across srvyr's `srvyr_select_vars()` paths — the
  `ids` path tolerated it (with the warning) while the `strata` path
  failed downstream in `survey::make.formula()` with an empty-formula
  error (`<text>:2:0: unexpected end of input`). The `!!rlang::sym()`
  approach substitutes column names at parse time, before srvyr's NSE
  captures the arguments, so srvyr only ever sees a clean bare-name
  reference. Implementation uses `rlang::inject()` to splice a
  dynamically-built argument list, so absent `ids` / `strata` args are
  *omitted* from the call rather than passed as `NULL` (avoiding the
  same empty-formula bug in another form).

# brfssTools 0.0.0.9006

## Bug fixes

* `brfss_as_svy()` now works with `srvyr` / `tidyselect` 1.1+. The
  previous implementation passed formula objects to
  `srvyr::as_survey_design()`, which `tidyselect` no longer accepts.
  The function now uses `dplyr::all_of()` to inject column names from
  string variables, the canonical tidyselect-friendly pattern. The
  no-clustering case passes `ids = NULL` (which srvyr correctly
  interprets as "no PSU") rather than a bare integer.

* Roxygen `[fn()]` cross-references in `R/demographics.R` and
  `R/pull.R` are now fully qualified as `[brfssTools::fn()]` to satisfy
  link resolution in newer roxygen2.

## Internal changes

* Migration scripts in `inst/migrate/` now wrap their logic in named
  functions (`migrate_01_canonical_naming()`, etc.) and have no
  top-level side effects on source. Sourcing the file alone is now
  inert; the user must invoke the function explicitly. This prevents
  accidental re-execution if scripts are loaded outside the intended
  context.

* `TODO.md` added to `.Rbuildignore`.

# brfssTools 0.0.0.9005

## New features

* `brfss_crosswalk_audit()`: diffs a crosswalk's `core`-tagged rules
  against a registered reference pool's actual variable inventory
  (typically `"National"`), to surface rows that are candidates for
  retagging to a state-specific source (`"OR"`, etc.). Useful once when
  onboarding a National pool to clean up legacy state-as-core curation
  noise. Returns a tibble with `in_reference`, `reference_years_seen`,
  `suggested_source`, and `needs_action` columns. Strict per-year
  matching by default; relaxed (any-year) matching available via
  `strict_year_match = FALSE`.

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
