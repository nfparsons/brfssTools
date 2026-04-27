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
