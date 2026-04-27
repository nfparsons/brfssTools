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
