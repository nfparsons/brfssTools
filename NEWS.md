# brfssTools 0.1.0

First architecture-complete release. The crosswalk schema, pull
machinery, and editor have been redesigned around a unified `concept_id`
addressing system. User data now lives in a per-user config directory
that persists across package upgrades.

## Architecture

* **Unified `concept_id` schema.** The crosswalk is now a thin
  three-column join: `concept_id` (the unit of analysis) maps a state
  variable and a CDC variable for each year. Different state variables
  across years can share a `concept_id` — `ACEHURT1`/`ACEHURT2`/`ACEHURT3`
  collapse to a single conceptual `ACEHURT`. `brfss_pull("ACEHURT")`
  resolves to the right per-year column automatically.

* **Hybrid storage layer.** Read-only package files (CDC codebook,
  taxonomy) ship with the package; writable user files (crosswalk,
  state codebook, transformations) live in
  `tools::R_user_dir("brfssTools", "config")`. Per-call `path =`
  arguments override for project-local, git-tracked configs.

* **Transformations.** Two systems for derived variables:
  * Declarative YAML *categorical maps* (preferred) — describe an
    output column as an ordered list of `(value, label, when)` rules
    with year-aware input aliasing. Example: `race.yaml` mirrors CDC's
    `_RACE` calculated variable spec.
  * Functional `.R` files (escape hatch) — for transformations YAML
    can't express.
  Both are detected by `brfss_pull()` from the config dir's
  `transformations/` folder.

## New functions

* `brfss_init_state(state, state_codebook_path = NULL, path = NULL)`
  scaffolds a user's config directory. State `"OR"` uses the
  package-shipped sample as a starting point; other states require a
  user-supplied state codebook CSV.
* `brfss_config_path(path = NULL)` resolves the active config dir.
* `brfss_validate_state_codebook(df)` validates an incoming state
  codebook against the expected schema.
* `cw_load(path = NULL)` loads the crosswalk bundle. Auto-populates
  `concept_id` on every load.
* `cw_save(bundle, path = NULL)` writes back with `.bak` rotation;
  refuses to write to package extdata.
* `cw_add_pair`, `cw_remove_pair`, `cw_update_pair`,
  `cw_mark_state_only`, `cw_mark_cdc_only`,
  `cw_replace_cdc_partner`, `cw_replace_state_partner`,
  `cw_rename_concept`, `cw_recompute_concept_ids` — full CRUD.
* `brfss_crosswalk_editor(path = NULL)` launches a local Shiny app
  with a heatmap, edit panel, CDC-anchored ↔ State-anchored mode
  toggle, and bulk concept-rename action.
* `brfss_setup_categorical_map(name)`, `brfss_setup_race_map()`,
  `brfss_load_categorical_map(fp)`,
  `brfss_render_transformation_code(name, save = TRUE)` — declarative
  YAML transformations.
* `brfss_setup_transformation(name)`, `brfss_setup_race()`,
  `brfss_list_transformations(path = NULL)` — functional `.R`
  transformations.

## Rewritten functions

* `brfss_pull()` is rewritten against the new schema. Accepts
  `concepts`, `domains`, `tags`, plus a `core_demographics` shortcut.
  Auto-detects `source` (state vs. CDC) from registered pools.
  Applies YAML categorical maps and functional `.R` transformations
  per year.
* `brfss_search(query, scope, path)` and
  `brfss_lookup(concept_ids, path)` rewritten as thin search/lookup
  helpers against the new bundle.

## Vignettes

* `state-data-workflow.Rmd` — end-to-end walkthrough for analysts with
  state-collected BRFSS files. Oregon-shipped sample as the running
  example; adapts to other states via `state_codebook_path`.
* `national-data-workflow.Rmd` — end-to-end walkthrough for analysts
  using only CDC's public LLCP files via `brfss_download()`.

## Removed

* The legacy `brfss_crosswalk()` API (read `concept_map`, `concepts`,
  `concept_values` CSVs, with embedded recode rules) is replaced by
  `cw_load()` plus the transformation system. Files
  `concept_map_brfss.csv`, `concepts_brfss.csv`,
  `concept_values_brfss.csv` removed from `inst/extdata/`. The functions
  `brfss_harmonize_vec()` and `brfss_clean_race()` are gone — the same
  logic now lives in YAML categorical maps under
  `transformations/race.yaml`.

## Demographics-first workflow

* **18 demographic templates ship with the package** under
  `inst/extdata/transformations/templates/`, covering the BRFSS "big six":
  age (6 variants), sex (3), race (2), ethnicity (2), education (2),
  income (4). `brfss_setup_demographic(name, template)` copies a chosen
  template into the user's `transformations/` folder, ready to use.
* `brfss_list_demographic_templates()` shows what's available; filter
  by `name` to see all templates for a single demographic.
* `brfss_demographic_status()` reports a checklist of which big-six
  demographics are configured.
* New transformation type **`passthrough`**: takes a single input
  column and renames it to the output column with no recoding. Used
  by passthrough-style templates (e.g., `age__continuous`).

## Year-aware transformations

* The categorical-map runtime now supports `by_year:` blocks that
  override `inputs:`, `levels:`, or both for specified year ranges.
  This handles real BRFSS history: e.g., `INCOME2` (8 brackets, pre-2021)
  vs `INCOME3` (11 brackets, 2021+) become a single unified `income`
  column via `income__cdc_unified` template.
* Templates with `by_year` shipped: `sex__birth_binary` (BIRTHSEX/SEX
  fallback), `sex__selfreport_binary` (SEX/SEX1/SEXVAR transition),
  `sex__gender_identity_4cat`, `income__cdc_unified`,
  `income__quartiles`. Race uses CDC's stable `_MRACE1`/`_HISPANC` so
  no by_year is needed for National data; the template documents
  state-codebook overrides as commented-out examples.
* Validator accepts levels-only-in-by_year: a transformation can
  define different level coding per year range with no top-level
  fallback.

## Discoverability and recovery

* `brfss_status()` prints a one-shot summary of where everything is:
  config dir, state, files, transformations, registered pools, cache.
  Run this any time you're not sure where the package is looking.
* `brfss_reset()` wipes the config dir back to DEMO state, with
  automatic timestamped backup of the previous config alongside.
* `brfss_export_config(zip_path)` and `brfss_import_config(zip_path)`
  bundle the config dir as a portable zip for moving work between
  machines or sharing curated crosswalks.

## CDC seed crosswalk

* The package now ships `inst/extdata/cdc_seed.csv` (4,209 rows): every
  CDC variable for every year 2012-2024, with `concept_id = cdc_var`,
  `state_var = NA`, `unverified = 1`. By default `brfss_init_state()`
  populates the user's `crosswalk.csv` from this seed; the user fills
  in `state_var` for variables their state collects, then verifies.
  Pass `seed_from_cdc = FALSE` for an empty start.
* New `unverified` column on the crosswalk schema. `cw_load()`
  back-fills this for legacy crosswalks loaded without it.
* `cw_map_seeded(year, cdc_var, state_var)` is the workflow function
  for assigning a state variable to a seeded row in one call (sets
  state_var, source = "manual_edit", clears unverified).
* `cw_verify_pair(year, state_var, cdc_var)` clears the unverified
  flag without other changes.

## DEMO replaces OR as shipped sample

* No state-collected data ships with the package install. The shipped
  sample is now `state = "DEMO"` — 15 fake variables over 3 years
  using the `EXAMPLE_*` prefix. Real states require a user-supplied
  `state_codebook_path` to `brfss_init_state()`.

## Editor

* Third tab added: **Demographics**. Shows the big-six checklist with
  configured/not-configured status. "Set up" opens a modal with a
  template picker; "Edit YAML" opens the file in the user's editor;
  "Change template" replaces the YAML with a different choice;
  "Remove" deletes with .bak rotation.
* Editor renders crosswalk rows with `kind = "seeded"` distinct from
  paired rows, with appropriate label ("CDC seed (unverified)" or
  "CDC seed (verified, not mapped)").
* `.cw_find()` semantics fixed: NA matches literal NA in a column;
  pass NULL to skip filtering on that field. Previously NA was
  ambiguously treated as "skip filtering".

# brfssTools 0.0.0.9010

## Crosswalk migration

* Added `inst/migrate/05-retag-core-to-or.R` — Migration 05 retags
  `source = "core"` rows whose `raw_var_name` is absent from National
  but present in OR's pool, flipping them to `source = "OR"`. Run
  `migrate_05_retag_core_to_or(apply = TRUE)` from the package root,
  with both National and OR pools registered. Defaults to a dry run
  with sanity checks. Rows whose raw column is in neither pool
  ("phantoms") are left as `core` and written to
  `inst/migrate/05-phantom-review.csv` for manual review. Expected
  effect: ~3,958 rows retagged, ~74 phantoms surfaced.

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
