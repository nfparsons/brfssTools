# brfssTools

A tool for harmonizing longitudinal Behavioral Risk Factor Surveillance
System (BRFSS) survey data. The crosswalk is anchored on **your most
recent year of data**: the package reads your latest year's variable
list, treats each variable as a concept, and helps you build a draft
crosswalk by mapping older years backwards through a Shiny editor.
Cells can be flagged "calculated" to store inline YAML defining a
year-specific computation.

<!-- ![brfssTools editor](man/figures/editor-screenshot.png) -->

## Install

```r
# install.packages("pak")
pak::pak("nfparsons/brfssTools")
```

## Quick start

```r
library(brfssTools)

# 1. Point at your data
brfss_set_pool("OR", data_path = "Z:/Secure_Data/Oregon_BRFSS")

# 2. Draft the crosswalk from your most recent year of data
brfss_draft_crosswalk(dataset = "OR")

# 3. Open the editor to fill in older years
brfss_crosswalk_editor(dataset = "OR")
```

For National-data users:

```r
brfss_use_national(years = 2018:2024)
brfss_draft_crosswalk(dataset = "National")
brfss_crosswalk_editor(dataset = "National")
```

See `vignette("getting-started", package = "brfssTools")` for the full
walkthrough including codebooks and migration.

## What's in the box

### Pool management
- `brfss_set_pool()` — register a dataset (only `data_path` is
  required; codebook and crosswalk paths default sensibly)
- `brfss_pool_status()` / `brfss_pool_get()` / `brfss_clear_pool()` —
  inspect and manage pools
- `brfss_status()` — printed summary of all registered pools

### Drafting and migration
- `brfss_draft_crosswalk()` / `brfss_redraft_crosswalk()` — generate
  the crosswalk from the most recent year; idempotent extension when
  new years arrive
- `brfss_assign_domains()` — domain auto-assignment via shipped CDC
  codebook reference
- `brfss_migrate_crosswalk_to_v2()` — upgrade a v0.1.0 crosswalk file
  to v0.2.0 schema
- `brfss_migrate_config_dir()` — for users with pre-release v0.2.0
  config-dir state

### Editor and console CRUD
- `brfss_crosswalk_editor()` — Shiny GUI for filling in older years,
  flagging calculated cells, assigning domains, merging duplicate
  concepts
- `cw_load()` / `cw_save()` and `cw_set_var()`, `cw_set_calc()`,
  `cw_assign_domain()`, `cw_verify()`, `cw_rename_concept()`,
  `cw_merge_concepts()`, `cw_add_concept()`, `cw_remove_concept()` —
  programmatic interface to the same operations the editor performs

### Codebook documentation
- `brfss_load_codebook()` — read a (dataset, year) codebook CSV from
  the registered codebook directory; powers the question-text and
  value-label display in the editor
- `brfss_codebook_help()` — print the format spec or AI-agent prompt
  for converting messy raw codebooks into the expected format

### National BRFSS
- `brfss_use_national()` — download CDC LLCP files and register the
  pool in one call
- `brfss_download()` / `brfss_cache_dir()` — direct download interface
  and cache location

### Pool inspection
- `brfss_inventory_pool()` — long-format inventory of variables
  across years
- `brfss_values_pool()` — value-frequency tables across years
- `brfss_detect_year()` — infer the year a data file represents

## Status

This is **v0.2.0**, a significant architectural rewrite from v0.1.x:

- The crosswalk schema is anchored on user data
- The editor is rebuilt around per-cell mapping
- Demographics are no longer architecturally special
- Calculation YAML lives inline in the crosswalk (no separate
  transformations folder)

`brfss_pull()` (loading harmonized data from raw files using the new
schema) is deferred to a follow-up release. The crosswalk-building
workflow is the focus of v0.2.0.

A pre-built National crosswalk (mapping CDC's variables across all
years) is planned for a future release as a shipped artifact users
can opt into; for now, `brfss_use_national()` lets a National user
draft one themselves.

## Issues

File issues at https://github.com/nfparsons/brfssTools/issues.
