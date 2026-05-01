# brfssTools

A tool for harmonizing longitudinal Behavioral Risk Factor Surveillance
System (BRFSS) survey data. The crosswalk is anchored on **your most
recent year of data**: the package reads your latest year's variable
list, treats each variable as a concept, and helps you build a draft
crosswalk by mapping older years backwards through a Shiny editor.
Cells can be flagged "calculated" to store inline YAML defining a
year-specific computation.

## Install

```r
# install.packages("pak")
pak::pak("nfparsons/brfssTools")
```

## Quick start

```r
library(brfssTools)

# 1. Point at your data
brfss_set_pool("OR", "Z:/Secure_Data/Oregon_BRFSS")

# 2. Initialize and draft from the most recent year
brfss_init_state(
  state               = "OR",
  state_codebook_path = "~/data/oregon_codebook.csv",
  draft               = TRUE
)

# 3. Open the editor to fill in older years
brfss_crosswalk_editor()
```

See `vignette("getting-started", package = "brfssTools")` for the full
walkthrough.

## What's in the box

- `brfss_init_state()` / `brfss_status()` / `brfss_reset()` — config
  scaffolding and state management.
- `brfss_set_pool()` / `brfss_pool_status()` — register and inspect
  your data files.
- `brfss_draft_crosswalk()` / `brfss_redraft_crosswalk()` — generate
  the crosswalk from your most recent year; idempotent extension when
  new years arrive.
- `brfss_crosswalk_editor()` — Shiny GUI for filling in older years,
  flagging calculated cells, assigning domains, merging duplicate
  concepts.
- `cw_load()` / `cw_save()` plus a console CRUD layer
  (`cw_set_var`, `cw_set_calc`, `cw_assign_domain`, `cw_verify`,
  `cw_rename_concept`, `cw_merge_concepts`, `cw_add_concept`,
  `cw_remove_concept`).
- `brfss_export_config()` / `brfss_import_config()` — portable zips
  for moving work between machines.
- `brfss_migrate_crosswalk_to_v2()` — one-shot upgrade from a
  v0.1.0 crosswalk.

## Status

This is **v0.2.0**, which represents a significant architectural
rewrite from v0.1.x. The crosswalk schema is now anchored on user
data rather than CDC's variable list; the editor is rebuilt around
per-cell mapping; and demographics are no longer architecturally
special — they're just concepts like any other.

The Shiny editor is the main interface. Console-only workflows
(`cw_*` family) are also fully supported.

`brfss_pull()` (loading harmonized data from your raw files) is
deferred to a follow-up release. The crosswalk-building workflow is
the focus of v0.2.0.

A pre-built National crosswalk (mapping CDC's National BRFSS variables
across all years) is planned to ship in a future release as an
opt-in `brfss_import_config()` target. Not in v0.2.0.

## Issues

File issues at https://github.com/nfparsons/brfssTools/issues.
