# brfssTools — Backlog

Status: v0.2.0 shipped May 1, 2026 with the path-based architecture.
Earlier TODO files are preserved in NEWS.md history.

---

## 1. Stage D: brfss_pull for v0.2.0 schema

The v0.1.0 `brfss_pull()` is stubbed — it errors with a message
pointing at the deferred implementation. For v0.2.x it needs:

- [ ] Resolve each cell as either a column lookup (`is_calculated == 0`)
  or YAML eval (`is_calculated == 1`)
- [ ] For YAML cells: parse the inline `calculation_yaml` string and
  call the existing `categorical_map` / `passthrough` runtime
- [ ] Drop dependency on `cdc_var`, `state_only`, etc.
- [ ] Output: tibble with `year`, `SEQNO`, plus one column per
  requested concept, plus survey-design columns
- [ ] Reuse the v0.1.0 implementation's strategy where possible (it's
  archived at `inst/archive/R/pull_v01.R`)

Estimated: 1 session.

---

## 2. National crosswalk (build + ship)

Build a fully-mapped CDC National BRFSS crosswalk via the editor as a
test of the v0.2.0 workflow. Ship as part of the package install.

- [ ] Use `brfss_use_national(years = 2012:2024)` to download CDC files
- [ ] Run `brfss_draft_crosswalk(dataset = "National")`
- [ ] Walk through every domain in the editor
- [ ] Resolve all Unassigned concepts
- [ ] Set up calculated cells where CDC's variable structure changed
  year-to-year (race, income, sex, etc.)
- [ ] Decide where the shipped crosswalk lives — likely
  `inst/extdata/cdc/National_crosswalk.csv` plus some way for users to
  copy it into their own pool location on first National use
- [ ] Update `brfss_use_national()` to offer to install the bundled
  crosswalk

Estimated: several focused curation sessions.

---

## 3. Reimplement brfss_search and brfss_lookup for v0.2.0

The v0.1.0 versions are archived at `inst/archive/R/crosswalk_v01.R`.
Useful console helpers but not blocking the v0.2.0 release.

- [ ] `brfss_search(query, dataset)` — regex search over crosswalk
- [ ] `brfss_lookup(concept_ids, dataset)` — fetch rows for specific
  concepts

Estimated: 0.25 session.

---

## 4. Outstanding cleanups

- [ ] Audit remaining R files for any v0.1.0 dead code I haven't
  archived: `R/categorical_map.R` (kept the runtime, stubbed the file-
  based exports); `R/transformations.R` (similarly); these have
  archived copies — clean up the active versions to remove now-orphan
  template / file-IO helpers
- [ ] `R/datasets.R` and `R/inventory.R` — review whether the dataset
  registry still serves a purpose under the path-based model
- [ ] `R/state_codebook_validate.R` — was used by the old
  `brfss_init_state(state_codebook_path = ...)` flow; check whether
  any path-based code still calls it

Estimated: 0.5 session.

---

## 5. Domain auto-assigner improvements

Current matcher catches ~73% of CDC variables but misses many
underscore-prefixed calculated vars and state-specific names.

- [ ] Match against `cdc_calculated_vars.csv` for the `_*` names
- [ ] Fuzzy matching for trailing version digits (`ADDEPEV1` → match
  `ADDEPEV2`'s domain)
- [ ] User-extensible domain mapping (e.g., a user-supplied
  `domain_overrides.csv` in the codebook directory)

---

## 6. pkgdown site

Build a pkgdown site for the v0.2.0 docs.

- [ ] `_pkgdown.yml` with grouped reference index
- [ ] Hosted on GitHub Pages

Estimated: 0.5 session.

---

## 7. Tests

The test suite is thin. Path-based refactor added some risk of
regression that tests would catch.

- [ ] Test `brfss_set_pool()` — defaults, overrides, missing data dir,
  etc.
- [ ] Test `brfss_draft_crosswalk()` end-to-end against a synthetic
  pool
- [ ] Test the migration functions on known-good fixtures
- [ ] Test `cw_merge_concepts()` happy path + conflict cases
- [ ] Test the sanitization rules
- [ ] Editor tests via `shinytest2`?

---

## Summary

| # | Item | Effort |
|---|---|---|
| 1 | brfss_pull rewrite | 1 session |
| 2 | National crosswalk | several sessions |
| 3 | brfss_search/lookup reimpl | 0.25 session |
| 4 | v0.1.0 cleanup | 0.5 session |
| 5 | Domain assigner improvements | 1 session |
| 6 | pkgdown | 0.5 session |
| 7 | Tests | 1+ session |

The v0.2.0 release is **fully functional for crosswalk building**.
Pull and the bundled National crosswalk roll into v0.2.x point releases.
