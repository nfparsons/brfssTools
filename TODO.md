# brfssTools — Backlog

Status: v0.2.0 shipped May 1, 2026. The previous TODO archives are
preserved as `TODO-archive-pre-0.1.0.md` and (covering the 0.1.0
to 0.2.0 transition) the v0.1.0 entries in NEWS.md.

---

## 1. Stage D: brfss_pull for v0.2.0 schema

The v0.1.0 `brfss_pull()` was tightly coupled to `cdc_var` /
`is_primary` / `source` and the separate `transformations/` folder.
For v0.2.0 it needs:

- [ ] Resolve each cell as either a column lookup (`is_calculated == 0`)
  or YAML eval (`is_calculated == 1`)
- [ ] For YAML cells: parse the inline `calculation_yaml` string and
  call the existing `categorical_map` / `passthrough` runtime
- [ ] Drop dependency on `cdc_var`, `state_only`, etc.
- [ ] Output schema: tibble with `year`, `SEQNO`, `_STATE`, plus one
  column per requested concept, plus `.weight`, `.strata`, `.psu`
  resolved from concepts named `weight`/`strata`/`psu` if present

Estimated: 1 session.

---

## 2. National crosswalk (build + ship)

Build a fully-mapped CDC National BRFSS crosswalk via the editor as
a test of the v0.2.0 workflow. Ship as `inst/extdata/cdc_drafted.csv`
for users to import via `brfss_import_config()`.

- [ ] Set up CDC LLCP files as a test pool
- [ ] Run `brfss_init_state("CDC", ..., draft = TRUE)`
- [ ] Walk through every domain in the editor
- [ ] Resolve all Unassigned concepts
- [ ] Set up calculated cells where CDC's variable structure changed
  year-to-year (race, income, sex, etc.)
- [ ] Export the finished crosswalk and ship as opt-in import target
- [ ] Vignette section explaining how to use it

Estimated: several focused sessions of curation, plus testing.

---

## 3. Codebook reader (Stage C from spec)

If a user's pool directory has a sibling `documentation/` folder with
codebook CSVs, the editor should display question text and value labels
for the selected cell.

- [ ] `brfss_load_codebook(year, dataset)` parses the CSV
- [ ] Editor renders question text + value labels in the right panel
- [ ] Format spec: `{state}_{year}_codebook.csv` with columns
  `variable_name`, `year`, `question_text`, `value_labels`, `notes`
- [ ] Ship `inst/codebook_prompt_template.md` users can hand to an AI
  agent along with their codebook PDF/Word doc to produce the CSV

Estimated: 0.5 session.

---

## 4. Outstanding cleanups

Stuff Stage A/B left behind:

- [ ] `R/matcher.R`, `R/crosswalk_audit.R`, `R/clean_age.R` —
  v0.1.0 helpers no longer used by anything active. Audit and
  archive what's truly dead.
- [ ] `R/transformations.R` — file-based transformation system; with
  per-cell YAML in v0.2.0, this is mostly redundant. Audit; archive
  if nothing references it.
- [ ] `brfss_status()` — currently still mentions `state_only`,
  `pending` files that don't exist in v0.2.0. Clean up the output.
- [ ] Remaining v0.1.0 vestiges in user-facing surfaces (search/lookup
  helpers; do they still make sense or should they retire?).

Estimated: 0.5 session.

---

## 5. Domain auto-assigner improvements

The current matcher catches ~73% of CDC variables but misses many
underscore-prefixed calculated vars and most state-specific names.
Low-priority improvements:

- [ ] Match against `cdc_calculated_vars.csv` for the `_*` names
- [ ] Fuzzy matching for trailing version digits (`ADDEPEV1` → match
  `ADDEPEV2`'s domain)
- [ ] User-extensible domain mapping (config file overrides)

---

## 6. pkgdown site

Build out a pkgdown site for the v0.2.0 docs.

- [ ] `_pkgdown.yml` with grouped reference index
- [ ] Hosted (GitHub Pages?)

Estimated: 0.5 session.

---

## 7. Tests

The test suite is thin. Stage A/B added some, but core flows have no
coverage.

- [ ] Test `brfss_draft_crosswalk()` end-to-end against a synthetic
  pool
- [ ] Test the migration function on a known-good v0.1.0 fixture
- [ ] Test `cw_merge_concepts()` happy path + conflict cases
- [ ] Test the sanitization rules
- [ ] Editor tests via `shinytest2`?

---

## Summary

| # | Item | Effort | Architectural? |
|---|---|---|---|
| 1 | brfss_pull rewrite | 1 session | yes |
| 2 | National crosswalk | several sessions | data + release |
| 3 | Codebook reader | 0.5 session | yes |
| 4 | v0.1.0 cleanup | 0.5 session | hygiene |
| 5 | Domain assigner improvements | 1 session | quality |
| 6 | pkgdown | 0.5 session | docs |
| 7 | Tests | 1+ session | quality |

The v0.2.0 release is **fully functional for crosswalk building**.
Pull, codebook context, and the National crosswalk are the items that
will roll into v0.2.x point releases.
