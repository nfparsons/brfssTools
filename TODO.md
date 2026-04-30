# brfssTools — Backlog

The previous (pre-0.1.0) TODO is preserved as
`TODO-archive-pre-0.1.0.md`. This file reflects work remaining as of
end-of-session April 30, 2026.

---

## 1. Editor: structured by_year UI (priority next session)

The runtime supports `by_year:` blocks that override `inputs:` and
`levels:` per year range. The editor's "Edit YAML" button currently
sends users to their system editor to write these by hand. The next
pass should provide a structured GUI:

- [ ] Demographics tab → click Edit on a configured demographic →
  opens a structured form (not the file editor)
- [ ] Form has a default-block panel (inputs alias picker, levels table)
- [ ] "+ Add year-range override" button creates a new collapsible
  section with its own inputs and levels editor
- [ ] Levels table supports add/remove rows, drag-to-reorder, and
  inline editing of value/label/when
- [ ] Save writes the YAML; optionally renders companion .R

Estimated: 2-3 sessions of focused Shiny work.

---

## 2. Pass 2: CDC calculated-variables crosswalk

`inst/extdata/cdc_calculated_vars.csv` (1,214 rows) sits ready. These
are CDC's `_RACE`, `_AGE65YR`, `_BMI5CAT`, `_RFHLTH`, etc.

- [ ] Audit each calc var: does it already exist in the crosswalk?
- [ ] Add new concept_ids for unmapped calc vars
- [ ] Crosswalk year-by-year (most are CDC-stable)

Estimated: 1-2 days.

---

## 3. Triage 36 pending F_unmatched rows

`pending.csv` holds rows the matcher couldn't auto-classify during
Pass 1. Half a day of editor work.

---

## 4. ACEADNED1/2/3/4 → ACEADNED concept rename

Confirmed during testing in 0.1.0: the four versioned ACE variables
across 2021-2024 are still distinct concept_ids. Should collapse to
a single conceptual `ACEADNED` via the editor's "Rename concept
everywhere" action. 5 minutes when you sit down to it.

---

## 5. State-data vignette rewrite

The state-data vignette currently reflects the pre-0.1.0 architecture.
Needs rewrite against the new workflow:

1. Install
2. Initialize a config (DEMO for tryouts; OR with state_codebook_path)
3. Check status (`brfss_status()`)
4. **Set up demographics first** — `brfss_setup_demographic()` for the big six
5. Register raw data files (`brfss_set_pool()`)
6. Open editor for outcome variable mapping
7. Pull data
8. Hand off to tidyverse

Estimated: 1 session.

---

## 6. Argument naming: `year` → `vintage`

Pure naming question. Not blocking. Requires deprecation shim if done.

---

## 7. `brfss_download()` collapse into `brfss_pool()`

Unified `brfss_pool(source, ...)` that handles both download (CDC) and
register (state). UX improvement, not blocking.

---

## 8. HAVHPAD 2012 → COPD pair verification

Inspect 2012 codebooks; confirm or break the pair. 5 minutes.

---

## 9. Documentation polish

- [ ] **README.md rewrite** — currently describes the old API
- [ ] **pkgdown site** (optional) — `_pkgdown.yml`, group reference index

---

## 10. Templates — done

All shipped templates target CDC variable names. State users add
by_year overrides for their state codebook.

- [x] race — done (CDC stable; by_year example included)
- [x] sex — done (SEX/SEX1/SEXVAR transition handled)
- [x] income — done (INCOME2/INCOME3 unified template added)
- [x] ethnicity — done (CDC stable; by_year example included)
- [x] education — done (EDUCA stable across years)
- [x] age — done (`_AGE80` stable across years)

---

## 11. Ongoing: crosswalk curation

Periodic review for newly-discovered drift, newly-fielded modules,
deprecated variables. Recurring slot.

---

## Summary

| Section | Effort | Architectural? |
|---|---|---|
| 1. Editor structured by_year | 2-3 sessions | yes — completes editor |
| 2. Pass 2 calc vars | 1-2 days | data |
| 3. F_unmatched triage | half day | data |
| 4. ACEADNED rename | 5 min | data |
| 5. State-data vignette | 1 session | release-quality |
| 6. year → vintage | 1 hour | naming |
| 7. download/pool collapse | half day | UX |
| 8. HAVHPAD verify | 5 min | data |
| 9. Docs polish | half day | release-quality |
| 11. Ongoing curation | recurring | data |

Package is **fully functional** today. The remaining items are
quality-of-life and data-curation work, not blockers.
