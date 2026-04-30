# brfssTools — Backlog

Items grouped by approximate effort and architectural significance.
Tick as you go.

The previous (pre-0.1.0) TODO is preserved as
`TODO-archive-pre-0.1.0.md` for historical reference.

---

## 1. Editor: categorical-map GUI tab

The runtime for declarative YAML transformations works (Stage 3a). Users
can already edit `race.yaml` by hand. The next step is a structured GUI
tab in `brfss_crosswalk_editor()` that renders the YAML as an editable
table.

- [ ] Add a third tab to the editor: **Transformations**.
- [ ] List existing transformations (one row per file from
  `brfss_list_transformations()`), with kind classification (yaml / r /
  r_autogen) and last-modified timestamp.
- [ ] Selecting a row opens a structured editor for the YAML: table of
  levels (value / label / when), an inputs alias editor, and a
  description field.
- [ ] Add/remove rows for levels; drag-and-drop reorder (rule precedence
  is row order).
- [ ] **Save** writes the YAML; an optional checkbox "also render
  companion .R" calls `brfss_render_transformation_code(..., save = TRUE)`.
- [ ] **New transformation** button, with template selector (race /
  generic / blank).

This is roughly the same effort as the original editor build — a few
hundred lines of Shiny UI + reactive logic. Plan for one focused
session.

---

## 2. Pass 2: CDC calculated-variables crosswalk

`inst/extdata/cdc_calculated_vars.csv` (1,214 rows) sits ready to be
worked through. These are CDC's `_RACE`, `_AGE65YR`, `_BMI5CAT`,
`_RFHLTH`, etc. — the calculated variables documented in the codebook
that aren't raw responses.

For each, two questions: (a) does this concept already exist in the
crosswalk under a different name (mapping issue)? (b) if not, should it
be added as a new `concept_id` and crosswalked across years?

- [ ] Run an audit pass: for each calc var, search the crosswalk for
  semantically-similar concepts. Many `_RACE` rows are already covered
  by `transformations/race.yaml`.
- [ ] Add new concept_ids for unmapped calc vars.
- [ ] Crosswalk them year-by-year (most are CDC-stable, low effort).

Estimated: 1-2 days of focused crosswalk work.

---

## 3. Triage 36 pending F_unmatched rows

`pending.csv` holds rows the matcher couldn't auto-classify during
Pass 1. They've been sitting there since the initial build.

- [ ] Open the editor, navigate to the **Pending** tab. (Doesn't exist
  yet — see Section 1; or process directly from `pending.csv` for now.)
- [ ] For each row, decide: pair, state-only, cdc-only, or drop.
- [ ] Apply via `cw_add_pair`, `cw_mark_state_only`, etc.

Estimated: half a day.

---

## 4. ACEADNED1/2/3/4 → ACEADNED concept rename

Confirmed during testing in 0.1.0: the four versioned ACE variables
across 2021-2024 are still distinct concept_ids in the crosswalk. They
should collapse to a single conceptual `ACEADNED`.

- [ ] Open the editor, find ACEADNED1, click any row.
- [ ] Edit the concept_id field to `ACEADNED`.
- [ ] Click **Rename concept everywhere**, confirm.
- [ ] Repeat for ACEADNED2/3/4 if they didn't merge automatically (they
  have different state_var names per year so they will need separate
  rename passes; or fix by editing crosswalk.csv directly to set all
  four rows' concept_id to `ACEADNED`, then `cw_save()`).

Estimated: 5 minutes once you sit down to it.

---

## 5. Argument naming: `year` → `vintage`

Function arguments still use `year` (singular accepting a vector) per
tidycensus convention. There's an open thought about migrating to
`vintage` to match the BRFSS-specific terminology (a "vintage" of the
codebook is the year it applies to).

This is a pure naming change, not architectural. Not blocking anything.

- [ ] Decide whether to make the change.
- [ ] If yes: bulk rename in function signatures, doc, and vignettes.
  Add a deprecation shim that translates `year =` to `vintage =` with
  a warning during the transition.

---

## 6. `brfss_download()` collapse into `brfss_pool()`

Right now `brfss_download()` and `brfss_set_pool()` are separate
functions. A unified `brfss_pool(source, ...)` could:
- For `source = "cdc"`: download + register
- For `source = "state"`: register a directory the user already has

Cleaner one-stop shop, less for users to remember.

- [ ] Design the unified API.
- [ ] Implement; deprecate the two-function approach.

Not blocking anything; UX improvement.

---

## 7. HAVHPAD 2012 → COPD pair verification

In the crosswalk, this pair is currently flagged unverified. Need to
look at the actual codebook entries to confirm whether HAVHPAD in 2012
is genuinely the COPD question (might be heart-related instead).

- [ ] Inspect codebooks for both vars in 2012.
- [ ] Confirm or break the pair.
- [ ] Update notes accordingly.

5 minutes.

---

## 8. Documentation polish

- [ ] **README.md rewrite** — currently describes the old
  `brfss_crosswalk()` API and REAL-D race classifications. Should
  instead show: install → init_state → editor → setup_race_map →
  pull. Match the vignette flow.
- [ ] **Package help page** (`R/brfssTools-package.R`) — bare
  `_PACKAGE` with the right one-paragraph summary, family `@seealso`
  cross-refs to the main entry points.
- [ ] **pkgdown site** (optional) — set up `_pkgdown.yml`, group the
  reference index by family (init / editor / pull / transformations).

---

## 9. Ongoing: crosswalk curation

Not a task with a finish line, but worth a recurring slot:

- [ ] Periodic review of the crosswalk for newly-discovered drift,
  newly-fielded modules, deprecated variables. As CDC publishes the
  next year's codebook, add a year and triage.

---

## Summary

| Section | Effort | Architectural? |
|---|---|---|
| 1. Editor GUI tab | 1 session | yes — completes the YAML workflow |
| 2. Pass 2 calc vars | 1-2 days | data |
| 3. F_unmatched triage | half day | data |
| 4. ACEADNED rename | 5 min | data |
| 5. year → vintage | 1 hour | naming, optional |
| 6. download/pool collapse | half day | UX, optional |
| 7. HAVHPAD verify | 5 min | data |
| 8. Docs polish | half day | release-quality |
| 9. Ongoing curation | recurring | data |

The package is **fully functional** today — sections 2-4 and 7 are
data quality work, not blockers. Section 1 (editor GUI tab) is the
last meaningful architectural piece.
