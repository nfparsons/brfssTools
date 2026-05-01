# brfssTools — Crosswalk Manual Work Checklist

Items grouped by effort and dependency. Tick as you go.

---

## 1. Quick curation cleanup (< 30 min total)

These are tiny, mechanical, and unblock nothing big — but they remove
known noise from the concept_map.

- [ ] **Resolve 4 drift-annotation conflicts** (same key, different drift label):
  - [ ] `CSRACE1O` 2013 — pick one of `moderate` / `major`
  - [ ] `CSRACE1O` 2014 — pick one of `moderate` / `major`
  - [ ] `HAOHP12` 2014 — pick one of `identical` / `moderate`
  - [ ] `SMARTPHBRAND` 2014 — pick one of `identical` / `major`

  Inspect with:
  ```r
  cm |> filter(concept_id %in% c("CSRACE1O", "HAOHP12", "SMARTPHBRAND")) |>
        filter(year %in% c(2013, 2014))
  ```
  Then `cm |> distinct(concept_id, year, raw_var_name, .keep_all = TRUE)`
  after removing the duplicates you don't want.

- [ ] **Verify the missing 2018 PSU row.** Either OR's 2018 file genuinely
  has no PSU column or this is a curation gap. Check via
  `brfss_inventory_pool("OR")` once 2018 is registered. If the file does
  have PSU, add a row: `psu, OR, 2018, PSU, identical, identity, , `.
  (Note: tag `source = "OR"` if you confirm OR-specific; currently the
  existing `psu` rows are tagged `core` carried over from the original
  curation.)

- [ ] **Decide on `Survey_Weight` concept** (the 2012-2013-only meta-flag,
  description "Survey Version-Use in Determining Correct Weight"). Options:
  - Leave it as inventory-only (it's not the real weight; that's
    `survey_weight` mapped to `wtrk_*`).
  - Rename for clarity (e.g., `survey_version_flag`).
  - Drop the concept entirely if you're not using the meta-flag.

---

## 2. State-tag retagging (1 day, do once)

After Migration 04 the source distribution is **5,498 core / 142 OR**. But
many of those `core` rows are actually OR-specific (`RACE1`-`RACE10`,
`wtrk_*` variants, `xraceth`, `ORACE3`, etc.) — they exist only in OR's
analyst files, not in CDC's public LLCP.

When you register a National pool, the pull engine will look for these
raw column names in LLCP files and not find them. The fix is mechanical
but bulk: retag rows where `raw_var_name` doesn't appear in any LLCP file.

- [x] **Build the state-tag audit helper** ✅ Done — `brfss_crosswalk_audit()`
  in `R/crosswalk_audit.R` with tests in `tests/testthat/test-crosswalk-audit.R`.
  Diffs `core`-tagged concept_map rows against a registered National
  pool's actual variable inventory. Returns a tibble with columns
  `concept_id, year, raw_var_name, source, in_reference,
  reference_years_seen, suggested_source, needs_action`.

- [ ] **Register a National pool** before running the audit:
  ```r
  brfss_download(2012:2024)   # one-time per machine
  ```

- [ ] **Run the audit** and review the output:
  ```r
  cw <- brfss_crosswalk()      # no dataset filter -> all rules
  audit <- brfss_crosswalk_audit(cw,
                                 reference_dataset = "National",
                                 suggested_source  = "OR")

  # Summary view: which raw variables fail per-year matching?
  audit |>
    dplyr::filter(needs_action) |>
    dplyr::count(raw_var_name, reference_years_seen, sort = TRUE)
  ```
  Expected categories likely to surface as `needs_action = TRUE`:
  - [ ] `RACE1`-`RACE10` (2013-2016) — pre-REAL-D OR race arrays
  - [ ] `wtrk_*` (all variants) — OR-specific weight variants
  - [ ] `xraceth`, `XRACETH_EXP`, `xrace_exp2`, `RACETHX_EXP2`, `ORACE3` —
        OR-derived race recodes
  - [ ] `agegrp_or` and any other `*_or` suffix variables
  - [ ] OR-derived age groupings (`age10`, `age50g3`, `age40g3`, etc.)
  - [ ] OR-specific Hispanic concepts (`racesum1`/`racesum2` family)

- [ ] **Apply the retags** as Migration 05. Rough template:
  ```r
  to_retag <- audit |> dplyr::filter(needs_action)
  cm <- read_csv("inst/extdata/concept_map_brfss.csv")
  cm <- cm |>
    dplyr::left_join(
      to_retag |> dplyr::select(concept_id, year, raw_var_name,
                                suggested_source),
      by = c("concept_id", "year", "raw_var_name")
    ) |>
    dplyr::mutate(source = dplyr::coalesce(suggested_source, source)) |>
    dplyr::select(-suggested_source)
  write_csv(cm, "inst/extdata/concept_map_brfss.csv", na = "")
  ```
  Spot-check before committing — some rows you may want to keep as `core`
  even if absent from National (e.g., if a variable is upcoming in CDC
  but not yet in your downloaded years).

---

## 3. Concept merges from `rename_candidates_brfss.csv`

454 candidate pairs detected, broken down by confidence:

- **52 HIGH-confidence pairs** — usually CDC just renamed a variable
  (`ACEDIV` → `ACEDIVRC`, `BPMEDS` → `BPMEDS1`). Question text matches
  closely (jaccard ≥ 0.80 typically), value codes identical, year ranges
  contiguous.
- **307 MEDIUM-confidence pairs** — need careful question-text inspection.
- **94 LOW-confidence pairs** — most are probably NOT renames; require
  deliberate review.

Currently each pair lives as two separate concept_ids in your map. After
merging, they'd be one concept with multiple raw_var_name values per year.

- [ ] **Triage the 52 HIGH pairs first**. For each:
  - [ ] Decide canonical concept_id (usually the newer one, since it
        represents the current CDC standard)
  - [ ] Decide whether to harmonize value codes if any drift exists
        (most HIGH pairs have identical codes)
  - [ ] Decide drift label (`identical` / `minor` / `moderate` / `major`)

- [ ] **Build a merge migration** (Migration 06). Each merge:
  - Pick canonical concept_id (e.g., `ACEDIVRC` over `ACEDIV`)
  - Update concept_map: rows for the deprecated id get rewritten to
    use the canonical id, raw_var_name preserved
  - Update concepts: drop the deprecated row
  - Update concept_values: dedupe/merge against the canonical row

- [ ] **Triage the 307 MEDIUM pairs**. Faster as a spreadsheet pass —
  open `rename_candidates_brfss.csv`, add an `action` column, mark each
  row `merge` / `keep_separate` / `defer`.

- [ ] **Triage the 94 LOW pairs** last. Most will end up `keep_separate`.

---

## 4. Recode rules for drift-annotated rows

Currently every `recode_type` is `"identity"` — pulls return raw codes
unchanged. Fine for the 4,644 rows tagged `identical`, but the **996 rows
tagged `minor`/`moderate`/`major` are a real harmonization gap**. The
package's `brfss_harmonize_vec()` supports two recode types beyond
`identity`:

- `inline` — small string like `"1=Yes; 2=No; 7=NA; 9=NA"`
- `function` — name of a function the engine can call

The engine works; the rules just haven't been written.

This is multi-week, per-domain work. Suggested ordering:

- [ ] **Race & ethnicity drift** (mostly resolved by Migration 04 +
      `brfss_clean_race()` rewrite — the cleaner does the harmonization
      in code, so concept_map identity is fine for these).

- [ ] **Demographics — sex/gender drift.** `SEX1` / `SEX2` / `SEXVAR` /
      `BIRTHSEX` over the years. Question wording shifted significantly
      in the 2018+ era. Probably 4-6 concept rows need actual recodes.

- [ ] **Demographics — education drift.** Codes were stable but cut-points
      varied for some downstream uses. Maybe 2-3 rules.

- [ ] **Demographics — income drift.** Income brackets changed multiple
      times (2018 added more upper brackets). Substantive recoding work
      to make pre-2018 and post-2018 comparable.

- [ ] **Health behaviors — smoking, drinking, exercise.** Each has 5-15
      drift-annotated rows.

- [ ] **Chronic conditions.** Mostly `identity`; biggest issue is the
      `ADDEPEV2` → `ADDEPEV3` style rename pairs (handled by section 3).

- [ ] **Module-specific items** (cancer screening, falls, oral health, etc.)
      — handle as needed for specific analyses; don't try to do all at once.

For each domain pass:
1. Filter concept_map to drift-annotated rows in that domain
2. Read the codebook entries for both variants of each variable
3. Write the `recode_rule` string (most are `inline`, a few may need `function`)
4. Update `recode_type` from `identity` to `inline`/`function`
5. Move drift annotation from `moderate`/`major` to `identical` (since
   the rule now bridges the drift)

---

## 5. Concept_values backfill (low priority)

406 of 1,743 concepts (~23%) have no value labels in
`concept_values_brfss.csv`. Mostly these are the `*_r` suffix recoded
variants (e.g., `ACEDIVr`, `ACEDRUGr`) which are already-derived columns
and may not need value labels.

- [ ] **Decide policy**: backfill labels for the `*_r` family or leave
      blank? They're not used by the harmonization engine, only for
      documentation/lookup via `brfss_lookup()`.

- [ ] **If backfilling**: derive labels by inspecting the source variable
      and the recode logic, OR pull from OR's master codebook directly.

---

## 6. Pre-release package polish (orthogonal to the crosswalk)

- [ ] **Author field in DESCRIPTION** — currently a placeholder.
- [ ] **License** — run `usethis::use_mit_license("Your Name")` (or
      whichever license you want).
- [ ] **Run `R CMD check`** — should be 0 errors / 0 warnings / a few
      benign notes.
- [ ] **Build pkgdown site** (optional, but useful for future
      collaborators).
- [ ] **Run all 11 migration scripts in order on a fresh checkout** to
      confirm idempotency.

---

## 7. Open questions still requiring your input

These are the things I couldn't decide for you:

- [ ] **`age_6group` concept** — Should we add it now (mapped only when
      National data is registered, since OR doesn't have `_AGE_G`), or
      defer until you bring in National? Currently
      `brfss_clean_age(scheme = "6group")` derives from `age_continuous`
      via the waterfall, so this isn't blocking.

- [ ] **`racesum1` / `racesum2`** — what are these? Currently unmapped
      to any canonical concept. If they're OR's pre-REAL-D race
      summary recodes (single-column equivalents to `xraceth`), they
      might warrant their own concept or inclusion in
      `race_legacy_summary`.

- [ ] **HISPANC3 sub-flag family beyond what we mapped** — `HISPANC3_1`–`HISPANC3_4`
      and `HISPANC3_01`–`HISPANC3_04` are now sources for `hisp_array_*`,
      but `HISPANC_R_CDC` exists as a separate concept (CDC-derived
      Hispanic recode). What's its role? Probably a redundant
      already-derived flag — confirm and either map under `hisp_array_1`
      for relevant years or document as inventory-only.

- [ ] **Legacy 2012 Hispanic** — `hisp_array_*` has no mapping for 2012.
      Did OR collect Hispanic origin in 2012 under a different variable
      name, or is 2012 genuinely Hispanic-less? If the former, add a
      mapping; if the latter, document.

- [ ] **2012 race coverage** — only `race_array_2` has a 2012 row
      (carryover from original `RACE2` curation). Real or curation noise?

---

## 8. Taxonomy alignment (architectural, do once)

Architecture commit: **National data sets the categories and
subcategories; state variables map into that foundation; state-specific
modules that don't fit get their own categorization.** Centralizing all
variables to the same set of meta-variable names as much as possible.

This implies two cleanup passes beyond the state-tag retag (Section 2):

### 8a. Domain / subdomain alignment with CDC taxonomy

Currently `concepts_brfss.csv` has partially populated `domain` and
`subdomain` columns using OR-internal terminology (e.g.,
`alcohol_screening_and_brief_intervention_module`, `race_ethnicity`,
`survey_design`). For National-as-foundation, these should mirror CDC's
official module taxonomy — Core Section 1 (Health Status), Section 2
(Healthy Days), Section 3 (Healthcare Access), Section 4 (Hypertension
Awareness), etc., plus optional modules.

- [ ] **Pull CDC's module taxonomy** from the latest LLCP codebook
      (probably the 2024 calculated variables document). Build a
      reference table: `(cdc_module, cdc_section, concept_examples)`.
- [ ] **Map existing concepts** to CDC modules. Most CDC-canonical
      variables will fall in obviously, e.g., `GENHLTH` → Core Section 1.
- [ ] **OR-specific modules** that don't fit any CDC section get their
      own domain values (e.g., `domain = "OR_state_added"`,
      `subdomain = "tribal_inclusion"`).
- [ ] **Apply as Migration 06** — single pass over `concepts_brfss.csv`.

### 8b. Concept-id canonicalization (defer until needed)

Currently the concept_map has two naming conventions coexisting:

- **Canonical snake_case** for the concepts we've worked on this
  session: `survey_weight`, `psu`, `strata`, `race_array_*`,
  `hisp_array_*`, `age_continuous`, `age_5yr`, `age_65plus`.
- **CDC variable names as concept_ids** for everything else:
  `MARITAL`, `EDUCA`, `GENHLTH`, `ASTHNOW`, etc. These work only because
  CDC's name happens to match what OR uses.

For full canonicalization: rename ~1,500 concepts from CDC-uppercase to
canonical snake_case (e.g., `MARITAL` → `marital_status`,
`EDUCA` → `education`, `GENHLTH` → `general_health`). Mechanical
but bulk.

- [ ] **Decide policy**: rename all at once, rename domain-by-domain,
      or rename only when a non-CDC-aligned name appears that breaks
      the implicit assumption (lazy renaming). Recommended: **lazy**.
      Many constructs will keep their CDC names forever because CDC's
      name *is* canonical. The renames that matter are the ones where
      CDC and Oregon (or a future second state) disagree.

---

## Summary scoreboard

| Section | Effort | Blocks |
|---|---|---|
| 1. Quick cleanup | 30 min | nothing important |
| 2. State-tag retagging | 1 day | National pool integration |
| 3. Concept merges | 2-5 days (HIGH first) | Cleaner pulls, some downstream functions |
| 4. Recode rules | Multi-week, ongoing | Cross-year analyses for affected variables |
| 5. Values backfill | Half day, optional | Nothing; doc-only |
| 6. Package polish | Half day | Public release |
| 7. Open questions | 1 hour to answer; varies to implement | varies |
| 8a. Domain alignment | 1-2 days | Clean taxonomy for downstream consumers |
| 8b. Concept canonicalization | 2-5 days (or lazy/incremental) | Multi-state robustness |

The package is **fully functional** today — sections 2-4 and 8 are
quality-of-data work, not blockers for using `brfss_pull()`,
`brfss_clean_age()`, `brfss_clean_race()`, or `brfss_as_svy()`.
