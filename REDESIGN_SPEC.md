# brfssTools — User-Anchored Crosswalk Redesign

**Status**: Spec, pre-implementation. Architectural reframe agreed
end-of-session April 30, 2026 (transcript: stage5b-vignettes).

---

## Why this redesign

The current 0.1.0 architecture treats CDC's variable list as the
canonical anchor. The crosswalk ships seeded with all 4,209 CDC
variables marked `unverified = 1`, and the user maps their state
columns onto that scaffold.

This puts the wrong thing in the center. Most users don't care about
"every CDC variable from 2012-2024" — they care about *their* state's
variables, and want to know how those line up across the years they
actually have data for.

The redesign anchors on **the user's most recent year of data**. The
user's variable list IS the concept list. CDC becomes one possible
state among many, not the source of truth.

---

## Core workflow (target)

1. User registers a data pool: `brfss_set_pool("OR", "Z:/data")`
2. User initializes config with drafting:
   `brfss_init_state("OR", state_codebook_path = "...", draft = TRUE)`
3. Package reads the most recent year's data, lists its variables,
   creates one row per variable per year (state_var = NA for years
   other than the most recent)
4. User opens the editor. For each concept (a most-recent-year variable),
   they fill in equivalents for older years via searchable dropdowns.
   Each cell can be flagged as "calculated" — when toggled, the cell
   stores inline YAML instead of a column name.
5. User pulls data: `brfss_pull(concepts = "MENTHLTH", years = 2018:2024)`
   — the package resolves each year's cell as either a column read or
   a calculation eval.

There's no separate "demographics" workflow. Demographics are concepts
like any other; if they're computed (race from MRACE1+HISPANC, for
example), the user marks those cells as calculated and writes a small
YAML expression. Templates remain available as opt-in convenience.

---

## Schema changes

### Crosswalk schema (v0.2.0)

Adds `is_calculated`, `calculation_yaml`, removes `cdc_var` (CDC has no
special status now), removes `is_primary` (one mapping per concept-year
unless the user explicitly creates alternates):

```
concept_id          chr   R-friendly name; default = most-recent-year var
year                int
state_var           chr   Column name in this year's data file (NA if calculated or unmapped)
is_calculated       int   0 = column lookup, 1 = use calculation_yaml
calculation_yaml    chr   Inline YAML for this year's calculation (NA unless is_calculated)
domain              chr   Top-level domain ("Demographics", "Behavioral Health", etc.)
subdomain           chr   Optional finer grouping
unverified          int   1 if drafted-not-yet-confirmed; 0 once user verifies
notes               chr
```

Compared to v0.1.0:
- Drops `cdc_var` (concepts aren't CDC-anchored anymore)
- Drops `is_primary` (eliminate the secondary-mapping concept; if you
  need it, that's an alternate concept_id)
- Drops `source` and `score` (provenance fields from the matcher pipeline)
- Adds `is_calculated`, `calculation_yaml`
- Adds `domain` and `subdomain` (was implicit via taxonomy.csv lookup)

### Migration from v0.1.0

A one-shot `brfss_migrate_crosswalk_to_v2()` function:
- Drops `cdc_var`, `is_primary`, `source`, `score`
- Adds new columns with default values (`is_calculated = 0`,
  `calculation_yaml = NA`, `unverified = 0` for already-mapped rows)
- Pulls `domain`/`subdomain` from current taxonomy.csv match
- Backs up old crosswalk to `.crosswalk_v01.csv.bak`

### What gets removed

- `cdc_seed.csv` from `inst/extdata/`
- `cdc_only.csv` from `inst/extdata/` (no longer relevant)
- `state_only.csv` from config dir (subsumed by main crosswalk)
- `pending.csv` from config dir (Pass 1 matcher output, no longer relevant)

### What stays

- `cdc_codebook.csv` and `cdc_calculated_vars.csv` stay as **reference**
  data — the editor can let the user consult them to see what CDC's
  variables look like. Not used to drive the crosswalk.
- `taxonomy.csv` stays — used by the domain auto-assigner.
- `state_codebook.csv` stays — required for the dropdown in the editor.

---

## New / changed functions

### `brfss_init_state(state, state_codebook_path, path, overwrite, draft = FALSE)`

Behavior change:
- `seed_from_cdc` argument removed
- New `draft = FALSE` argument
- If `draft = TRUE` AND a pool is registered for `state`, calls
  `brfss_draft_crosswalk()` automatically as part of init
- If `draft = TRUE` but no pool registered, errors with helpful message
  ("Register a pool first via brfss_set_pool() then re-run.")

### `brfss_draft_crosswalk(dataset = NULL, path = NULL, overwrite_existing = FALSE)`

NEW. Reads the most recent year's data file from the registered pool,
extracts the variable list, creates a draft crosswalk:

- One concept_id per variable in the most recent year
- concept_id = sanitize_to_r_name(variable_name)
- For each concept, one row per year in the pool:
  - In the most-recent year: `state_var = variable_name`, `unverified = 0`
  - In older years: `state_var = NA`, `unverified = 1`
- `is_calculated = 0` for all
- Domain auto-assignment via taxonomy.csv (see below)

If a crosswalk already exists, this errors unless `overwrite_existing = TRUE`.
For adding new years to an existing crosswalk, use `brfss_redraft_crosswalk()`.

### `brfss_redraft_crosswalk(dataset = NULL, path = NULL)`

NEW. Idempotent extend: keeps existing concept_ids and mappings, but:
- Adds rows for any new years detected in the pool that aren't yet
  represented (state_var = NA, unverified = 1)
- For variables in the most recent year that don't yet have a concept_id,
  creates new concept_ids for them

This is the function users run after dropping a 2025 data file in.

### Sanitization rule

Variable name → concept_id:
- Strip leading `_`: `_RACE` → `RACE`
- Replace any non-alphanumeric-or-underscore with `_`
- Collapse runs of `_` to single `_`
- If the result starts with a digit, prepend `X`
- Truncate to 63 chars

CDC's calculated vars (`_RACE`, `_AGE80`, `_RFHLTH`, etc.) all become
their stripped form. We need to handle the rare collision case where
both `RACE` (raw) and `_RACE` (calculated) exist in the same year —
the calculated version becomes `X_RACE` to disambiguate.

### Domain auto-assignment

`taxonomy.csv` has columns `concept_id, domain, subdomain` (today;
reflects CDC variable names). On `brfss_draft_crosswalk()`:

- For each new concept_id, look up in taxonomy.csv:
  - Direct match? Use that domain/subdomain.
  - No match? Try matching the un-sanitized variable name against the
    taxonomy's `concept_id` column.
  - Still no match? Set `domain = "Unassigned"`, `subdomain = NA`.

The editor is a **single view**: one heatmap organized by domain, with
"Unassigned" as a permanent section pinned to the bottom. The user
scrolls down to find unassigned concepts, assigns each one a domain,
and the row pops into the correct section.

**Unassigned concepts are not variable-mappable.** The cell-edit panel
shows "Assign a domain to this concept first" instead of the dropdown
or YAML editor. This forces the user to think about taxonomy before
mapping.

There is no separate State-anchored, CDC-anchored, or Demographics
tab. The crosswalk IS state-anchored now (the user's data drives it),
and demographics aren't architecturally special.

### Per-cell editing in the GUI

Each cell in the heatmap (a concept × year) is editable. The right-hand
edit panel shows for the selected (concept, year):

```
Concept:   MENTHLTH
Year:      2018

[ ] Calculated

Variable name (from 2018 data file):
  [ MENTHLTH                            ▼ ]   ← searchable dropdown
                                                of 2018's variable list

  Question text (from codebook, if available):
    "Now thinking about your mental health, which includes stress,
     depression, and problems with emotions, for how many days during
     the past 30 days was your mental health not good?"

  Value labels:
    1-30: Number of days
    88: None
    77: Don't know / Not sure
    99: Refused

[ Verify ]   [ Reset ]
```

When **Calculated** is checked, the dropdown is replaced with a YAML
text editor:

```
Concept:   race
Year:      2019

[x] Calculated

Calculation YAML (for this year):

  inputs:
    mrace: _MRACE1
    hisp:  _HISPANC

  levels:
    - value: 1
      label: "White only, NH"
      when: mrace == 1 & hisp == 2
    - value: 2
      ...

[ Show example ▼ ]   [ Verify ]   [ Reset ]
```

The YAML stored in `calculation_yaml` is just the body — `output_column`,
`type`, `description` are added at runtime when the cell is evaluated
(so users don't write boilerplate they don't need).

### Codebook documentation reader

If the registered pool's directory has a sibling `documentation/`
subfolder, the editor shows codebook context for the selected cell.
Format spec for the codebook CSV (one per year):

```
Filename:    {state}_{year}_codebook.csv
Location:    {pool_dir}/documentation/

Columns:
  variable_name      chr  required
  year               int  required (must match filename)
  question_text      chr  optional, the survey question prompt
  value_labels       chr  optional, semicolon-delimited "code=label"
  notes              chr  optional, any additional context
```

The package ships an `inst/codebook_prompt_template.md` that users can
hand to an AI agent along with their codebook PDF/Word doc to produce
this CSV. (Not built yet — written in Stage 2 of implementation.)

A `brfss_load_codebook(year, dataset)` reads the CSV. Editor calls
this on cell selection; if missing or unparseable, just skips the
codebook context display.

---

## Implementation stages

Roughly four sessions of work:

### Stage A: Schema + draft + migrate (~1 session)

- Schema migration function (`brfss_migrate_crosswalk_to_v2`)
- New schema enforced in `cw_load`/`cw_save`
- `brfss_draft_crosswalk()` and `brfss_redraft_crosswalk()`
- Sanitization helper
- Domain auto-assigner using existing taxonomy.csv

Deliverable: from the console, can init+draft a fresh crosswalk
end-to-end. Editor still uses old views (will break some — that's OK;
we'll fix in Stage B).

### Stage B: Editor refactor (~1.5 sessions)

- Strip out the existing view-mode radio buttons (CDC-anchored /
  State-anchored / Demographics). Single view only.
- Single heatmap organized by domain; "Unassigned" pinned to the bottom.
- Per-cell edit panel with searchable dropdown of that year's variables
- Calculated toggle + inline YAML editor with example help
- Cells in concepts where domain == "Unassigned" show "Assign a domain
  first" message instead of the editor.
- Codebook context display (if codebook CSV present)
- **Remove**: all Demographics-tab code, all template-related UI/observers,
  state-anchored mode logic, view_mode reactivity, CDC-anchored mode logic.

Deliverable: editor fully usable for the new flow.

### Stage B-cleanup: Archive demographics/templates code (~0.5 session, parallel with B)

Code we built but won't use in v0.2.0 gets archived rather than deleted —
in case we want to revisit later. Move (don't delete):

- `R/demographics_setup.R` → `inst/archive/demographics_setup.R`
- `inst/extdata/transformations/templates/` (all 18 files) →
  `inst/archive/transformations/templates/`
- Any related test files → `inst/archive/tests/`

Then strip from the active package:

- Remove `brfss_setup_demographic()`, `brfss_list_demographic_templates()`,
  `brfss_demographic_status()` exports from NAMESPACE
- Remove all references in package help, NEWS (note as "moved to archive"),
  TODO, vignette
- Remove any roxygen `@seealso` cross-refs to the archived functions

Files in `inst/archive/` ship with the package install but aren't loaded.
They're a frozen reference — if we want them back, they're a `git mv`
plus re-export away.

Deliverable: smaller, less confused package surface, with the previous
work preserved as a recoverable reference.

### Stage C: Codebook reader (~0.5 session)

- `brfss_load_codebook()` parses the CSV format
- Codebook prompt template written and shipped in `inst/`
- Editor surfaces context

Deliverable: users with codebooks get rich context; users without
get the basic dropdown.

### Stage D: Pull + transformation runtime (~0.5 session)

- `brfss_pull` resolves cells: column lookup OR inline YAML eval
- Inline YAML uses the existing `categorical_map` / `passthrough`
  runtime — just parsed from a string instead of loaded from a file
- The `transformations/` folder concept is **dropped**. Calculation YAML
  lives only in the crosswalk's `calculation_yaml` column.
- Existing `R/categorical_map.R` runtime stays; only the loader changes
  (parse from string, not from file).

Deliverable: end-to-end pull works with both column and calculated cells.

### Migration / docs (~0.5 session)

- Update vignette, README, NEWS, package help
- Run the migration function on user's existing v0.1.0 crosswalk
- Smoke test against demo data
- Bump to 0.2.0

---

## Things deferred / questions for later

1. **Multiple state_vars per concept_year**: in the old schema,
   `is_primary` allowed alternate mappings (e.g., a concept could match
   both `MRACE1` and `MRACE_NEW` in the same year, with `MRACE1` as
   primary). The new schema is one-to-one. If users need alternates,
   they create a new concept_id (`race_alt`). Is this OK or do we need
   a "fallback chain" mechanism?

2. **`brfss_pull` for National BRFSS**: with no CDC seed, a pure
   National user has to register CDC files as their pool, draft the
   crosswalk from CDC's 2024 file, then have CDC variable names as
   their concept_ids. This works but feels heavyweight. We could ship
   a pre-drafted CDC crosswalk as `inst/extdata/cdc_drafted.csv` for
   users to import via `brfss_import_config()`. (Not "the canonical
   anchor" — just "a starter for National-only users.")

3. **Multiple concept_ids in the same year mapping to the same
   state_var**: can `MENTHLTH_2018` be both the concept_id `MENTHLTH`
   AND `mental_health_days`? Probably yes — the schema doesn't prevent
   it. But this opens questions about which one `brfss_pull("MENTHLTH")`
   resolves to when both exist.

---

## Decisions log

| # | Decision | Date |
|---|---|---|
| 1 | Drop CDC seed entirely; user data is the source of truth | 2026-04-30 |
| 2 | Per-cell calculation: new `calculation_yaml` column on each row | 2026-04-30 |
| 3 | Domain auto-assigner using taxonomy.csv; Unassigned bucket for non-matches | 2026-04-30 |
| 4 | Build codebook documentation reader + format spec | 2026-04-30 |
| 5 | Both triggers for drafting: `brfss_init_state(draft=TRUE)` and `brfss_draft_crosswalk()` standalone | 2026-04-30 |
| 6 | Codebook files: filename detection convention, CSV format with variable_name/year/question_text/value_labels/notes | 2026-04-30 |
| 7 | concept_id default = sanitize(most_recent_year_var); user can rename freely | 2026-04-30 |
| 8 | Editor is a single view (no CDC/State/Demographics tabs) | 2026-05-01 |
| 9 | Unassigned concepts are pinned to the bottom of the heatmap; not variable-mappable until assigned a domain | 2026-05-01 |
| 10 | Archive (not delete) the Demographics tab and 18 templates; demographics are not architecturally special in v0.2.0 but the code stays available in inst/archive/ | 2026-05-01 |
| 11 | The `transformations/` folder concept is dropped; calculation YAML lives only in the crosswalk's `calculation_yaml` column | 2026-05-01 |
