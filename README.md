# brfssTools 🛠️

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Working with longitudinal BRFSS (Behavioral Risk Factor Surveillance System) data is historically tedious. Variable names change, value codes shift (e.g., "1" means "Yes" in 2015, but "2" means "Yes" in 2016), and question text drifts. Furthermore, the transition to REAL-D demographic standards between 2021 and 2024 fractured race and ethnicity data into dozens of disparate array columns. And if you want to combine state-restricted data (e.g., Oregon) with the public national CDC files, the bookkeeping multiplies.

`brfssTools` is a unified harmonization engine that solves this. It provides a `tidycensus`-style workflow: register or download a pool of raw survey files, search a bundled master crosswalk, and pull harmonized concepts as a tidy tibble — across years, across datasets.

> See `vignette("brfssTools")` for a comprehensive walkthrough including a self-contained runnable example, the source-tag architecture, and the workflow for adding a new state dataset.

## Installation

```r
# install.packages("pak")
pak::pak("nfparsons/brfssTools")
```

For national CDC data support, also install `haven`:

```r
pak::pak("haven")
```

## Quick start: national CDC BRFSS

```r
library(brfssTools)

# 1. Pull a few years of national LLCP data from CDC into the user cache.
#    This is a one-time setup per machine, per year.
brfss_download(2020:2023)
#> Downloading BRFSS 2020 from CDC...
#>   -> ~/.cache/R/brfssTools/LLCP2020.XPT
#> ...
#> Indexed 4 file(s) for dataset 'National'.

# 2. Load the master crosswalk filtered to national-applicable rules
cw <- brfss_crosswalk(dataset = "National", years = 2020:2023)

# 3. Pull harmonized concepts. Filter by state if you only need a few.
my_data <- brfss_pull(
  cw,
  concept_ids = c("survey_weight", "psu", "strata", "fmd"),
  dataset     = "National",
  years       = 2020:2023,
  states      = c("OR", "WA", "ID"),   # USPS or FIPS, your choice
  id_cols     = "SEQNO",
  output      = "wide"
)
```

## Quick start: state-restricted data (BYOD)

For Oregon and other state-restricted data, point the package at your secure data directory. Files can be `.csv` or `.xpt` — year is auto-detected from filename or the `SEQNO` column.

```r
brfss_set_pool("OR", "Z:/Secure_Data/BRFSS/Raw_Files")

cw <- brfss_crosswalk(dataset = "OR", years = 2018:2023)

my_data <- brfss_pull(
  cw,
  concept_ids = c("survey_weight", "psu", "strata", "multnomah_flag", "fmd"),
  dataset     = "OR",
  id_cols     = "SEQNO",
  output      = "wide"
)
```

## The master concept map

There is one crosswalk for everything. Each rule in the `concept_map` carries a `source` tag that controls where it applies:

| `source` value | Meaning                                                      |
|----------------|--------------------------------------------------------------|
| `"core"`       | Applies to every dataset (national LLCP and any state)       |
| `"OR"`         | Oregon state-added items only                                |
| `"WA"`, `"CA"`, ... | (Future) State-added items for that state                  |

When you call `brfss_pull(..., dataset = "National")`, only `core` rules are applied. When you call `brfss_pull(..., dataset = "OR")`, both `core` and `OR` rules are applied — so Oregon's state-added questions show up alongside the LLCP core. Adding a new state means tagging that state's rules with the state code; no schema changes, no parallel maps.

If you have a legacy `concept_map` CSV with a `survey` column (values `"BRFSS"`), it loads transparently — the column is renamed to `source` and values are coerced to `"core"` on the fly.

## Searching the crosswalk

Not sure what a variable was called in 2016 vs 2023? Search the crosswalk by concept name, raw question text, or raw variable name:

```r
brfss_search(cw, "asthma")
brfss_search(cw, "ASTHNOW", scope = "raw_var")
```

## Untangling race and ethnicity

Between 2012 and 2024, BRFSS shifted from basic summary columns to highly granular REAL-D arrays (e.g., `RE1` through `RE10`). The `brfss_race()` helper scans across all historical and modern race columns to derive a single, standardized demographic variable.

```r
demos <- brfss_pull(
  cw, c("race_array_1", "race_array_2", "hisp_flag"),
  dataset = "OR", output = "wide"
)

# Strict CDC 8-level mutually exclusive standard
demos |> brfss_race(standard = "cdc")

# High-level minimum categories (e.g., White NH vs. BIPOC)
demos |> brfss_race(standard = "fewest")

# Full REAL-D granularity (binary indicator columns)
demos |> brfss_race(standard = "reald")
```

## Cache directory

National downloads land in [`brfss_cache_dir()`](R/download.R) — the OS-appropriate user cache path provided by `tools::R_user_dir()`. You can override per-call with `brfss_download(years, dir = "...")`, or relocate by running `brfss_download()` once with a custom `dir` and pointing future sessions at it via `brfss_set_pool("National", "...")`.

## How it works under the hood

The engine is driven by a `concept_map.csv` bundled inside the package. This map links raw variables (like `CTYANSI` or `region_2016`) to standard concepts (like `multnomah_flag`) and provides the recode logic (`051=1; 51=1; 1=1`).

If a rule needs to be updated, it's updated centrally in the map — instantly fixing the logic for all future pulls.
