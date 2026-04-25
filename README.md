# brfssTools 🛠️

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
Working with longitudinal Oregon BRFSS (Behavioral Risk Factor Surveillance System) data is historically tedious. Variable names change, value codes shift (e.g., "1" means "Yes" in 2015, but "2" means "Yes" in 2016), and question text drifts. Furthermore, the transition to REAL-D demographic standards between 2021 and 2024 fractured race and ethnicity data into dozens of disparate array columns.

`brfssTools` is a unified harmonization engine that solves this. It allows analysts to effortlessly query, load, and harmonize restricted Oregon BRFSS data across multiple years using a simple, `tidycensus`-like workflow.

> **Note:** Because BRFSS microdata is restricted, this package **does not** include the raw data. It relies on a "Bring Your Own Data" (BYOD) architecture.

## Installation

You can install the development version of `brfssTools` from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("nfparsons/brfssTools")
```

## The "Bring Your Own Data" (BYOD) Workflow

Because you cannot share restricted OHA data, `brfssTools` acts purely as a computation and harmonization engine.

To use it, simply place your raw yearly Oregon BRFSS files (saved as `.csv` files) into a single secure local or network directory. The files do not need a specific naming convention; the package will auto-detect the survey year based on the internal `SEQNO` column.

### 1. Set Your Data Pool
Tell the package where your raw data lives. You only need to do this once per session.

```r
library(brfssTools)

# Point the package to your secure folder of raw .csv files
brfss_set_pool("Z:/Secure_Data/BRFSS/Raw_Files")
```

### 2. Load the Crosswalk
The package relies on an internal "Rosetta Stone" called the concept map. This map translates messy, drifting raw variables into clean, standardized `concept_ids`.

```r
# Load the master crosswalk rules built into the package
cw <- brfss_load_crosswalk()

# Or, load rules strictly for specific years to save memory
cw_recent <- brfss_load_crosswalk(years = 2022:2024)
```

### 3. Search for Variables
Not sure what a variable was called in 2016 vs 2023? Search the crosswalk by concept name or raw question text.

```r
brfss_search(cw, "asthma")
```

### 4. Pull Harmonized Data
Instead of writing brittle `dplyr::case_when()` scripts to track column names across a dozen files, just ask for the `concept_ids` you want. The engine will silently fetch the correct files from your secure pool, apply the historical recode rules, handle survey skip logic (converting missing codes to `NA`), and return a clean dataset.

```r
# Pull harmonized mental health and geography data
my_data <- brfss_pull(
  cw = cw,
  concept_ids = c("survey_weight", "psu", "strata", "multnomah_flag", "fmd"),
  id_cols = "SEQNO",
  output = "wide"
)
```

### 5. Untangling the Race & Ethnicity Mess
Between 2012 and 2024, Oregon BRFSS shifted from basic summary columns to highly granular REAL-D arrays (e.g., `RE1` through `RE10`).

`brfssTools` includes a dedicated demographic helper that scans across all historical and modern race columns to derive a single, standardized demographic variable. You can tailor the output to your reporting needs:

```r
# Pull the raw race array concepts first
brfss_demographics <- brfss_pull(cw, c("race_array_1", "race_array_2", "hisp_flag"), output = "wide")

# Option A: Strict CDC 8-Level mutually exclusive standard
brfss_demographics |>
  brfss_race(standard = "cdc")

# Option B: High-level minimum categories (e.g., White NH vs. BIPOC)
brfss_demographics |>
  brfss_race(standard = "fewest")

# Option C: Full REAL-D granularity (creates specific binary indicator columns)
brfss_demographics |>
  brfss_race(standard = "reald")
```

## How It Works Under the Hood
The engine is driven by a `concept_map.csv` file bundled inside the package. This map links raw variables (like `CTYANSI` or `region_2016`) to standard concepts (like `multnomah_flag`) and provides the recode logic (`051=1; 51=1; 1=1`).

If a rule ever needs to be updated, it is updated centrally in the package's concept map, instantly fixing the logic for all future data pulls.
