# brfssTools codebook format specification

The brfssTools editor can display question text and value labels for
each cell, if a properly-formatted codebook CSV is present in your
data directory.

This document is the **format spec**. It tells you exactly what the
package needs to read your codebooks. How you produce that file is up
to you (see `codebook_prompt.md` for an AI-agent shortcut, or write
the file by hand if you prefer).

---

## File location

For a registered pool at `/path/to/data/`, codebooks live in a
sibling `documentation/` folder:

```
/path/to/data/
├── 2018.csv
├── 2019.csv
├── ...
├── 2024.csv
└── documentation/
    ├── OR_2018_codebook.csv
    ├── OR_2019_codebook.csv
    ├── ...
    └── OR_2024_codebook.csv
```

## File naming

One CSV file per year:

```
{state}_{year}_codebook.csv
```

- `{state}` matches the dataset name passed to `brfss_set_pool()`.
- `{year}` is the four-digit year.

Examples:
- `OR_2024_codebook.csv`
- `MA_2018_codebook.csv`
- `CDC_2024_codebook.csv` (for the National BRFSS file)

If the editor can't find a codebook file for a given (dataset, year),
the cell's edit panel shows the dropdown without question text — the
codebook is purely additive context.

## CSV schema

The file must have these columns, in any order:

| Column          | Type | Required | Description                              |
|-----------------|------|----------|------------------------------------------|
| variable_name   | chr  | yes      | Exact column name in the data file       |
| year            | int  | yes      | Year (must match the filename year)      |
| question_text   | chr  | no       | The survey question prompt               |
| value_labels    | chr  | no       | Semicolon-delimited "code=label" pairs   |
| notes           | chr  | no       | Any additional context                   |

### `variable_name`

The exact column name as it appears in the data file. Case-sensitive.
Preserve any leading underscores (`_RACE` not `RACE`).

### `year`

Four-digit year as an integer. Every row's `year` should match the
filename's year — it's redundant but the loader checks it.

### `question_text`

The survey question text as a single string. Multi-line text in the
source codebook should be flattened to a single line (replace
newlines with spaces; collapse runs of whitespace).

For calculated variables that don't have an associated question, this
can be a brief description of what the variable computes (e.g.,
"Calculated: imputed race based on respondent answers to MRACE1 and
HISPANC2"). Or leave it blank.

### `value_labels`

Semicolon-delimited "code=label" pairs:

```
1=Yes;2=No;7=Don't know/Not sure;9=Refused
```

Rules:
- One pair per response code
- `code=label` separated by `=`
- Pairs separated by `;`
- Trim whitespace around each part
- For continuous variables (e.g., a count of days), describe the range
  using the same syntax: `1-30=Number of days;88=None;77=Don't
  know/Not sure;99=Refused`
- For variables with no value labels (continuous, free text), leave blank
- Always preserve "Don't know" / "Not sure" / "Refused" codes — these
  matter for analysis

### `notes`

Optional free text. Use for:
- Skip patterns ("Asked only of respondents 18+")
- Cross-references ("See also: PHYSHLTH")
- Module identification ("Optional module: Adverse Childhood
  Experiences, asked in OR only in 2017, 2019, 2021, 2023")
- Sentinel values ("BLANK = not asked")

## Example

```csv
variable_name,year,question_text,value_labels,notes
GENHLTH,2024,"Would you say that in general your health is...","1=Excellent;2=Very good;3=Good;4=Fair;5=Poor;7=Don't know/Not sure;9=Refused",
PHYSHLTH,2024,"Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?","1-30=Number of days;88=None;77=Don't know/Not sure;99=Refused",
MENTHLTH,2024,"Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?","1-30=Number of days;88=None;77=Don't know/Not sure;99=Refused",
_AGE_G,2024,"Calculated: six-level imputed age category","1=18-24;2=25-34;3=35-44;4=45-54;5=55-64;6=65 or older",Calculated variable
```

## Validation

The package validates on read:

- All required columns present
- `year` values all match the filename year
- `variable_name` values are non-empty and unique within the file
- `value_labels` parses as `{code}={label}` pairs (warns on malformed
  rows but doesn't fail)

To test a codebook before relying on it:

```r
brfss_load_codebook("OR", 2024)
# Returns a tibble; reports any validation warnings.
```
