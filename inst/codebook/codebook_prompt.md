# brfssTools codebook conversion prompt

The brfssTools editor displays question text and value labels for each
cell when a properly-formatted codebook CSV is available. This prompt
helps you convert your raw codebook (PDF, Word doc, scanned image,
spreadsheet — whatever you have) into the right format using an AI
agent like Claude or ChatGPT.

## How to use this prompt

1. Open your AI agent of choice in a browser or app.
2. Attach or paste your raw codebook for ONE year of data.
3. Copy the **prompt below** into the conversation, replace the year
   and dataset placeholders, and send.
4. The agent will return CSV content. Save it as
   `{dataset}_{year}_codebook.csv` in the `documentation/` folder
   alongside your data files.
5. Repeat per year. (Codebooks change year-to-year — don't combine.)
6. Validate in R:
   ```r
   brfss_load_codebook("{dataset}", {year})
   ```

If you have a many-year project, this is a good half-hour. If you
have any doubt about the conversion's accuracy, spot-check a handful
of variables against the raw codebook before relying on it for
analysis.

---

## The prompt

```
I have a BRFSS codebook for one year of data. I need you to convert
it into a structured CSV for the brfssTools R package.

OUTPUT REQUIREMENTS:

A single CSV with these columns, in this order:
  variable_name, year, question_text, value_labels, notes

Output as code (in a single ```csv block), nothing else. No preamble,
no explanation, no commentary.

ROW PER VARIABLE:

One row per variable in the codebook. Include EVERY variable, even
calculated/derived ones (those starting with underscore in CDC
codebooks, e.g., _RACE, _AGE80).

COLUMN RULES:

variable_name:
  Exact column name as it appears in the data file.
  CASE-SENSITIVE. Preserve leading underscores. Do not modify.
  Examples: GENHLTH, _RACE, MRACE1, SEXVAR

year:
  The four-digit year, same value for every row in this file.
  Use: {YEAR}

question_text:
  The survey question prompt as a single line. If the codebook shows
  multi-line text, collapse newlines to spaces and collapse multiple
  spaces to one. Trim leading/trailing whitespace.
  For calculated variables that don't have a question, write a brief
  description of what's being calculated (e.g., "Calculated: imputed
  race based on MRACE1 and HISPANC").
  If the variable's purpose is genuinely unclear or undocumented,
  leave blank.

value_labels:
  Semicolon-delimited "code=label" pairs:
    1=Yes;2=No;7=Don't know/Not sure;9=Refused

  Rules:
    - One pair per response code
    - code=label separated by =
    - Pairs separated by ;
    - Trim whitespace
    - For continuous variables (e.g., number of days), describe the
      range with the same syntax:
        1-30=Number of days;88=None;77=Don't know/Not sure;99=Refused
    - ALWAYS preserve "Don't know", "Not sure", and "Refused" codes —
      they matter for analysis. Common codes are 7/9 for two-digit
      vars; 77/99 for three-digit; 777/999 for four-digit, but match
      whatever the codebook actually shows.
    - If the variable is continuous/free-text/numeric range with no
      labeled values, leave blank.

notes:
  Free text. Use for:
    - Skip patterns ("Asked only of respondents 18+")
    - Module identification ("Adverse Childhood Experiences module")
    - Sentinel values ("BLANK = not asked")
    - Anything else useful for context
  If nothing useful to add, leave blank.

CSV FORMAT:

  - Comma-separated
  - Header row first
  - Quote any field containing comma, semicolon, quote, or newline
  - Use standard CSV double-quote escaping (replace " with "")
  - UTF-8 encoding

CONTEXT:

  - Dataset: {DATASET}
  - Year: {YEAR}

THE CODEBOOK:

[paste codebook text here, or attach the file]
```

---

## Tips for getting good results

**Long codebooks**: BRFSS codebooks are big — CDC's annual codebook is
hundreds of pages. If you hit context limits, split the codebook into
chunks (e.g., by section), run the prompt per chunk, then concatenate
the resulting CSVs (keeping only one header row).

**OCR'd PDFs**: If your codebook started as a scan and was OCR'd, expect
errors in unusual variable names, accented characters, and value
labels. Spot-check the output against a handful of variables before
trusting the whole file.

**Calculated variables**: CDC's calculated/derived variables (`_RACE`,
`_AGE80`, `_RFHLTH`, etc.) often live in their own section of the
codebook with no question text. The prompt instructs the agent to
include a description, but if your codebook's calculated section is
sparse, the resulting `question_text` may just say "Calculated".
That's fine.

**Multiple language versions**: If your state's codebook documents
both English and Spanish question text, use the English text for
`question_text`. Note the existence of the Spanish version in `notes`
if you want.

**Variable name oddities**: state codebooks sometimes have variables
that don't match what's in the data file:
- Trailing whitespace (`MENTHLTH ` instead of `MENTHLTH`)
- Underscore vs hyphen confusion (`_AGE-G` vs `_AGE_G`)
- Module suffixes added in the codebook but not the data
  (`MENTHLTH_OR` vs `MENTHLTH`)

The variable_name should match what's IN THE DATA FILE, not what's
printed in the codebook header. If you spot a mismatch, fix the
`variable_name` to match the data; you can note the discrepancy in
`notes`.

**Blank responses**: Some states use BLANK or empty cells to mean
"not asked of this respondent." Note this in `notes` rather than
inventing a code for it.

## Validation

After dropping the file in `documentation/`:

```r
brfss_load_codebook("OR", 2024)
```

This:
- Reads the CSV
- Checks all required columns are present
- Verifies `year` values match the filename year
- Checks `variable_name` values are unique
- Warns on malformed `value_labels` entries
- Returns the parsed tibble

If validation passes, the editor will show codebook context for cells
in that (dataset, year). If validation fails, the function reports
specific issues; fix and retry.

If the editor still doesn't show codebook context after a successful
load, click Reload in the editor header — the codebook is loaded once
per session.
