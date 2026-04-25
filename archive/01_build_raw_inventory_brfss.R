# scripts/01_build_raw_inventory_brfss.R
#
# Build raw_inventory_brfss.csv and raw_values_brfss.csv from the
# 13 BRFSS codebooks (2012-2024) in data/codebooks/brfss/.
#
# Run from the project root:
#   Rscript scripts/01_build_raw_inventory_brfss.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

# --- Locate project root so this script is directory-invariant --------
# If run from project root (Rscript scripts/...), cwd IS the project root.
# If run interactively (Source button in RStudio), `here` package would be
# nicer but we avoid the dependency; we walk up to find the parsers file.
find_project_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/")
  for (up in c(".", "..", "../..")) {
    cand <- normalizePath(file.path(cwd, up), winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(cand, "R", "parsers.R"))) return(cand)
  }
  stop("Could not locate project root (no R/parsers.R found nearby).")
}
ROOT      <- find_project_root()
BRFSS_DIR <- file.path(ROOT, "data", "codebooks", "brfss")
OUT_DIR   <- file.path(ROOT, "data", "crosswalk")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Source parsers (also loads _utils.R).
source(file.path(ROOT, "R", "parsers.R"), chdir = TRUE)

# --- Dispatch: year -> (parser, label) -------------------------------
# Format groups (see README):
#   A: 2012-2015  parse_brfss_2012_2015
#   B: 2016       parse_brfss_2016
#   C: 2017-2024  parse_tidy_var_values
dispatch <- function(year) {
  if (year %in% 2012:2015) list(
    fn    = function(path, fname) parse_brfss_2012_2015(path, year, fname),
    label = "A"
  ) else if (year == 2016) list(
    fn    = function(path, fname) parse_brfss_2016(path, fname),
    label = "B"
  ) else if (year %in% 2017:2024) list(
    fn    = function(path, fname) parse_tidy_var_values(path, "BRFSS", year, fname),
    label = "C"
  ) else stop(sprintf("No parser registered for BRFSS %d", year))
}

# --- Run all years ---------------------------------------------------
years <- 2012:2024
inv_all <- list()
val_all <- list()

for (yr in years) {
  fname <- sprintf("OHA_BRFSS_%d_Data_Dictionary.xlsx", yr)
  path  <- file.path(BRFSS_DIR, fname)
  if (!file.exists(path)) {
    warning(sprintf("BRFSS %d: file not found at %s -- skipping", yr, path))
    next
  }
  d <- dispatch(yr)
  result <- d$fn(path, fname)
  inv_all[[length(inv_all) + 1]] <- result$inventory
  val_all[[length(val_all) + 1]] <- result$values
  cat(sprintf("BRFSS %d (%s): %5d vars, %5d value rows\n",
              yr, d$label, nrow(result$inventory), nrow(result$values)))
}

inv <- dplyr::bind_rows(inv_all) |> flag_duplicate_keys()
val <- dplyr::bind_rows(val_all)

cat(sprintf("\nBRFSS totals: inventory=%d, values=%d\n", nrow(inv), nrow(val)))

readr::write_csv(inv, file.path(OUT_DIR, "raw_inventory_brfss.csv"), na = "")
readr::write_csv(val, file.path(OUT_DIR, "raw_values_brfss.csv"),    na = "")

cat(sprintf("Wrote %s\nWrote %s\n",
            file.path(OUT_DIR, "raw_inventory_brfss.csv"),
            file.path(OUT_DIR, "raw_values_brfss.csv")))
