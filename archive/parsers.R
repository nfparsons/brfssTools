# R/parsers.R
#
# Single entry point: source this file to load all parsers and the
# shared utilities. The build scripts in scripts/ use this.

parsers_dir <- file.path(dirname(sys.frame(1)$ofile), "parsers")
# Fallback if called via source(... chdir = TRUE) from elsewhere:
if (!dir.exists(parsers_dir)) {
  parsers_dir <- "R/parsers"
}

# Utilities must come first (they define helpers used by the parsers).
source(file.path(parsers_dir, "_utils.R"), local = FALSE)

# Parsers (alphabetical).
source(file.path(parsers_dir, "parse_brfss_2012_2015.R"), local = FALSE)
source(file.path(parsers_dir, "parse_brfss_2016.R"),      local = FALSE)
source(file.path(parsers_dir, "parse_oht_2011.R"),        local = FALSE)
source(file.path(parsers_dir, "parse_oht_2013.R"),        local = FALSE)
source(file.path(parsers_dir, "parse_oht_2015_2017.R"),   local = FALSE)
source(file.path(parsers_dir, "parse_shs_2024_block.R"),  local = FALSE)
source(file.path(parsers_dir, "parse_tidy_var_values.R"), local = FALSE)
