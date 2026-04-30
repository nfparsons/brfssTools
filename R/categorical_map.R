# R/categorical_map.R
#
# Declarative categorical-map transformations defined in YAML.
# A categorical_map turns one or more raw input columns into a single output
# column via an ordered list of (value, label, when) rules. First match wins.
#
# Schema (transformations/<name>.yaml):
#   output_column: <character>      # required; matches the file basename
#   type: categorical_map           # required; literal string
#   description: <character>        # optional; multi-line OK
#   inputs:                         # optional; alias -> raw column name
#     mrace: _MRACE1
#     hisp:  _HISPANC
#   levels:                         # required; ordered list (first match wins)
#     - value: <integer or string>
#       label: <character>
#       when:  <R expression as string>
#   by_year:                        # optional; year-range overrides
#     "2018-2024":
#       inputs: { mrace: _MRACE1, hisp: _HISPAN3 }
#
# 'when' expressions are evaluated as R code. They may reference either:
#   - alias names (if `inputs` defines them), or
#   - raw column names directly.
# The runtime resolves both: aliases are bound first (taking precedence over
# raw columns of the same name).

# ============================================================================
# Public: setup helpers
# ============================================================================

#' Set up a declarative categorical-map transformation
#'
#' Creates `<config>/transformations/<name>.yaml` with a starter template and
#' opens it for editing. The YAML file is the source of truth; the runtime
#' reads it on every `brfss_pull()` call.
#'
#' Optionally, `brfss_render_transformation_code()` can generate a
#' human-readable R companion file (`<name>.R`) that mirrors the YAML logic.
#' Useful for documentation, methods sections, or audit purposes.
#'
#' @param name   The transformation name. Becomes the output column name.
#' @param template Optional template name. `"race"` is supplied; everything
#'   else gets a generic stub.
#' @param path   Optional config path override.
#' @param edit   Logical; opens the file in editor when interactive.
#' @return The YAML file path, invisibly.
#' @export
brfss_setup_categorical_map <- function(name,
                                        template = NULL,
                                        path = NULL,
                                        edit = interactive()) {
  if (missing(name) || !is.character(name) || length(name) != 1L ||
      !nzchar(name)) {
    stop("`name` is required.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    stop("`name` must be R-friendly (letters/digits/underscores, ",
         "no leading digit).", call. = FALSE)
  }

  cfg <- brfss_config_path(path, must_exist = FALSE)
  trans_dir <- file.path(cfg, "transformations")
  if (!dir.exists(trans_dir)) {
    dir.create(trans_dir, recursive = TRUE, showWarnings = FALSE)
  }

  fp <- file.path(trans_dir, paste0(name, ".yaml"))

  if (file.exists(fp)) {
    message(sprintf("Categorical map '%s' already exists. Opening for editing.", name))
  } else {
    body <- .brfss_categorical_map_template(name, template)
    writeLines(body, fp)
    message(sprintf("Created categorical map '%s' at:\n  %s", name, fp))
  }

  if (edit) utils::file.edit(fp)
  invisible(fp)
}

#' Set up the race categorical map (CDC-style)
#'
#' Convenience wrapper around `brfss_setup_categorical_map("race", "race")`.
#' The shipped template implements CDC's `_RACE` calculated variable: 7 NH
#' single-race categories + Hispanic + don't-know/refused.
#'
#' @inheritParams brfss_setup_categorical_map
#' @return The YAML file path, invisibly.
#' @export
brfss_setup_race_map <- function(path = NULL, edit = interactive()) {
  brfss_setup_categorical_map("race", template = "race",
                              path = path, edit = edit)
}

# ============================================================================
# Public: load, validate, render
# ============================================================================

#' Load and validate a categorical-map YAML
#'
#' Returns a parsed list with the `levels` normalized for runtime use.
#' Errors verbosely if the YAML is malformed or missing required fields.
#'
#' @param fp Path to the YAML file.
#' @return A list with `output_column`, `type`, `description`, `inputs`,
#'   `levels` (normalized), and `by_year` (if present).
#' @export
brfss_load_categorical_map <- function(fp) {
  if (!file.exists(fp)) {
    stop("Categorical map file not found: ", fp, call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Reading categorical maps requires the 'yaml' package. ",
      "Install with: install.packages(\"yaml\")",
      call. = FALSE
    )
  }
  spec <- tryCatch(
    yaml::read_yaml(fp),
    error = function(e) {
      stop("Failed to parse YAML at ", fp, ":\n  ", conditionMessage(e),
           call. = FALSE)
    }
  )

  # Required fields
  if (is.null(spec$output_column) || !nzchar(spec$output_column)) {
    stop("Categorical map at ", fp, " missing required `output_column`.",
         call. = FALSE)
  }
  if (is.null(spec$type) || spec$type != "categorical_map") {
    stop("Categorical map at ", fp,
         " must have type: categorical_map. Found: ",
         spec$type %||% "<missing>", call. = FALSE)
  }
  if (is.null(spec$levels) || !is.list(spec$levels) || length(spec$levels) == 0L) {
    stop("Categorical map at ", fp, " must define at least one level.",
         call. = FALSE)
  }

  # File basename should match output_column
  expected_name <- sub("\\.ya?ml$", "", basename(fp))
  if (!identical(expected_name, spec$output_column)) {
    warning(sprintf(
      "Categorical map filename '%s' does not match output_column '%s'. ",
      expected_name, spec$output_column),
      "brfss_pull() resolves transformations by filename, so this map will ",
      "be loaded as '%s'.", expected_name,
      call. = FALSE)
  }

  # Normalize levels
  spec$levels <- lapply(seq_along(spec$levels), function(i) {
    lvl <- spec$levels[[i]]
    if (is.null(lvl$value)) {
      stop("Level #", i, " in ", fp, " missing `value`.", call. = FALSE)
    }
    if (is.null(lvl$when) || !nzchar(lvl$when)) {
      stop("Level #", i, " (value=", lvl$value, ") in ", fp,
           " missing `when` expression.", call. = FALSE)
    }
    list(
      value = lvl$value,
      label = lvl$label %||% as.character(lvl$value),
      when  = lvl$when
    )
  })

  # Validate when expressions are parseable R
  for (i in seq_along(spec$levels)) {
    expr_str <- spec$levels[[i]]$when
    parsed <- tryCatch(parse(text = expr_str),
                       error = function(e) e)
    if (inherits(parsed, "error")) {
      stop("Level #", i, " (value=", spec$levels[[i]]$value, ") in ", fp,
           " has unparseable `when` expression:\n  ", expr_str, "\n  Error: ",
           conditionMessage(parsed), call. = FALSE)
    }
  }

  spec$inputs <- spec$inputs %||% list()
  spec
}

#' Render a categorical-map YAML as readable R code
#'
#' Generates an R file that mirrors the YAML logic using `dplyr::case_when()`.
#' Optionally writes the rendered code to `<name>.R` alongside the YAML.
#'
#' Two intended uses:
#' \enumerate{
#'   \item Documentation. Methods sections / IRB packets benefit from a
#'     human-readable code listing alongside the YAML spec.
#'   \item Audit. The generated code makes it easy to confirm the YAML
#'     produces what you expect.
#' }
#'
#' Note: brfss_pull() always uses the YAML at runtime, never the generated
#' .R file. The .R is for reading, not execution.
#'
#' @param name  Transformation name (the file basename, e.g., "race").
#' @param path  Optional config path override.
#' @param save  Logical. If TRUE, writes `<name>.R` next to `<name>.yaml`.
#'   If FALSE (default), returns the rendered code as a character vector.
#' @return Character vector of R code lines (invisibly if save=TRUE).
#' @export
brfss_render_transformation_code <- function(name, path = NULL, save = FALSE) {
  cfg <- brfss_config_path(path, must_exist = FALSE)
  fp_yaml <- file.path(cfg, "transformations", paste0(name, ".yaml"))
  if (!file.exists(fp_yaml)) {
    stop("No YAML transformation found at: ", fp_yaml, call. = FALSE)
  }
  spec <- brfss_load_categorical_map(fp_yaml)
  code <- .render_categorical_map_as_r(spec, name)

  if (save) {
    fp_r <- file.path(cfg, "transformations", paste0(name, ".R"))
    writeLines(code, fp_r)
    message("Rendered R code saved to: ", fp_r)
    return(invisible(code))
  }

  code
}

# ============================================================================
# Internal: runtime evaluator
# ============================================================================

# Apply a categorical map to a per-year data frame.
# Returns the data frame with an added/replaced column matching spec$output_column.
.brfss_apply_categorical_map <- function(.data, spec, year) {
  # Resolve which inputs apply this year.
  inputs <- .resolve_inputs_for_year(spec, year)

  # Build an evaluation environment that combines:
  #   - aliases (alias_name -> column vector), with priority
  #   - raw columns (raw_name -> column vector) as fallback
  eval_env <- new.env(parent = baseenv())

  # Bind raw columns first
  for (col in names(.data)) {
    assign(col, .data[[col]], envir = eval_env)
  }
  # Bind aliases (override raw if name collision)
  missing_inputs <- character(0)
  for (alias in names(inputs)) {
    raw_col <- inputs[[alias]]
    if (raw_col %in% names(.data)) {
      assign(alias, .data[[raw_col]], envir = eval_env)
    } else {
      missing_inputs <- c(missing_inputs, sprintf("%s -> %s", alias, raw_col))
    }
  }
  if (length(missing_inputs) > 0L) {
    warning(sprintf(
      "Categorical map '%s' for year %d: missing input columns: %s. ",
      spec$output_column, year, paste(missing_inputs, collapse = ", ")),
      "Output will be NA for this year.",
      call. = FALSE)
  }

  # Evaluate each level's `when` expression and stack results.
  out <- rep(NA, nrow(.data))
  # Use the first level's value to infer output type (integer vs character)
  out_type <- if (length(spec$levels) &&
                   is.numeric(spec$levels[[1]]$value)) "integer" else "character"
  if (out_type == "integer") {
    out <- as.integer(out)
  } else {
    out <- as.character(out)
  }

  for (lvl in spec$levels) {
    cond <- tryCatch(
      eval(parse(text = lvl$when), envir = eval_env),
      error = function(e) {
        stop(sprintf(
          "Categorical map '%s' for year %d: error evaluating level value=%s: %s",
          spec$output_column, year, lvl$value, conditionMessage(e)),
          call. = FALSE)
      }
    )
    if (length(cond) != nrow(.data)) {
      stop(sprintf(
        "Categorical map '%s' level value=%s: condition returned length %d, ",
        spec$output_column, lvl$value, length(cond)),
        sprintf("expected %d (nrow of data).", nrow(.data)),
        call. = FALSE)
    }
    cond[is.na(cond)] <- FALSE
    # First-match-wins: only assign where output is still NA
    pick <- cond & is.na(out)
    if (out_type == "integer") {
      out[pick] <- as.integer(lvl$value)
    } else {
      out[pick] <- as.character(lvl$value)
    }
  }

  .data[[spec$output_column]] <- out
  .data
}

# Determine which inputs apply for a given year.
# Order of precedence: by_year override > top-level inputs > {} (none).
.resolve_inputs_for_year <- function(spec, year) {
  by_year <- spec$by_year
  if (is.null(by_year) || length(by_year) == 0L) {
    return(spec$inputs %||% list())
  }
  # Look for a matching key
  for (key in names(by_year)) {
    if (.year_in_range(year, key)) {
      return(by_year[[key]]$inputs %||% spec$inputs %||% list())
    }
  }
  spec$inputs %||% list()
}

# Parse a year-range string like "2018-2024", "2012,2014", "2020"; check if year matches.
.year_in_range <- function(year, range_spec) {
  year <- as.integer(year)
  pieces <- strsplit(range_spec, ",", fixed = TRUE)[[1]]
  for (p in pieces) {
    p <- trimws(p)
    if (grepl("^\\d+\\s*-\\s*\\d+$", p)) {
      bounds <- as.integer(strsplit(p, "-", fixed = TRUE)[[1]])
      if (year >= bounds[1] && year <= bounds[2]) return(TRUE)
    } else if (grepl("^\\d+$", p)) {
      if (year == as.integer(p)) return(TRUE)
    }
  }
  FALSE
}

# ============================================================================
# Internal: code generation (YAML -> readable R)
# ============================================================================

.render_categorical_map_as_r <- function(spec, name) {
  out <- character(0)

  # Header / docstring
  out <- c(out,
    sprintf("# transformations/%s.R", name),
    "#",
    "# AUTO-GENERATED from the corresponding .yaml file.",
    "# DO NOT EDIT directly: changes to this file will be overwritten the next",
    "# time brfss_render_transformation_code() runs.",
    "#",
    "# Edit the .yaml instead, then re-render.",
    "#"
  )

  if (!is.null(spec$description) && nzchar(spec$description)) {
    desc_lines <- strsplit(spec$description, "\n", fixed = TRUE)[[1]]
    out <- c(out, paste0("# ", desc_lines), "#")
  }

  # Document the input mapping
  if (length(spec$inputs)) {
    out <- c(out, "# Input variable aliases:")
    for (alias in names(spec$inputs)) {
      out <- c(out, sprintf("#   %s -> %s", alias, spec$inputs[[alias]]))
    }
    out <- c(out, "#")
  }

  # Document the levels in CDC-style table form
  out <- c(out, "# Levels (first match wins):")
  for (lvl in spec$levels) {
    out <- c(out,
      sprintf("#   %s = %s", as.character(lvl$value), lvl$label),
      sprintf("#       when: %s", lvl$when)
    )
  }
  out <- c(out, "")

  # The function body
  out <- c(out, "library(dplyr)", "")

  out <- c(out, sprintf("%s <- function(.data, year) {", name))

  if (!is.null(spec$by_year) && length(spec$by_year)) {
    out <- c(out,
      "  # Year-aware input mapping",
      "  inputs <- list(")
    # Top-level
    if (length(spec$inputs)) {
      out <- c(out, "    default = list(")
      for (alias in names(spec$inputs)) {
        out <- c(out, sprintf('      %s = "%s",', alias, spec$inputs[[alias]]))
      }
      out[length(out)] <- sub(",$", "", out[length(out)])
      out <- c(out, "    ),")
    }
    for (yr_key in names(spec$by_year)) {
      yr_inputs <- spec$by_year[[yr_key]]$inputs
      if (!is.null(yr_inputs)) {
        out <- c(out, sprintf('    `%s` = list(', yr_key))
        for (alias in names(yr_inputs)) {
          out <- c(out, sprintf('      %s = "%s",', alias, yr_inputs[[alias]]))
        }
        out[length(out)] <- sub(",$", "", out[length(out)])
        out <- c(out, "    ),")
      }
    }
    out[length(out)] <- sub(",$", "", out[length(out)])
    out <- c(out, "  )", "")
    out <- c(out, "  # (Year-resolution logic omitted in rendered version;",
                  "  # see the YAML and runtime for full semantics.)", "")
  }

  # Bind aliases to columns in the data
  if (length(spec$inputs)) {
    out <- c(out, "  # Bind input aliases (using top-level inputs; year overrides")
    out <- c(out, "  # honored only by the YAML runtime).")
    for (alias in names(spec$inputs)) {
      raw <- spec$inputs[[alias]]
      out <- c(out, sprintf('  %s <- .data[["%s"]]', alias, raw))
    }
    out <- c(out, "")
  }

  # Build the case_when() expression
  out <- c(out, "  .data %>%")
  out <- c(out, sprintf("    mutate(%s = case_when(", spec$output_column))

  for (i in seq_along(spec$levels)) {
    lvl <- spec$levels[[i]]
    val_repr <- if (is.numeric(lvl$value)) {
      as.character(lvl$value)
    } else {
      sprintf('"%s"', lvl$value)
    }
    sep <- if (i == length(spec$levels)) "" else ","
    out <- c(out,
      sprintf("      %-50s ~ %s%s   # %s",
              lvl$when, val_repr, sep, lvl$label))
  }

  out <- c(out, "    ))", "}", "")

  out
}

# ============================================================================
# Templates
# ============================================================================

.brfss_categorical_map_template <- function(name, template = NULL) {
  if (!is.null(template) && template == "race") {
    return(.template_race_yaml())
  }
  .template_categorical_generic(name)
}

.template_categorical_generic <- function(name) {
  c(
    sprintf("# transformations/%s.yaml", name),
    "# A categorical_map transformation for brfss_pull().",
    "# Edit this file; brfss_pull() reads it on every call.",
    "",
    sprintf("output_column: %s", name),
    "type: categorical_map",
    "",
    "description: |",
    sprintf("  TODO: describe what '%s' represents and how it is computed.", name),
    "",
    "# Optional: alias raw column names so 'when' rules are year-portable.",
    "# Leave empty (`inputs: {}`) if you'd rather use raw column names directly.",
    "inputs: {}",
    "",
    "levels:",
    "  - value: 1",
    "    label: First category",
    "    when: TRUE   # TODO: real condition",
    "  - value: 9",
    "    label: Don't know / Refused",
    "    when: TRUE   # catches everything not assigned by earlier rules",
    ""
  )
}

.template_race_yaml <- function() {
  c(
    "# transformations/race.yaml",
    "#",
    "# CDC-style race/ethnicity calculated variable.",
    "# Mirrors CDC's _RACE: derived from _MRACE1 (multi-race indicator) and",
    "# _HISPANC (Hispanic ethnicity).",
    "#",
    "# Edit this file to match your project's race definitions.",
    "# Re-run brfss_render_transformation_code(\"race\", save=TRUE) to refresh",
    "# the readable .R companion file (audit / docs / methods sections).",
    "",
    "output_column: race",
    "type: categorical_map",
    "",
    "description: |",
    "  CDC-style race/ethnicity calculated variable.",
    "  All Hispanic respondents are coded as Hispanic regardless of race.",
    "  Non-Hispanic respondents are coded by their _MRACE1 value.",
    "  Don't-know/refused on either axis -> 9.",
    "",
    "inputs:",
    "  mrace: _MRACE1",
    "  hisp:  _HISPANC",
    "",
    "levels:",
    "  - value: 1",
    "    label: White only, NH",
    "    when: mrace == 1 & hisp == 2",
    "  - value: 2",
    "    label: Black only, NH",
    "    when: mrace == 2 & hisp == 2",
    "  - value: 3",
    "    label: AIAN only, NH",
    "    when: mrace == 3 & hisp == 2",
    "  - value: 4",
    "    label: Asian only, NH",
    "    when: mrace == 4 & hisp == 2",
    "  - value: 5",
    "    label: NHPI only, NH",
    "    when: mrace == 5 & hisp == 2",
    "  - value: 6",
    "    label: Other race only, NH",
    "    when: mrace == 6 & hisp == 2",
    "  - value: 7",
    "    label: Multiracial, NH",
    "    when: mrace == 7 & hisp == 2",
    "  - value: 8",
    "    label: Hispanic",
    "    when: hisp == 1",
    "  - value: 9",
    "    label: Don't know / Not sure / Refused",
    "    when: hisp == 9 | (mrace %in% c(77, 99) & hisp == 2)",
    ""
  )
}
