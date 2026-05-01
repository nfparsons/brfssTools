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
  stop("brfss_setup_categorical_map() is not used in v0.2.0. ",
       "Calculation YAML now lives inline in the crosswalk's ",
       "calculation_yaml column. To set up a calculated cell, use ",
       "the editor's 'Calculated' toggle or call cw_set_calc().",
       call. = FALSE)
}

#' (deprecated in v0.2.0)
#' @export
brfss_setup_race_map <- function(path = NULL, edit = interactive()) {
  stop("brfss_setup_race_map() is not used in v0.2.0. ",
       "Calculation YAML now lives inline in the crosswalk's ",
       "calculation_yaml column.", call. = FALSE)
}

# ============================================================================
# Public: load, validate, render
# ============================================================================

#' Load and validate a transformation YAML
#'
#' Generic loader that reads any transformation YAML and dispatches based
#' on the `type:` field. Currently supported types:
#' \describe{
#'   \item{`categorical_map`}{Ordered list of `(value, label, when)` rules;
#'     first match wins. The classic categorical recode.}
#'   \item{`passthrough`}{Renames a single raw input column to the output
#'     column name. No recoding. Year-aware aliasing supported via
#'     `inputs:` and `by_year:` blocks.}
#' }
#'
#' @param fp Path to the YAML file.
#' @return A list with `output_column`, `type`, `description`, `inputs`,
#'   plus type-specific fields (`levels` for categorical_map; `input` for
#'   passthrough).
#' @export
brfss_load_transformation_spec <- function(fp) {
  if (!file.exists(fp)) {
    stop("Transformation file not found: ", fp, call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Reading transformations requires the 'yaml' package. ",
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

  if (is.null(spec$output_column) || !nzchar(spec$output_column)) {
    stop("Transformation at ", fp, " missing required `output_column`.",
         call. = FALSE)
  }
  if (is.null(spec$type) || !nzchar(spec$type)) {
    stop("Transformation at ", fp, " missing required `type`.",
         call. = FALSE)
  }

  expected_name <- sub("\\.ya?ml$", "", basename(fp))
  if (!identical(expected_name, spec$output_column)) {
    warning(sprintf(
      "Transformation filename '%s' does not match output_column '%s'. ",
      expected_name, spec$output_column),
      "brfss_pull() resolves transformations by filename, so this will ",
      "be loaded as '%s'.", expected_name,
      call. = FALSE)
  }

  # Type-specific validation
  switch(
    spec$type,
    "categorical_map" = .validate_categorical_map(spec, fp),
    "passthrough"     = .validate_passthrough(spec, fp),
    stop("Unknown transformation type at ", fp, ": '", spec$type, "'. ",
         "Supported: categorical_map, passthrough.", call. = FALSE)
  )
}

# Backward-compat alias — older code still calls brfss_load_categorical_map.
#' @rdname brfss_load_transformation_spec
#' @export
brfss_load_categorical_map <- function(fp) {
  spec <- brfss_load_transformation_spec(fp)
  if (spec$type != "categorical_map") {
    stop("File at ", fp, " is type '", spec$type,
         "', not categorical_map. ",
         "Use brfss_load_transformation_spec() for type-agnostic loading.",
         call. = FALSE)
  }
  spec
}

# ----- Type-specific validators -----

.validate_categorical_map <- function(spec, fp) {
  has_top_levels <- !is.null(spec$levels) && is.list(spec$levels) &&
                     length(spec$levels) > 0L
  has_year_levels <- !is.null(spec$by_year) && length(spec$by_year) > 0L &&
                      any(vapply(spec$by_year, function(blk) {
                        !is.null(blk$levels) && length(blk$levels) > 0L
                      }, logical(1L)))

  if (!has_top_levels && !has_year_levels) {
    stop("categorical_map at ", fp,
         " must define at least one level (either at top-level `levels:` ",
         "or in a `by_year:` block).", call. = FALSE)
  }

  # Normalize and validate top-level levels (if any)
  if (has_top_levels) {
    spec$levels <- .validate_levels_block(spec$levels, fp, "top-level")
  }

  # Normalize and validate any by_year levels blocks
  if (!is.null(spec$by_year) && length(spec$by_year) > 0L) {
    for (key in names(spec$by_year)) {
      blk <- spec$by_year[[key]]
      if (!is.null(blk$levels) && length(blk$levels) > 0L) {
        spec$by_year[[key]]$levels <- .validate_levels_block(
          blk$levels, fp, sprintf("by_year['%s']", key)
        )
      }
    }
  }

  spec$inputs <- spec$inputs %||% list()
  spec
}

# Validate a list of level definitions; return normalized list.
.validate_levels_block <- function(levels, fp, ctx) {
  out <- lapply(seq_along(levels), function(i) {
    lvl <- levels[[i]]
    if (is.null(lvl$value)) {
      stop("Level #", i, " in ", ctx, " of ", fp, " missing `value`.",
           call. = FALSE)
    }
    if (is.null(lvl$when) || !nzchar(lvl$when)) {
      stop("Level #", i, " (value=", lvl$value, ") in ", ctx, " of ", fp,
           " missing `when` expression.", call. = FALSE)
    }
    list(
      value = lvl$value,
      label = lvl$label %||% as.character(lvl$value),
      when  = lvl$when
    )
  })
  for (i in seq_along(out)) {
    expr_str <- out[[i]]$when
    parsed <- tryCatch(parse(text = expr_str), error = function(e) e)
    if (inherits(parsed, "error")) {
      stop("Level #", i, " (value=", out[[i]]$value, ") in ", ctx, " of ",
           fp, " has unparseable `when` expression:\n  ", expr_str,
           "\n  Error: ", conditionMessage(parsed), call. = FALSE)
    }
  }
  out
}

.validate_passthrough <- function(spec, fp) {
  if (is.null(spec$inputs) || length(spec$inputs) == 0L) {
    stop("passthrough at ", fp, " requires `inputs:` with at least one ",
         "alias \u2192 raw column mapping.", call. = FALSE)
  }
  if (length(spec$inputs) > 1L) {
    warning("passthrough at ", fp, " has multiple inputs; only the first ",
            "will be used.", call. = FALSE)
  }
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
  stop("brfss_render_transformation_code() is not used in v0.2.0. ",
       "Calculation YAML now lives inline in the crosswalk's ",
       "calculation_yaml column.", call. = FALSE)
}

# ============================================================================
# Internal: runtime evaluator
# ============================================================================

# Apply any transformation spec. Dispatches on type. Used by brfss_pull().
.brfss_apply_transformation_spec <- function(.data, spec, year) {
  switch(
    spec$type,
    "categorical_map" = .brfss_apply_categorical_map(.data, spec, year),
    "passthrough"     = .brfss_apply_passthrough(.data, spec, year),
    stop("Unknown transformation type: '", spec$type, "'", call. = FALSE)
  )
}

# Apply a passthrough: rename a single input column to the output column.
# If the input is missing, output is all NA (with a warning).
.brfss_apply_passthrough <- function(.data, spec, year) {
  inputs <- .resolve_inputs_for_year(spec, year)
  if (length(inputs) == 0L) {
    stop("passthrough '", spec$output_column,
         "' has no inputs declared.", call. = FALSE)
  }
  raw_col <- inputs[[1]]
  if (!raw_col %in% names(.data)) {
    warning(sprintf(
      "passthrough '%s' for year %d: input column '%s' missing. ",
      spec$output_column, year, raw_col),
      "Output will be NA for this year.",
      call. = FALSE)
    .data[[spec$output_column]] <- NA
    return(.data)
  }
  .data[[spec$output_column]] <- .data[[raw_col]]
  .data
}

# Apply a categorical map to a per-year data frame.
# Returns the data frame with an added/replaced column matching spec$output_column.
.brfss_apply_categorical_map <- function(.data, spec, year) {
  # Resolve which inputs and levels apply this year.
  inputs <- .resolve_inputs_for_year(spec, year)
  levels <- .resolve_levels_for_year(spec, year)

  if (length(levels) == 0L) {
    warning(sprintf(
      "Categorical map '%s' has no levels for year %d. Output will be NA.",
      spec$output_column, year),
      call. = FALSE)
    .data[[spec$output_column]] <- NA
    return(.data)
  }

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
  out_type <- if (length(levels) &&
                   is.numeric(levels[[1]]$value)) "integer" else "character"
  if (out_type == "integer") {
    out <- as.integer(out)
  } else {
    out <- as.character(out)
  }

  for (lvl in levels) {
    cond <- tryCatch(
      eval(parse(text = lvl$when), envir = eval_env),
      error = function(e) {
        stop(sprintf(
          "Categorical map '%s' for year %d: error evaluating level value=%s: %s",
          spec$output_column, year, lvl$value, conditionMessage(e)),
          call. = FALSE)
      }
    )
    # Recycle scalar logical (e.g. `when: TRUE`) to the data length.
    if (length(cond) == 1L && nrow(.data) > 1L) {
      cond <- rep(cond, nrow(.data))
    }
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

# Determine which levels apply for a given year.
# Order of precedence: by_year override > top-level levels > NULL.
# Note: levels are NOT MERGED with the top-level — a by_year levels block
# completely replaces them. This matches the mental model of a "different
# coding for this year range" (e.g., INCOME2 codes 1-8 vs INCOME3 codes 1-11).
.resolve_levels_for_year <- function(spec, year) {
  by_year <- spec$by_year
  if (is.null(by_year) || length(by_year) == 0L) {
    return(spec$levels %||% list())
  }
  for (key in names(by_year)) {
    if (.year_in_range(year, key)) {
      blk <- by_year[[key]]
      if (!is.null(blk$levels) && length(blk$levels) > 0L) {
        return(blk$levels)
      }
      # block matched but no levels override; fall through to top-level
      return(spec$levels %||% list())
    }
  }
  spec$levels %||% list()
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

.render_passthrough_as_r <- function(spec, name) {
  inputs <- spec$inputs %||% list()
  raw_col <- if (length(inputs)) inputs[[1]] else NA_character_

  out <- c(
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

  out <- c(out,
    sprintf("# Type: passthrough (input column copied to output without recoding)"),
    sprintf("# Input: %s -> %s", names(inputs)[1] %||% "?", raw_col %||% "?"),
    "")

  out <- c(out, "library(dplyr)", "")
  out <- c(out, sprintf("%s <- function(.data, year) {", name))
  out <- c(out, sprintf('  .data %%>%% mutate(%s = .data[["%s"]])',
                        spec$output_column, raw_col))
  out <- c(out, "}", "")

  out
}

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
  if (!is.null(spec$levels) && length(spec$levels) > 0L) {
    out <- c(out, "# Levels (first match wins):")
    for (lvl in spec$levels) {
      out <- c(out,
        sprintf("#   %s = %s", as.character(lvl$value), lvl$label),
        sprintf("#       when: %s", lvl$when)
      )
    }
    out <- c(out, "")
  }
  if (!is.null(spec$by_year) && length(spec$by_year) > 0L) {
    out <- c(out, "# Year-aware blocks present:")
    for (yr_key in names(spec$by_year)) {
      blk <- spec$by_year[[yr_key]]
      bits <- character(0)
      if (!is.null(blk$inputs)) bits <- c(bits, "inputs")
      if (!is.null(blk$levels)) bits <- c(bits, "levels")
      out <- c(out, sprintf("#   %s: overrides %s", yr_key,
                             paste(bits, collapse = " + ")))
    }
    out <- c(out, "")
  }

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

  # Build the case_when() expression. Use whatever levels block we can find:
  # top-level if present, else first by_year block's levels.
  effective_levels <- spec$levels
  if (is.null(effective_levels) || length(effective_levels) == 0L) {
    if (!is.null(spec$by_year) && length(spec$by_year)) {
      for (key in names(spec$by_year)) {
        if (!is.null(spec$by_year[[key]]$levels) &&
            length(spec$by_year[[key]]$levels) > 0L) {
          effective_levels <- spec$by_year[[key]]$levels
          out <- c(out,
            sprintf("  # NOTE: rendered using levels from by_year['%s']; ",
                    key),
            "  # other year-blocks have their own level sets — see the YAML.",
            "")
          break
        }
      }
    }
  }
  if (is.null(effective_levels) || length(effective_levels) == 0L) {
    out <- c(out, "  # No levels defined in this transformation.", "  .data",
             "}", "")
    return(out)
  }

  out <- c(out, "  .data %>%")
  out <- c(out, sprintf("    mutate(%s = case_when(", spec$output_column))

  for (i in seq_along(effective_levels)) {
    lvl <- effective_levels[[i]]
    val_repr <- if (is.numeric(lvl$value)) {
      as.character(lvl$value)
    } else {
      sprintf('"%s"', lvl$value)
    }
    sep <- if (i == length(effective_levels)) "" else ","
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
