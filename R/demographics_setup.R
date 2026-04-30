# R/demographics_setup.R
#
# brfss_setup_demographic() — copy a shipped demographic template into
# the user's transformations/ folder, customizing the output column name
# to the standard demographic name (age, sex, race, ethnicity, education,
# income).
#
# brfss_list_demographic_templates() — show what's available.
# brfss_demographic_status() — show what the user has configured.

#' Set up a demographic transformation from a shipped template
#'
#' Copies a built-in template YAML into your config dir's
#' `transformations/` folder, ready to use with `brfss_pull()`. After
#' setup, edit the YAML if your codebook uses different column names
#' than the template assumes (most state codebooks need at least
#' light editing).
#'
#' If a transformation with the same name already exists, this errors
#' unless `overwrite = TRUE`.
#'
#' @param name      The demographic name, becomes the output column.
#'   One of: `"age"`, `"sex"`, `"race"`, `"ethnicity"`, `"education"`,
#'   `"income"`. (Other names are accepted but won't have shipped
#'   templates.)
#' @param template  The template variant. See
#'   [brfss_list_demographic_templates()] for the full set. If NULL,
#'   uses the package's default for that demographic.
#' @param path      Optional config dir override.
#' @param edit      Logical. If TRUE (default in interactive sessions),
#'   opens the resulting YAML in the user's editor.
#' @param overwrite Logical. Default FALSE.
#' @return The destination YAML path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Set up the standard CDC age groupings:
#' brfss_setup_demographic("age", template = "cdc_6group")
#'
#' # Self-reported binary sex:
#' brfss_setup_demographic("sex", template = "selfreport_binary")
#'
#' # CDC's 9-category race:
#' brfss_setup_demographic("race", template = "cdc_9category")
#' }
brfss_setup_demographic <- function(name,
                                    template  = NULL,
                                    path      = NULL,
                                    edit      = interactive(),
                                    overwrite = FALSE) {
  if (missing(name) || !is.character(name) || length(name) != 1L ||
      !nzchar(name)) {
    stop("`name` is required.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    stop("`name` must be R-friendly (letters/digits/underscores, ",
         "no leading digit).", call. = FALSE)
  }

  # Pick template (defaults per demographic)
  if (is.null(template)) {
    template <- .brfss_default_template_for(name)
    if (is.null(template)) {
      stop("No default template for '", name, "'. Specify a template ",
           "explicitly. See brfss_list_demographic_templates() for options.",
           call. = FALSE)
    }
  }

  # Resolve template path: inst/extdata/transformations/templates/<name>__<template>.yaml
  pkg_extdata <- .brfss_package_extdata()
  template_dir <- file.path(pkg_extdata, "transformations", "templates")
  template_fname <- sprintf("%s__%s.yaml", name, template)
  template_fp <- file.path(template_dir, template_fname)

  if (!file.exists(template_fp)) {
    available <- brfss_list_demographic_templates(name = name)
    stop("Template '", template, "' not found for demographic '", name,
         "'. Available: ",
         paste(available$template, collapse = ", "),
         ".\n  Use brfss_list_demographic_templates() to see all templates.",
         call. = FALSE)
  }

  cfg <- brfss_config_path(path, must_exist = FALSE)
  trans_dir <- file.path(cfg, "transformations")
  if (!dir.exists(trans_dir)) {
    dir.create(trans_dir, recursive = TRUE, showWarnings = FALSE)
  }

  dest <- file.path(trans_dir, paste0(name, ".yaml"))
  if (file.exists(dest) && !overwrite) {
    stop("Transformation '", name, ".yaml' already exists at:\n  ", dest, "\n",
         "Set overwrite = TRUE to replace.", call. = FALSE)
  }

  ok <- file.copy(template_fp, dest, overwrite = overwrite)
  if (!isTRUE(ok)) {
    stop("Failed to copy template to: ", dest, call. = FALSE)
  }

  message(sprintf("Set up demographic '%s' from template '%s'.\n  -> %s",
                  name, template, dest))
  message("Edit if your codebook uses different input column names.")

  if (edit) utils::file.edit(dest)
  invisible(dest)
}

#' List available demographic templates
#'
#' Returns a tibble of every template shipped with the package, optionally
#' filtered to a single demographic.
#'
#' @param name Optional demographic name to filter to (e.g., `"age"`).
#' @return A tibble with columns `demographic`, `template`, `description`,
#'   `path`.
#' @export
#' @examples
#' \dontrun{
#' brfss_list_demographic_templates()
#' brfss_list_demographic_templates(name = "age")
#' }
brfss_list_demographic_templates <- function(name = NULL) {
  pkg_extdata <- .brfss_package_extdata()
  template_dir <- file.path(pkg_extdata, "transformations", "templates")
  if (!dir.exists(template_dir)) {
    return(tibble::tibble(
      demographic = character(), template = character(),
      description = character(), path = character()
    ))
  }

  files <- list.files(template_dir, pattern = "\\.ya?ml$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0L) {
    return(tibble::tibble(
      demographic = character(), template = character(),
      description = character(), path = character()
    ))
  }

  parts <- strsplit(sub("\\.ya?ml$", "", basename(files)), "__", fixed = TRUE)
  demographic <- vapply(parts, function(p) p[1], character(1))
  template    <- vapply(parts, function(p) if (length(p) >= 2L) p[2] else "default",
                        character(1))

  # Pull description from the YAML's `description:` field
  description <- vapply(files, .read_yaml_description, character(1))

  out <- tibble::tibble(
    demographic = demographic,
    template    = template,
    description = description,
    path        = files
  )

  if (!is.null(name)) {
    out <- out[out$demographic == name, , drop = FALSE]
  }

  out[order(out$demographic, out$template), ]
}

#' Show which demographics are configured in your config dir
#'
#' Each of the BRFSS "big six" demographics (age, sex, race, ethnicity,
#' education, income) is reported as either configured (a YAML or .R
#' transformation exists) or not. Useful as a setup checklist.
#'
#' @param path Optional config dir override.
#' @return A tibble with columns `demographic`, `status`, `kind`, `path`.
#' @export
brfss_demographic_status <- function(path = NULL) {
  cfg <- brfss_config_path(path, must_exist = FALSE)
  trans_dir <- file.path(cfg, "transformations")

  big_six <- c("age", "sex", "race", "ethnicity", "education", "income")

  rows <- lapply(big_six, function(d) {
    yaml_fp <- file.path(trans_dir, paste0(d, ".yaml"))
    r_fp    <- file.path(trans_dir, paste0(d, ".R"))
    if (file.exists(yaml_fp)) {
      list(demographic = d, status = "configured", kind = "yaml",
           path = yaml_fp)
    } else if (file.exists(r_fp) && !.is_autogen_r_companion(r_fp)) {
      list(demographic = d, status = "configured", kind = "r",
           path = r_fp)
    } else {
      list(demographic = d, status = "not configured", kind = NA_character_,
           path = NA_character_)
    }
  })

  tibble::tibble(
    demographic = vapply(rows, `[[`, character(1), "demographic"),
    status      = vapply(rows, `[[`, character(1), "status"),
    kind        = vapply(rows, `[[`, character(1), "kind"),
    path        = vapply(rows, `[[`, character(1), "path")
  )
}

# ============================================================================
# Internal helpers
# ============================================================================

# Default template per demographic. Used when brfss_setup_demographic() is
# called with template = NULL.
.brfss_default_template_for <- function(name) {
  defaults <- list(
    age        = "cdc_6group",
    sex        = "selfreport_binary",
    race       = "cdc_9category",
    ethnicity  = "hispanic_binary",
    education  = "cdc_4category",
    income     = "cdc_unified"
  )
  defaults[[name]]
}

# Read just the description: field from a YAML file, returning a one-line
# summary. Avoids loading the yaml package for the simple case.
.read_yaml_description <- function(fp) {
  if (!file.exists(fp)) return(NA_character_)
  lines <- readLines(fp, warn = FALSE)
  # Find the description: block (single-line or pipe-style)
  desc_idx <- grep("^description:", lines)
  if (length(desc_idx) == 0L) return(NA_character_)
  desc_line <- lines[desc_idx[1]]
  inline <- sub("^description:\\s*", "", desc_line)
  if (nzchar(inline) && !inline %in% c("|", ">", "|-", ">-")) {
    return(trimws(inline))
  }
  # Pipe-style: take the next non-empty indented line
  for (i in seq_len(length(lines) - desc_idx[1])) {
    nx <- lines[desc_idx[1] + i]
    if (nzchar(trimws(nx)) && grepl("^\\s+", nx)) {
      return(trimws(nx))
    }
    if (nzchar(trimws(nx)) && !grepl("^\\s+", nx)) break
  }
  NA_character_
}
