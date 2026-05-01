# R/crosswalk_io.R
#
# v0.2.0 schema. Crosswalk schema:
#   concept_id        chr (R-friendly name)
#   year              int
#   state_var         chr (NA if calculated or unmapped)
#   is_calculated     int (0 = column lookup, 1 = use calculation_yaml)
#   calculation_yaml  chr (NA unless is_calculated)
#   domain            chr (top-level; "Unassigned" until user assigns)
#   subdomain         chr (optional finer grouping)
#   unverified        int (1 if drafted-not-yet-confirmed; 0 once user verifies)
#   notes             chr
#
# v0.1.0 CRUD (cw_add_pair, cw_remove_pair, etc.) is archived in
# inst/archive/R/crosswalk_io_v01.R. The v0.2.0 schema doesn't pair
# state_var with cdc_var any more, so the new CRUD is concept-centered.

#' @keywords internal
.cw_default_path <- function() {
  # Deprecated in v0.2.0 path-based architecture. Kept for back-compat
  # with calls from older code paths; returns NULL.
  NULL
}

#' Canonical column ordering for v0.2.0 crosswalk
#' @keywords internal
.cw_v2_columns <- function() {
  c("concept_id", "year", "state_var", "is_calculated",
    "calculation_yaml", "domain", "subdomain", "unverified", "notes")
}

# Resolve the crosswalk path: explicit `path` arg wins; else use the
# registered pool's crosswalk for `dataset`.
.cw_resolve_path <- function(dataset = NULL, path = NULL) {
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L) {
      stop("`path` must be a single character.", call. = FALSE)
    }
    return(path)
  }
  ds <- .brfss_resolve_dataset(dataset)
  cfg <- .brfss_get_pool(ds)
  cfg$crosswalk
}

#' Load the crosswalk bundle
#'
#' Reads the crosswalk and reference data for a registered pool.
#' Returns a list bundle with mutable \code{$crosswalk} plus read-only
#' reference data.
#'
#' @param dataset Character. The registered pool to load. If NULL and
#'   exactly one pool is registered, uses that one.
#' @param path Optional explicit path to the crosswalk CSV (overrides
#'   what's registered for the pool).
#' @return A list bundle with elements `crosswalk`, `cdc_codebook`,
#'   `taxonomy`, `cdc_calculated_vars`, `state_codebook` (the codebook
#'   for the most recent year, if available), `dataset`, `.path`.
#' @export
cw_load <- function(dataset = NULL, path = NULL) {
  ds <- if (is.null(path)) .brfss_resolve_dataset(dataset) else (dataset %||% NA_character_)
  cw_path <- .cw_resolve_path(ds, path)
  if (!file.exists(cw_path)) {
    stop("Crosswalk not found at: ", cw_path,
         "\nRun brfss_draft_crosswalk(dataset = \"", ds, "\") to create it.",
         call. = FALSE)
  }

  # Read crosswalk
  cw <- readr::read_csv(cw_path, show_col_types = FALSE,
                         locale = readr::locale(encoding = "UTF-8"),
                         progress = FALSE)

  required_cw <- .cw_v2_columns()
  missing_cw  <- setdiff(required_cw, names(cw))
  v01_only <- intersect(c("cdc_var", "is_primary", "source", "score"),
                        names(cw))
  if (length(v01_only) > 0L && length(missing_cw) > 0L) {
    stop(
      "crosswalk.csv at ", cw_path, " appears to be the v0.1.0 schema ",
      "(still has columns: ", paste(v01_only, collapse = ", "),
      ").\nRun brfss_migrate_crosswalk_to_v2(path = \"",
      cw_path, "\") to update.",
      call. = FALSE
    )
  }
  if (length(missing_cw) > 0L) {
    stop("crosswalk.csv missing columns: ",
         paste(missing_cw, collapse = ", "),
         "\n(Expected v0.2.0 schema: ", paste(required_cw, collapse = ", "),
         ")", call. = FALSE)
  }

  cw$year             <- as.integer(cw$year)
  cw$is_calculated    <- as.integer(cw$is_calculated)
  cw$is_calculated[is.na(cw$is_calculated)] <- 0L
  cw$unverified       <- as.integer(cw$unverified)
  cw$unverified[is.na(cw$unverified)] <- 0L

  # Read shipped reference data (read-only)
  pkg_extdata <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
  read_ref <- function(fname) {
    if (is.null(pkg_extdata)) return(NULL)
    fp <- file.path(pkg_extdata, fname)
    if (!file.exists(fp)) return(NULL)
    readr::read_csv(fp, show_col_types = FALSE,
                     locale = readr::locale(encoding = "UTF-8"),
                     progress = FALSE)
  }

  # The "state_codebook" the editor uses for its dropdown is now derived
  # from the registered pool's codebook directory: stack the per-year
  # codebook CSVs to get a single (variable_name, year) table. If no
  # codebook files exist, falls back to NULL — the editor handles this
  # by using the data file column lists directly.
  state_codebook <- if (!is.na(ds)) .stack_codebooks_for_pool(ds) else NULL

  bundle <- list(
    crosswalk           = cw,
    cdc_codebook        = read_ref("cdc_codebook.csv"),
    cdc_calculated_vars = read_ref("cdc_calculated_vars.csv"),
    taxonomy            = read_ref("taxonomy.csv"),
    state_codebook      = state_codebook,
    dataset             = ds,
    .path               = cw_path
  )
  bundle
}

#' Stack per-year codebook CSVs for a dataset into one tibble. If no
#' codebooks are present, falls back to reading variable names from the
#' data files (no rich context, but the editor's dropdown still works).
#' @keywords internal
.stack_codebooks_for_pool <- function(dataset) {
  cfg <- .brfss_get_pool(dataset)
  if (is.null(cfg)) return(NULL)

  # First, try codebooks
  doc_dir <- cfg$codebook_path
  rows <- list()
  if (dir.exists(doc_dir)) {
    pattern <- sprintf("^%s_[0-9]{4}_codebook\\.csv$", dataset)
    files <- list.files(doc_dir, pattern = pattern, full.names = TRUE)
    for (fp in files) {
      cb <- tryCatch(
        readr::read_csv(fp, show_col_types = FALSE, progress = FALSE,
                         locale = readr::locale(encoding = "UTF-8")),
        error = function(e) NULL
      )
      if (is.null(cb)) next
      if (!"variable_name" %in% names(cb) || !"year" %in% names(cb)) next
      cb$raw_var_name <- as.character(cb$variable_name)
      cb$year         <- as.integer(cb$year)
      rows[[length(rows) + 1L]] <- cb[, c("raw_var_name", "year"),
                                        drop = FALSE]
    }
  }

  # Fall back to data files for years not covered by a codebook
  cb_years <- if (length(rows) > 0L) {
    unique(do.call(rbind, rows)$year)
  } else integer(0)
  data_years <- as.integer(names(cfg$files))
  fallback_years <- setdiff(data_years, cb_years)

  for (yr in fallback_years) {
    fp <- cfg$files[[as.character(yr)]]
    if (is.null(fp) || !file.exists(fp)) next
    vars <- tryCatch(
      .brfss_read_var_names(fp),
      error = function(e) character(0)
    )
    if (length(vars) == 0L) next
    rows[[length(rows) + 1L]] <- tibble::tibble(
      raw_var_name = vars,
      year         = as.integer(yr)
    )
  }

  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

#' Save the bundle back to disk
#'
#' Writes \code{crosswalk.csv} (with .bak rotation). Reference data
#' is never written.
#'
#' @param bundle Bundle from \code{cw_load()}, possibly modified.
#' @param dataset Character dataset name. If NULL, uses the bundle's
#'   recorded dataset.
#' @param path Optional explicit path override.
#' @return The bundle, invisibly.
#' @export
cw_save <- function(bundle, dataset = NULL, path = NULL) {
  if (is.null(path)) {
    if (!is.null(bundle$.path)) {
      path <- bundle$.path
    } else {
      ds <- dataset %||% bundle$dataset %||% .brfss_resolve_dataset(NULL)
      path <- .cw_resolve_path(ds, NULL)
    }
  }

  # Ensure parent dir exists
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  # Backup
  if (file.exists(path)) {
    file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  }

  if (!is.null(bundle$crosswalk)) {
    cw <- bundle$crosswalk
    canonical <- intersect(.cw_v2_columns(), names(cw))
    extras    <- setdiff(names(cw), canonical)
    cw <- cw[, c(canonical, extras), drop = FALSE]
    readr::write_csv(cw, path, na = "")
  }

  invisible(bundle)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# v0.2.0 CRUD
# ============================================================================

#' Validate a concept_id string.
#' @keywords internal
.cw_validate_concept_id <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("concept_id must be a non-empty string.", call. = FALSE)
  }
  if (nchar(x) > 63) {
    stop("concept_id must be 63 characters or fewer.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)) {
    stop("concept_id '", x, "' must start with a letter or underscore and ",
         "contain only letters, digits, and underscores.", call. = FALSE)
  }
  invisible(TRUE)
}

# Find the index of the (concept_id, year) row, or integer(0) if absent.
.cw_find_cell <- function(bundle, concept_id, year) {
  cw <- bundle$crosswalk
  which(cw$concept_id == concept_id & cw$year == as.integer(year))
}

#' Set a state_var for a (concept, year) cell
#'
#' Updates the cell to be a column lookup (is_calculated = 0).
#' Clears the calculation_yaml if previously set. Marks the cell as
#' verified (unverified = 0).
#'
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id Concept name.
#' @param year Integer year.
#' @param state_var Column name in that year's data file. Pass NA to
#'   clear the mapping.
#' @return Updated bundle.
#' @export
cw_set_var <- function(bundle, concept_id, year, state_var) {
  idx <- .cw_find_cell(bundle, concept_id, year)
  if (length(idx) == 0L) {
    stop(sprintf("No crosswalk row for concept '%s' year %s.",
                 concept_id, year), call. = FALSE)
  }
  if (length(idx) > 1L) {
    stop(sprintf("Multiple rows found for concept '%s' year %s. ",
                 concept_id, year),
         "Crosswalk integrity violated.", call. = FALSE)
  }
  bundle$crosswalk$state_var[idx]        <- state_var
  bundle$crosswalk$is_calculated[idx]    <- 0L
  bundle$crosswalk$calculation_yaml[idx] <- NA_character_
  bundle$crosswalk$unverified[idx]       <- if (is.na(state_var)) 1L else 0L
  bundle
}

#' Set a calculation YAML for a (concept, year) cell
#'
#' Marks the cell as calculated (is_calculated = 1) and stores the
#' inline YAML. Clears state_var.
#'
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id Concept name.
#' @param year Integer year.
#' @param calculation_yaml YAML string defining the calculation.
#' @return Updated bundle.
#' @export
cw_set_calc <- function(bundle, concept_id, year, calculation_yaml) {
  idx <- .cw_find_cell(bundle, concept_id, year)
  if (length(idx) == 0L) {
    stop(sprintf("No crosswalk row for concept '%s' year %s.",
                 concept_id, year), call. = FALSE)
  }
  if (is.null(calculation_yaml) || !nzchar(calculation_yaml)) {
    stop("calculation_yaml must be a non-empty string.", call. = FALSE)
  }
  bundle$crosswalk$state_var[idx]        <- NA_character_
  bundle$crosswalk$is_calculated[idx]    <- 1L
  bundle$crosswalk$calculation_yaml[idx] <- calculation_yaml
  bundle$crosswalk$unverified[idx]       <- 0L
  bundle
}

#' Assign a domain (and optionally subdomain) to a concept across all its rows
#'
#' Domain assignment is per-concept, not per-year. All rows for the
#' concept get the same domain.
#'
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id Concept name.
#' @param domain New domain (e.g., "Behavioral Health").
#' @param subdomain Optional subdomain.
#' @return Updated bundle.
#' @export
cw_assign_domain <- function(bundle, concept_id,
                             domain, subdomain = NA_character_) {
  if (is.null(domain) || !nzchar(domain)) {
    stop("domain must be a non-empty string.", call. = FALSE)
  }
  rows <- which(bundle$crosswalk$concept_id == concept_id)
  if (length(rows) == 0L) {
    stop(sprintf("No rows for concept '%s'.", concept_id), call. = FALSE)
  }
  bundle$crosswalk$domain[rows]    <- domain
  bundle$crosswalk$subdomain[rows] <- subdomain
  bundle
}

#' Mark a (concept, year) cell as verified
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id Concept name.
#' @param year Integer year.
#' @return Updated bundle.
#' @export
cw_verify <- function(bundle, concept_id, year) {
  idx <- .cw_find_cell(bundle, concept_id, year)
  if (length(idx) == 0L) {
    stop(sprintf("No crosswalk row for concept '%s' year %s.",
                 concept_id, year), call. = FALSE)
  }
  bundle$crosswalk$unverified[idx] <- 0L
  bundle
}

#' Rename a concept across all its rows
#' @param bundle Bundle from \code{cw_load()}.
#' @param from Existing concept_id.
#' @param to New concept_id (will be validated for R-friendliness).
#' @return Updated bundle.
#' @export
cw_rename_concept <- function(bundle, from, to) {
  .cw_validate_concept_id(to)
  if (to %in% bundle$crosswalk$concept_id && to != from) {
    stop(sprintf("concept_id '%s' already exists. ", to),
         "Choose a different name or merge manually.",
         call. = FALSE)
  }
  rows <- which(bundle$crosswalk$concept_id == from)
  if (length(rows) == 0L) {
    stop(sprintf("No rows for concept '%s'.", from), call. = FALSE)
  }
  bundle$crosswalk$concept_id[rows] <- to
  bundle
}

#' Add a new concept (zero rows) to the crosswalk
#'
#' Use when you want a calculated-only concept with no most-recent-year
#' state_var anchor. Creates one row per year, all unmapped, awaiting
#' setup via \code{cw_set_var()} or \code{cw_set_calc()}.
#'
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id New concept name.
#' @param years Integer vector of years to create rows for.
#' @param domain Initial domain (default "Unassigned").
#' @param subdomain Optional subdomain.
#' @return Updated bundle.
#' @export
cw_add_concept <- function(bundle, concept_id, years,
                           domain = "Unassigned",
                           subdomain = NA_character_) {
  .cw_validate_concept_id(concept_id)
  if (concept_id %in% bundle$crosswalk$concept_id) {
    stop(sprintf("Concept '%s' already exists.", concept_id), call. = FALSE)
  }
  years <- as.integer(years)
  new_rows <- tibble::tibble(
    concept_id       = concept_id,
    year             = years,
    state_var        = NA_character_,
    is_calculated    = 0L,
    calculation_yaml = NA_character_,
    domain           = domain,
    subdomain        = subdomain,
    unverified       = 1L,
    notes            = NA_character_
  )
  bundle$crosswalk <- rbind(bundle$crosswalk, new_rows)
  bundle
}

#' Remove a concept and all its rows
#' @param bundle Bundle from \code{cw_load()}.
#' @param concept_id Concept to remove.
#' @return Updated bundle.
#' @export
cw_remove_concept <- function(bundle, concept_id) {
  rows <- which(bundle$crosswalk$concept_id == concept_id)
  if (length(rows) == 0L) {
    warning(sprintf("No rows for concept '%s'; nothing to remove.",
                    concept_id), call. = FALSE)
    return(bundle)
  }
  bundle$crosswalk <- bundle$crosswalk[-rows, , drop = FALSE]
  bundle
}

#' Merge one concept into another
#'
#' Moves all of \code{from}'s non-NA cell mappings (state_var or
#' calculation_yaml) into \code{into}'s empty cells, then removes
#' \code{from} from the crosswalk. Useful for cleaning up CDC's
#' year-versioned variables (e.g., merging ADDEPEV2 into ADDEPEV3).
#'
#' Conservative conflict handling: if both \code{from} and \code{into}
#' have a non-empty mapping for the same year, this errors. The user
#' must resolve manually (clear one of the cells via \code{cw_set_var}
#' or \code{cw_set_calc}, then retry the merge).
#'
#' \code{into}'s domain is preserved. \code{from}'s domain is discarded
#' along with the removed rows.
#'
#' @param bundle Bundle from \code{cw_load()}.
#' @param into Target concept_id (kept).
#' @param from Source concept_id (removed after mappings move).
#' @return Updated bundle.
#' @export
#' @examples
#' \dontrun{
#' cw <- cw_load()
#' # Move ADDEPEV2's mappings into ADDEPEV3, then drop ADDEPEV2:
#' cw <- cw_merge_concepts(cw, into = "ADDEPEV3", from = "ADDEPEV2")
#' cw_save(cw)
#' }
cw_merge_concepts <- function(bundle, into, from) {
  if (missing(into) || missing(from)) {
    stop("Both `into` and `from` are required.", call. = FALSE)
  }
  if (identical(into, from)) {
    stop("`into` and `from` must be different concepts.", call. = FALSE)
  }

  cw <- bundle$crosswalk
  into_rows <- which(cw$concept_id == into)
  from_rows <- which(cw$concept_id == from)

  if (length(into_rows) == 0L) {
    stop(sprintf("Target concept '%s' not found in crosswalk.", into),
         call. = FALSE)
  }
  if (length(from_rows) == 0L) {
    stop(sprintf("Source concept '%s' not found in crosswalk.", from),
         call. = FALSE)
  }

  # Detect conflicts: years where both concepts have non-empty mappings
  conflicts <- character(0)

  for (i in seq_along(from_rows)) {
    fr <- cw[from_rows[i], ]
    yr <- fr$year

    # Is this from-row "non-empty" (has a mapping or calc)?
    fr_has_var  <- !is.na(fr$state_var) && nzchar(fr$state_var)
    fr_has_calc <- isTRUE(fr$is_calculated == 1L) &&
                    !is.na(fr$calculation_yaml) &&
                    nzchar(fr$calculation_yaml)
    if (!fr_has_var && !fr_has_calc) next

    # Find the corresponding into-row for the same year
    into_idx <- into_rows[cw$year[into_rows] == yr]
    if (length(into_idx) == 0L) {
      # No into-row exists for this year; skip — we'll add the from-row
      # as a new into-row later
      next
    }
    if (length(into_idx) > 1L) {
      stop(sprintf("Concept '%s' has %d rows for year %d. ",
                   into, length(into_idx), yr),
           "Crosswalk integrity violated.", call. = FALSE)
    }

    ir <- cw[into_idx, ]
    ir_has_var  <- !is.na(ir$state_var) && nzchar(ir$state_var)
    ir_has_calc <- isTRUE(ir$is_calculated == 1L) &&
                    !is.na(ir$calculation_yaml) &&
                    nzchar(ir$calculation_yaml)

    if (ir_has_var || ir_has_calc) {
      conflicts <- c(conflicts, sprintf("year %d", yr))
    }
  }

  if (length(conflicts) > 0L) {
    stop(sprintf(
      "Cannot merge: both '%s' and '%s' have mappings for: %s.\n",
      from, into, paste(conflicts, collapse = ", ")),
      "Resolve manually (clear one cell or the other) before retrying.",
      call. = FALSE)
  }

  # Pass 2: actually move the mappings.
  for (i in seq_along(from_rows)) {
    fr <- cw[from_rows[i], ]
    yr <- fr$year

    fr_has_var  <- !is.na(fr$state_var) && nzchar(fr$state_var)
    fr_has_calc <- isTRUE(fr$is_calculated == 1L) &&
                    !is.na(fr$calculation_yaml) &&
                    nzchar(fr$calculation_yaml)
    if (!fr_has_var && !fr_has_calc) next

    into_idx <- into_rows[cw$year[into_rows] == yr]
    if (length(into_idx) == 0L) {
      # Add a new into-row for this year. (Edge case: from has data
      # in years into doesn't cover; we extend into.)
      new_row <- fr
      new_row$concept_id <- into
      new_row$domain     <- cw$domain[into_rows[1]]
      new_row$subdomain  <- cw$subdomain[into_rows[1]]
      cw <- rbind(cw, new_row)
      # Refresh into_rows since we just added one
      into_rows <- which(cw$concept_id == into)
      next
    }

    # Move the mapping
    cw$state_var[into_idx]        <- fr$state_var
    cw$is_calculated[into_idx]    <- fr$is_calculated
    cw$calculation_yaml[into_idx] <- fr$calculation_yaml
    cw$unverified[into_idx]       <- fr$unverified
  }

  # Remove the from-rows. Recompute indices since we may have added rows.
  from_rows <- which(cw$concept_id == from)
  if (length(from_rows) > 0L) {
    cw <- cw[-from_rows, , drop = FALSE]
  }

  bundle$crosswalk <- cw
  bundle
}
