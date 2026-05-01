# R/draft_crosswalk.R
#
# Draft a v2 crosswalk from the registered data pool. Anchors on the
# user's most recent year's variable list.

#' Draft a crosswalk from your registered data pool
#'
#' Reads the most recent year's data file from the registered pool,
#' takes its variable list, and creates a draft crosswalk:
#' \itemize{
#'   \item One concept_id per variable in the most recent year
#'     (sanitized via \code{\link{sanitize_concept_id}})
#'   \item One row per concept per year covered by the pool
#'   \item state_var = the variable name in the most recent year
#'     (already verified, since that's the data the user is anchoring on);
#'     state_var = NA in earlier years (unverified, awaiting user mapping)
#'   \item Domain auto-assigned via the CDC codebook lookup
#'     (\code{\link{brfss_assign_domains}}); concepts with no match get
#'     `domain = "Unassigned"` and need user assignment in the editor.
#' }
#'
#' @param dataset Pool dataset name (e.g., "OR"). If NULL and exactly one
#'   pool is registered, uses that one.
#' @param path Optional config dir override.
#' @param overwrite_existing Logical. If FALSE (default), errors when a
#'   non-empty crosswalk already exists. If TRUE, replaces it (with .bak
#'   rotation).
#' @return The drafted crosswalk tibble, invisibly.
#' @export
brfss_draft_crosswalk <- function(dataset = NULL,
                                  path = NULL,
                                  overwrite_existing = FALSE) {
  cfg <- brfss_config_path(path, must_exist = TRUE)

  # Resolve dataset
  pool <- brfss_pool_status()
  if (is.null(pool) || nrow(pool) == 0L) {
    stop("No data pool registered. ",
         "Run brfss_set_pool(dataset, path) first.", call. = FALSE)
  }
  if (is.null(dataset)) {
    datasets <- unique(pool$dataset)
    if (length(datasets) > 1L) {
      stop("Multiple pools registered: ",
           paste(datasets, collapse = ", "),
           ". Specify which one with `dataset =`.", call. = FALSE)
    }
    dataset <- datasets[1]
  }
  pool <- pool[pool$dataset == dataset, , drop = FALSE]
  if (nrow(pool) == 0L) {
    stop(sprintf("No pool registered for dataset '%s'.", dataset),
         call. = FALSE)
  }

  # Check existing crosswalk
  cw_fp <- file.path(cfg, "crosswalk.csv")
  if (file.exists(cw_fp)) {
    existing <- readr::read_csv(cw_fp, show_col_types = FALSE,
                                 progress = FALSE)
    if (nrow(existing) > 0L && !overwrite_existing) {
      stop("A non-empty crosswalk already exists at:\n  ", cw_fp, "\n",
           "Pass overwrite_existing = TRUE to replace, or use ",
           "brfss_redraft_crosswalk() to add new years/variables ",
           "while preserving existing mappings.", call. = FALSE)
    }
    if (overwrite_existing && nrow(existing) > 0L) {
      bak <- paste0(cw_fp, ".bak")
      file.copy(cw_fp, bak, overwrite = TRUE)
      message("Existing crosswalk backed up to: ", bak)
    }
  }

  # Read most recent year's variable list
  most_recent_year <- max(pool$year)
  most_recent_file <- pool$file[pool$year == most_recent_year][1]
  message(sprintf(
    "Drafting from %s's most recent year (%d): %s",
    dataset, most_recent_year, most_recent_file))

  vars <- .brfss_read_var_names(most_recent_file)
  if (length(vars) == 0L) {
    stop(sprintf("No variables found in %s.", most_recent_file),
         call. = FALSE)
  }
  message(sprintf("Found %d variables in %d.", length(vars), most_recent_year))

  # Sanitize variable names to concept_ids
  concept_ids <- sanitize_concept_id(vars, resolve_collisions = TRUE)

  # Auto-assign domains
  domains <- brfss_assign_domains(concept_ids, raw_var_names = vars)
  n_unassigned <- sum(domains$domain == "Unassigned")
  message(sprintf(
    "Auto-assigned domains: %d assigned, %d unassigned (need user review).",
    nrow(domains) - n_unassigned, n_unassigned))

  # Build the crosswalk: cross-join concepts × years
  years <- sort(unique(pool$year))
  n_concepts <- length(concept_ids)
  n_years <- length(years)

  # Build the data frame manually for clarity
  rows <- list()
  for (i in seq_len(n_concepts)) {
    cid <- concept_ids[i]
    raw <- vars[i]
    dom <- domains$domain[i]
    sub <- domains$subdomain[i]

    for (yr in years) {
      is_recent <- yr == most_recent_year
      rows[[length(rows) + 1L]] <- list(
        concept_id       = cid,
        year             = as.integer(yr),
        state_var        = if (is_recent) raw else NA_character_,
        is_calculated    = 0L,
        calculation_yaml = NA_character_,
        domain           = dom,
        subdomain        = sub,
        unverified       = if (is_recent) 0L else 1L,
        notes            = NA_character_
      )
    }
  }

  # Build the tibble efficiently
  cw <- tibble::tibble(
    concept_id       = vapply(rows, `[[`, character(1), "concept_id"),
    year             = vapply(rows, `[[`, integer(1),   "year"),
    state_var        = vapply(rows, `[[`, character(1), "state_var"),
    is_calculated    = vapply(rows, `[[`, integer(1),   "is_calculated"),
    calculation_yaml = vapply(rows, `[[`, character(1), "calculation_yaml"),
    domain           = vapply(rows, `[[`, character(1), "domain"),
    subdomain        = vapply(rows, `[[`, character(1), "subdomain"),
    unverified       = vapply(rows, `[[`, integer(1),   "unverified"),
    notes            = vapply(rows, `[[`, character(1), "notes")
  )

  # Sort: by domain (Unassigned last), then by concept_id, then by year
  cw <- cw[order(cw$domain == "Unassigned",
                  cw$domain,
                  cw$concept_id,
                  cw$year), , drop = FALSE]

  readr::write_csv(cw, cw_fp, na = "")
  message(sprintf("Wrote %d rows to %s", nrow(cw), cw_fp))
  message(sprintf(
    "Next: open the editor and assign domains to %d Unassigned concepts.",
    n_unassigned))

  invisible(cw)
}

#' Re-draft a crosswalk to add new years / variables
#'
#' Idempotent extend: keeps existing concept_ids and mappings, but:
#' \itemize{
#'   \item Adds rows for any years in the pool that aren't yet
#'     represented (state_var = NA, unverified = 1).
#'   \item For variables in the most recent year that don't yet have a
#'     concept_id, creates new concepts and adds rows for all years.
#' }
#'
#' This is what you run after dropping a 2025 data file in: existing
#' work survives, new variables show up as Unassigned for review.
#'
#' @param dataset Pool dataset name. If NULL and exactly one pool is
#'   registered, uses that one.
#' @param path Optional config dir override.
#' @return The updated crosswalk tibble, invisibly.
#' @export
brfss_redraft_crosswalk <- function(dataset = NULL, path = NULL) {
  cfg <- brfss_config_path(path, must_exist = TRUE)
  cw_fp <- file.path(cfg, "crosswalk.csv")
  if (!file.exists(cw_fp)) {
    stop("No existing crosswalk found at:\n  ", cw_fp, "\n",
         "Use brfss_draft_crosswalk() for first-time drafting.",
         call. = FALSE)
  }

  pool <- brfss_pool_status()
  if (is.null(pool) || nrow(pool) == 0L) {
    stop("No data pool registered. ",
         "Run brfss_set_pool(dataset, path) first.", call. = FALSE)
  }
  if (is.null(dataset)) {
    datasets <- unique(pool$dataset)
    if (length(datasets) > 1L) {
      stop("Multiple pools registered: ",
           paste(datasets, collapse = ", "),
           ". Specify which one with `dataset =`.", call. = FALSE)
    }
    dataset <- datasets[1]
  }
  pool <- pool[pool$dataset == dataset, , drop = FALSE]

  cw <- readr::read_csv(cw_fp, show_col_types = FALSE, progress = FALSE)

  # ---- 1. Add rows for new years ----
  pool_years <- sort(unique(pool$year))
  cw_years <- sort(unique(cw$year))
  new_years <- setdiff(pool_years, cw_years)

  if (length(new_years) > 0L) {
    existing_concepts <- unique(cw$concept_id)
    new_year_rows <- expand.grid(
      concept_id = existing_concepts,
      year       = as.integer(new_years),
      stringsAsFactors = FALSE
    )
    new_year_rows$state_var        <- NA_character_
    new_year_rows$is_calculated    <- 0L
    new_year_rows$calculation_yaml <- NA_character_

    # Inherit domain/subdomain from existing rows for the same concept
    dom_lookup <- setNames(cw$domain[!duplicated(cw$concept_id)],
                            cw$concept_id[!duplicated(cw$concept_id)])
    sub_lookup <- setNames(cw$subdomain[!duplicated(cw$concept_id)],
                            cw$concept_id[!duplicated(cw$concept_id)])
    new_year_rows$domain    <- dom_lookup[new_year_rows$concept_id]
    new_year_rows$subdomain <- sub_lookup[new_year_rows$concept_id]
    new_year_rows$unverified <- 1L
    new_year_rows$notes      <- NA_character_

    cw <- rbind(cw, new_year_rows)
    message(sprintf("Added rows for %d new year(s): %s",
                    length(new_years), paste(new_years, collapse = ", ")))
  }

  # ---- 2. Add new concepts from the most recent year ----
  most_recent_year <- max(pool_years)
  most_recent_file <- pool$file[pool$year == most_recent_year][1]
  recent_vars <- .brfss_read_var_names(most_recent_file)
  recent_concept_ids <- sanitize_concept_id(recent_vars, resolve_collisions = TRUE)

  existing_concepts <- unique(cw$concept_id)
  new_concept_idx <- which(!recent_concept_ids %in% existing_concepts)

  if (length(new_concept_idx) > 0L) {
    new_concepts   <- recent_concept_ids[new_concept_idx]
    new_raw_names  <- recent_vars[new_concept_idx]
    new_domains    <- brfss_assign_domains(new_concepts,
                                            raw_var_names = new_raw_names)

    new_concept_rows <- list()
    for (i in seq_along(new_concepts)) {
      cid <- new_concepts[i]
      raw <- new_raw_names[i]
      dom <- new_domains$domain[i]
      sub <- new_domains$subdomain[i]
      for (yr in pool_years) {
        is_recent <- yr == most_recent_year
        new_concept_rows[[length(new_concept_rows) + 1L]] <- list(
          concept_id       = cid,
          year             = as.integer(yr),
          state_var        = if (is_recent) raw else NA_character_,
          is_calculated    = 0L,
          calculation_yaml = NA_character_,
          domain           = dom,
          subdomain        = sub,
          unverified       = if (is_recent) 0L else 1L,
          notes            = NA_character_
        )
      }
    }
    new_df <- tibble::tibble(
      concept_id       = vapply(new_concept_rows, `[[`, character(1), "concept_id"),
      year             = vapply(new_concept_rows, `[[`, integer(1),   "year"),
      state_var        = vapply(new_concept_rows, `[[`, character(1), "state_var"),
      is_calculated    = vapply(new_concept_rows, `[[`, integer(1),   "is_calculated"),
      calculation_yaml = vapply(new_concept_rows, `[[`, character(1), "calculation_yaml"),
      domain           = vapply(new_concept_rows, `[[`, character(1), "domain"),
      subdomain        = vapply(new_concept_rows, `[[`, character(1), "subdomain"),
      unverified       = vapply(new_concept_rows, `[[`, integer(1),   "unverified"),
      notes            = vapply(new_concept_rows, `[[`, character(1), "notes")
    )
    cw <- rbind(cw, new_df)
    message(sprintf("Added %d new concept(s) from %d's variable list.",
                    length(new_concepts), most_recent_year))
  }

  # Re-sort
  cw <- cw[order(cw$domain == "Unassigned",
                  cw$domain,
                  cw$concept_id,
                  cw$year), , drop = FALSE]

  # Backup + write
  bak <- paste0(cw_fp, ".bak")
  file.copy(cw_fp, bak, overwrite = TRUE)
  readr::write_csv(cw, cw_fp, na = "")

  message(sprintf("Wrote %d rows to %s (backup at %s).",
                  nrow(cw), cw_fp, bak))
  invisible(cw)
}

# ============================================================================
# Internal helpers
# ============================================================================

# Read column names from a data file. Supports CSV and XPT.
.brfss_read_var_names <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    h <- readr::read_csv(path, n_max = 0, show_col_types = FALSE,
                          progress = FALSE)
    return(names(h))
  }
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Reading XPT files requires the 'haven' package. ",
           "Install with: install.packages(\"haven\")", call. = FALSE)
    }
    h <- haven::read_xpt(path, n_max = 0)
    return(names(h))
  }
  stop("Unsupported file type: ", path,
       "\nExpected .csv or .xpt", call. = FALSE)
}
