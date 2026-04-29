# R/crosswalk_io.R
#
# Data layer for crosswalk CRUD. UI-free; usable from console.
#
# All functions operate on a "crosswalk bundle" - a list with the canonical
# data files loaded as tibbles:
#   $crosswalk       state_var <-> cdc_var pairs
#   $state_only      state-only variables (no CDC counterpart)
#   $cdc_only        CDC variables the state didn't field
#   $cdc_codebook    CDC variable definitions
#   $state_codebook  state variable definitions
#   $taxonomy        domain/subdomain/category hierarchy
#   $pending         undecided rows
#   $.path           directory the bundle was loaded from (for cw_save())
#
# Each mutation function returns a NEW bundle. Save explicitly with cw_save().

# Resolve the canonical extdata path. Prefers an explicit path if given,
# otherwise falls back to the package-installed extdata.
.cw_default_path <- function() {
  pkg_path <- system.file("extdata", package = "brfssTools")
  if (!nzchar(pkg_path)) {
    stop("brfssTools is not installed; pass an explicit `path =`.", call. = FALSE)
  }
  pkg_path
}

#' Load the crosswalk bundle from disk
#'
#' @param path Directory containing crosswalk.csv and friends. Defaults to
#'   the package's installed `inst/extdata/`.
#' @return A list of tibbles describing the crosswalk and its supporting data.
#' @export
cw_load <- function(path = NULL) {
  if (is.null(path)) path <- .cw_default_path()
  if (!dir.exists(path)) stop("Path not found: ", path, call. = FALSE)

  read_one <- function(fname, required = TRUE) {
    fp <- file.path(path, fname)
    if (!file.exists(fp)) {
      if (required) stop("Missing canonical file: ", fname, call. = FALSE)
      return(NULL)
    }
    readr::read_csv(fp, show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"))
  }

  bundle <- list(
    crosswalk      = read_one("crosswalk.csv"),
    state_only     = read_one("state_only.csv"),
    cdc_only       = read_one("cdc_only.csv"),
    cdc_codebook   = read_one("cdc_codebook.csv"),
    state_codebook = read_one("state_codebook.csv"),
    taxonomy       = read_one("taxonomy.csv", required = FALSE),
    pending        = read_one("pending.csv",  required = FALSE),
    .path          = path
  )

  # Schema sanity
  required_cw <- c("year","state_var","cdc_var","is_primary","source",
                   "score","notes")
  missing_cw <- setdiff(required_cw, names(bundle$crosswalk))
  if (length(missing_cw) > 0) {
    stop("crosswalk.csv missing columns: ",
         paste(missing_cw, collapse=", "), call. = FALSE)
  }

  bundle
}

#' Save the bundle back to disk with a .bak rotation
#'
#' Before writing, copies the existing crosswalk.csv to crosswalk.csv.bak.
#' Only files that have actually changed in-memory are rewritten.
#'
#' @param bundle Crosswalk bundle from `cw_load()` (potentially mutated).
#' @param path  Optional override of write path. Defaults to the bundle's
#'   loaded path.
#' @return The bundle, invisibly.
#' @export
cw_save <- function(bundle, path = NULL) {
  if (is.null(path)) path <- bundle$.path %||% .cw_default_path()
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  # Backup crosswalk.csv first
  cw_path <- file.path(path, "crosswalk.csv")
  if (file.exists(cw_path)) {
    bak_path <- paste0(cw_path, ".bak")
    file.copy(cw_path, bak_path, overwrite = TRUE)
  }

  # Write the mutable files. Codebooks (CDC, state) are read-only by design.
  write_if_present <- function(df, fname) {
    if (!is.null(df)) {
      readr::write_csv(df, file.path(path, fname), na = "")
    }
  }
  write_if_present(bundle$crosswalk,  "crosswalk.csv")
  write_if_present(bundle$state_only, "state_only.csv")
  write_if_present(bundle$cdc_only,   "cdc_only.csv")
  write_if_present(bundle$pending,    "pending.csv")

  invisible(bundle)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# Internal lookups
# ============================================================================

#' Find the row index in the crosswalk for (year, state_var, cdc_var)
#' @keywords internal
.cw_find <- function(bundle, year, state_var = NA_character_,
                     cdc_var = NA_character_) {
  cw <- bundle$crosswalk
  matches <- cw$year == year
  if (!is.na(state_var)) matches <- matches & cw$state_var == state_var
  if (!is.na(cdc_var))   matches <- matches & cw$cdc_var   == cdc_var
  which(matches)
}

# Validate that a variable name exists in the relevant codebook for that year.
# Returns TRUE silently or stops with a descriptive error.
.cw_validate_var <- function(bundle, year, var, side = c("state","cdc"),
                             allow_unknown = FALSE) {
  side <- match.arg(side)
  cb <- if (side == "state") bundle$state_codebook else bundle$cdc_codebook
  hit <- cb$year == year & cb$raw_var_name == var
  if (any(hit)) return(invisible(TRUE))
  if (allow_unknown) {
    warning(sprintf("%s variable '%s' not in %s codebook for year %d",
                    side, var, side, year), call. = FALSE)
    return(invisible(TRUE))
  }
  stop(sprintf("%s variable '%s' not found in %s codebook for year %d",
               side, var, side, year), call. = FALSE)
}

# ============================================================================
# Mutations
# ============================================================================

#' Add a new (year, state_var, cdc_var) pair to the crosswalk
#'
#' @param bundle      Bundle from `cw_load()`.
#' @param year        Integer year.
#' @param state_var   State variable name (must exist in state_codebook,
#'   unless `allow_unknown=TRUE`).
#' @param cdc_var     CDC variable name (must exist in cdc_codebook).
#' @param is_primary  1 if this is the canonical pair, 0 if secondary mapping.
#'   When unset, becomes 1 unless an existing primary already exists for this
#'   (year, state_var), in which case becomes 0.
#' @param source      Free-text source label. Default 'manual_edit'.
#' @param score       Optional confidence score; NA if hand-curated.
#' @param notes       Optional notes string.
#' @param allow_unknown If TRUE, won't error when the var is missing from the
#'   codebook.
#' @return Updated bundle.
#' @export
cw_add_pair <- function(bundle, year, state_var, cdc_var,
                        is_primary = NA, source = "manual_edit",
                        score = NA_real_, notes = "",
                        allow_unknown = FALSE) {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, state_var, "state", allow_unknown)
  .cw_validate_var(bundle, year, cdc_var,   "cdc",   allow_unknown)

  # Reject exact-duplicate pair
  dup <- .cw_find(bundle, year, state_var, cdc_var)
  if (length(dup) > 0) {
    stop(sprintf("Pair already exists: %d %s <-> %s",
                 year, state_var, cdc_var), call. = FALSE)
  }

  # Auto-resolve is_primary
  if (is.na(is_primary)) {
    existing_primary <- bundle$crosswalk |>
      dplyr::filter(.data$year == !!year,
                    .data$state_var == !!state_var,
                    .data$is_primary == 1L)
    is_primary <- if (nrow(existing_primary) == 0L) 1L else 0L
  }

  new_row <- tibble::tibble(
    year       = year,
    state_var  = state_var,
    cdc_var    = cdc_var,
    is_primary = as.integer(is_primary),
    source     = source,
    score      = score,
    notes      = notes
  )

  bundle$crosswalk <- dplyr::bind_rows(bundle$crosswalk, new_row) |>
    dplyr::arrange(.data$year, .data$state_var, .data$cdc_var)

  # If this state_var was previously in state_only, remove it
  if (!is.null(bundle$state_only) && nrow(bundle$state_only) > 0) {
    bundle$state_only <- bundle$state_only |>
      dplyr::filter(!(.data$year == !!year &
                      .data$state_var == !!state_var))
  }
  # If this cdc_var was previously in cdc_only, remove it
  if (!is.null(bundle$cdc_only) && nrow(bundle$cdc_only) > 0) {
    bundle$cdc_only <- bundle$cdc_only |>
      dplyr::filter(!(.data$year == !!year &
                      .data$cdc_var == !!cdc_var))
  }

  bundle
}

#' Remove a pair from the crosswalk
#'
#' @inheritParams cw_add_pair
#' @return Updated bundle.
#' @export
cw_remove_pair <- function(bundle, year, state_var, cdc_var) {
  year <- as.integer(year)
  idx <- .cw_find(bundle, year, state_var, cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf("No such pair: %d %s <-> %s",
                 year, state_var, cdc_var), call. = FALSE)
  }
  removed <- bundle$crosswalk[idx, ]
  bundle$crosswalk <- bundle$crosswalk[-idx, ]

  # If this was the primary and there are remaining secondaries, promote one
  if (any(removed$is_primary == 1L)) {
    remain_idx <- which(bundle$crosswalk$year == year &
                        bundle$crosswalk$state_var == state_var)
    if (length(remain_idx) > 0L) {
      bundle$crosswalk$is_primary[remain_idx[1]] <- 1L
    }
  }

  bundle
}

#' Update fields on an existing pair
#'
#' Only `is_primary`, `source`, `score`, and `notes` are editable. To change
#' `state_var` or `cdc_var`, remove the pair and add a new one.
#'
#' @inheritParams cw_add_pair
#' @return Updated bundle.
#' @export
cw_update_pair <- function(bundle, year, state_var, cdc_var,
                           is_primary = NULL, source = NULL,
                           score = NULL, notes = NULL) {
  year <- as.integer(year)
  idx <- .cw_find(bundle, year, state_var, cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf("No such pair: %d %s <-> %s",
                 year, state_var, cdc_var), call. = FALSE)
  }
  if (length(idx) > 1L) {
    stop("Found multiple matching rows. Crosswalk integrity violated.",
         call. = FALSE)
  }

  if (!is.null(is_primary)) bundle$crosswalk$is_primary[idx] <- as.integer(is_primary)
  if (!is.null(source))     bundle$crosswalk$source[idx]     <- source
  if (!is.null(score))      bundle$crosswalk$score[idx]      <- as.numeric(score)
  if (!is.null(notes))      bundle$crosswalk$notes[idx]      <- notes

  # If we set is_primary=1 here, demote any other primaries for same
  # (year, state_var) so invariant is preserved
  if (isTRUE(bundle$crosswalk$is_primary[idx] == 1L)) {
    others <- which(bundle$crosswalk$year == year &
                    bundle$crosswalk$state_var == state_var &
                    seq_len(nrow(bundle$crosswalk)) != idx)
    if (length(others)) {
      bundle$crosswalk$is_primary[others] <- 0L
    }
  }

  bundle
}

#' Mark a state variable as state-only (no CDC counterpart)
#'
#' Removes any existing crosswalk pairs for this (year, state_var) and adds
#' a row to state_only.csv if not already present.
#'
#' @param bundle    Bundle from `cw_load()`.
#' @param year      Integer year.
#' @param state_var State variable name.
#' @param notes     Optional notes string.
#' @return Updated bundle.
#' @export
cw_mark_state_only <- function(bundle, year, state_var, notes = "") {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, state_var, "state")

  # Remove any existing crosswalk pairs
  drop <- which(bundle$crosswalk$year == year &
                bundle$crosswalk$state_var == state_var)
  if (length(drop) > 0L) bundle$crosswalk <- bundle$crosswalk[-drop, ]

  # Add to state_only if not already there
  if (is.null(bundle$state_only)) {
    bundle$state_only <- tibble::tibble(
      year = integer(), state_var = character(),
      source = character(), notes = character()
    )
  }

  exists <- bundle$state_only$year == year &
            bundle$state_only$state_var == state_var
  if (any(exists)) {
    if (nzchar(notes)) bundle$state_only$notes[which(exists)[1]] <- notes
  } else {
    bundle$state_only <- dplyr::bind_rows(
      bundle$state_only,
      tibble::tibble(year = year, state_var = state_var,
                     source = "manual_edit", notes = notes)
    )
  }
  bundle
}

#' Mark a CDC variable as CDC-only (state didn't field it)
#'
#' Removes any existing crosswalk pairs for this (year, cdc_var) and adds
#' a row to cdc_only.csv if not already present.
#'
#' @param bundle  Bundle from `cw_load()`.
#' @param year    Integer year.
#' @param cdc_var CDC variable name.
#' @param notes   Optional notes string.
#' @return Updated bundle.
#' @export
cw_mark_cdc_only <- function(bundle, year, cdc_var, notes = "") {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, cdc_var, "cdc")

  # Remove any crosswalk pairs
  drop <- which(bundle$crosswalk$year == year &
                bundle$crosswalk$cdc_var == cdc_var)
  if (length(drop) > 0L) bundle$crosswalk <- bundle$crosswalk[-drop, ]

  # Lookup full label/question from codebook for the cdc_only row
  hit <- bundle$cdc_codebook |>
    dplyr::filter(.data$year == !!year, .data$raw_var_name == !!cdc_var) |>
    dplyr::slice(1)

  if (is.null(bundle$cdc_only)) {
    bundle$cdc_only <- tibble::tibble(
      year = integer(), cdc_var = character(),
      cdc_label = character(), cdc_question = character(),
      source = character(), research_finding = character()
    )
  }

  exists <- bundle$cdc_only$year == year &
            bundle$cdc_only$cdc_var == cdc_var
  if (any(exists)) {
    if (nzchar(notes)) {
      bundle$cdc_only$research_finding[which(exists)[1]] <- notes
    }
  } else {
    bundle$cdc_only <- dplyr::bind_rows(
      bundle$cdc_only,
      tibble::tibble(
        year = year, cdc_var = cdc_var,
        cdc_label    = if (nrow(hit)) hit$label    else NA_character_,
        cdc_question = if (nrow(hit)) hit$question else NA_character_,
        source       = "manual_edit",
        research_finding = notes
      )
    )
  }
  bundle
}

# ============================================================================
# Higher-level helpers
# ============================================================================

#' Replace the CDC partner of an existing crosswalk pair
#'
#' Equivalent to remove-then-add but preserves notes/score/source unless
#' overridden.
#'
#' @param bundle       Bundle from `cw_load()`.
#' @param year         Integer year.
#' @param state_var    State variable name.
#' @param old_cdc_var  Current CDC partner.
#' @param new_cdc_var  New CDC partner.
#' @param ...          Optional fields to override (`source`, `score`, `notes`).
#' @return Updated bundle.
#' @export
cw_replace_cdc_partner <- function(bundle, year, state_var,
                                   old_cdc_var, new_cdc_var, ...) {
  year <- as.integer(year)
  idx <- .cw_find(bundle, year, state_var, old_cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf("No pair to replace: %d %s <-> %s",
                 year, state_var, old_cdc_var), call. = FALSE)
  }
  old <- bundle$crosswalk[idx, ]
  new_args <- list(...)

  bundle <- cw_remove_pair(bundle, year, state_var, old_cdc_var)
  bundle <- cw_add_pair(
    bundle, year, state_var, new_cdc_var,
    is_primary = old$is_primary,
    source = new_args$source %||% paste0(old$source, "+replaced"),
    score  = new_args$score  %||% old$score,
    notes  = new_args$notes  %||% paste0(old$notes %||% "",
                                          if (nzchar(old$notes %||% "")) "; " else "",
                                          "was: ", old_cdc_var)
  )
  bundle
}

#' Replace the state partner of an existing crosswalk pair
#'
#' @param bundle         Bundle from `cw_load()`.
#' @param year           Integer year.
#' @param old_state_var  Current state variable.
#' @param cdc_var        CDC variable (the anchor that stays).
#' @param new_state_var  New state variable.
#' @param ...            Optional fields to override.
#' @return Updated bundle.
#' @export
cw_replace_state_partner <- function(bundle, year, old_state_var,
                                     cdc_var, new_state_var, ...) {
  year <- as.integer(year)
  idx <- .cw_find(bundle, year, old_state_var, cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf("No pair to replace: %d %s <-> %s",
                 year, old_state_var, cdc_var), call. = FALSE)
  }
  old <- bundle$crosswalk[idx, ]
  new_args <- list(...)

  bundle <- cw_remove_pair(bundle, year, old_state_var, cdc_var)
  bundle <- cw_add_pair(
    bundle, year, new_state_var, cdc_var,
    is_primary = old$is_primary,
    source = new_args$source %||% paste0(old$source, "+replaced"),
    score  = new_args$score  %||% old$score,
    notes  = new_args$notes  %||% paste0(old$notes %||% "",
                                          if (nzchar(old$notes %||% "")) "; " else "",
                                          "was: ", old_state_var)
  )
  bundle
}
