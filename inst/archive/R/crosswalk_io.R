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

# Resolve the canonical extdata path. NEW: uses the hybrid resolver.
# Priority: explicit path > R option > env var > R_user_dir > package extdata.
.cw_default_path <- function() {
  cfg <- tryCatch(brfss_config_path(must_exist = FALSE),
                  error = function(e) NULL)

  # If config dir exists and has at least crosswalk.csv, use it.
  if (!is.null(cfg) && file.exists(file.path(cfg, "crosswalk.csv"))) {
    return(cfg)
  }

  # Otherwise fall back to the package's shipped extdata (read-only sample).
  pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
  if (!is.null(pkg) && file.exists(file.path(pkg, "crosswalk.csv"))) {
    return(pkg)
  }

  stop(
    "No brfssTools config found.\n",
    "Run brfss_init_state(state = ..., state_codebook_path = ...) first, ",
    "or pass an explicit path = argument.",
    call. = FALSE
  )
}

#' Load the crosswalk bundle from disk
#'
#' Reads the crosswalk and supporting files from the resolved config path.
#' By default uses the path resolver (`brfss_config_path()`); pass an
#' explicit `path =` to override.
#'
#' Files split between package extdata and user config:
#' \itemize{
#'   \item Always from package: `cdc_codebook.csv`, `cdc_calculated_vars.csv`,
#'     `cdc_only.csv`, `taxonomy.csv` (unless user has local overrides).
#'   \item Always from config dir: `crosswalk.csv`, `state_codebook.csv`,
#'     `state_only.csv`, `pending.csv`.
#' }
#'
#' @param path Optional config dir override. Default uses `brfss_config_path()`.
#' @return A list of tibbles describing the crosswalk and its supporting data.
#' @export
cw_load <- function(path = NULL) {
  if (is.null(path)) path <- .cw_default_path()
  if (!dir.exists(path)) stop("Path not found: ", path, call. = FALSE)

  read_one <- function(fname, required = TRUE, prefer = c("config","package")) {
    prefer <- match.arg(prefer)

    # Where to look first: 'config' (path) or 'package' (extdata)
    candidates <- character(0)
    pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
    if (prefer == "config") {
      candidates <- c(file.path(path, fname),
                      if (!is.null(pkg)) file.path(pkg, fname))
    } else {
      candidates <- c(if (!is.null(pkg)) file.path(pkg, fname),
                      file.path(path, fname))
    }
    candidates <- candidates[file.exists(candidates)]

    if (length(candidates) == 0L) {
      if (required) stop("Missing canonical file: ", fname, call. = FALSE)
      return(NULL)
    }

    readr::read_csv(candidates[1], show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"))
  }

  bundle <- list(
    crosswalk           = read_one("crosswalk.csv",           prefer = "config"),
    state_only          = read_one("state_only.csv",          prefer = "config"),
    cdc_only            = read_one("cdc_only.csv",            prefer = "package", required = FALSE),
    cdc_codebook        = read_one("cdc_codebook.csv",        prefer = "package"),
    state_codebook      = read_one("state_codebook.csv",      prefer = "config"),
    taxonomy            = read_one("taxonomy.csv",            prefer = "package", required = FALSE),
    pending             = read_one("pending.csv",             prefer = "config",  required = FALSE),
    cdc_calculated_vars = read_one("cdc_calculated_vars.csv", prefer = "package", required = FALSE),
    .path               = path
  )

  # Schema sanity. unverified column is added in 0.1.0; back-fill if a
  # legacy crosswalk doesn't have it yet.
  required_cw <- c("year","state_var","cdc_var","is_primary","source",
                   "score","notes")
  missing_cw <- setdiff(required_cw, names(bundle$crosswalk))
  if (length(missing_cw) > 0) {
    stop("crosswalk.csv missing columns: ",
         paste(missing_cw, collapse=", "), call. = FALSE)
  }
  if (!"unverified" %in% names(bundle$crosswalk)) {
    bundle$crosswalk$unverified <- 0L
  } else {
    # Coerce to integer in case CSV read it as char or logical
    bundle$crosswalk$unverified <- as.integer(bundle$crosswalk$unverified)
    bundle$crosswalk$unverified[is.na(bundle$crosswalk$unverified)] <- 0L
  }

  # Auto-populate concept_id where missing.
  bundle <- .cw_ensure_concept_id(bundle)

  bundle
}

# Add concept_id columns where absent. Pure function: returns updated bundle.
# Existing concept_id values are preserved; only NA / missing-column cases
# get filled.
.cw_ensure_concept_id <- function(bundle) {
  if (!is.null(bundle$crosswalk)) {
    if (!"concept_id" %in% names(bundle$crosswalk)) {
      bundle$crosswalk$concept_id <- NA_character_
    }
    fill <- is.na(bundle$crosswalk$concept_id) |
            !nzchar(bundle$crosswalk$concept_id)
    bundle$crosswalk$concept_id[fill] <- bundle$crosswalk$cdc_var[fill]
    # Reorder so concept_id is first
    bundle$crosswalk <- bundle$crosswalk[, c(
      "concept_id",
      setdiff(names(bundle$crosswalk), "concept_id")
    )]
  }
  if (!is.null(bundle$state_only) && nrow(bundle$state_only) > 0) {
    if (!"concept_id" %in% names(bundle$state_only)) {
      bundle$state_only$concept_id <- NA_character_
    }
    fill <- is.na(bundle$state_only$concept_id) |
            !nzchar(bundle$state_only$concept_id)
    bundle$state_only$concept_id[fill] <- bundle$state_only$state_var[fill]
    bundle$state_only <- bundle$state_only[, c(
      "concept_id",
      setdiff(names(bundle$state_only), "concept_id")
    )]
  }
  if (!is.null(bundle$cdc_only) && nrow(bundle$cdc_only) > 0) {
    if (!"concept_id" %in% names(bundle$cdc_only)) {
      bundle$cdc_only$concept_id <- NA_character_
    }
    fill <- is.na(bundle$cdc_only$concept_id) |
            !nzchar(bundle$cdc_only$concept_id)
    bundle$cdc_only$concept_id[fill] <- bundle$cdc_only$cdc_var[fill]
    bundle$cdc_only <- bundle$cdc_only[, c(
      "concept_id",
      setdiff(names(bundle$cdc_only), "concept_id")
    )]
  }
  bundle
}

# Validate a concept_id string. Must be R-friendly: alphanumeric + underscore,
# no leading digit, non-empty, <=63 chars.
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

#' Save the bundle back to disk with a .bak rotation
#'
#' Before writing, copies the existing crosswalk.csv to crosswalk.csv.bak.
#' Only files that have actually changed in-memory are rewritten.
#'
#' If the bundle was loaded from the package's shipped extdata (read-only),
#' this errors. Run `brfss_init_state()` to copy the sample into your user
#' config dir first.
#'
#' @param bundle Crosswalk bundle from `cw_load()` (potentially mutated).
#' @param path  Optional override of write path. Defaults to the bundle's
#'   loaded path.
#' @return The bundle, invisibly.
#' @export
cw_save <- function(bundle, path = NULL) {
  if (is.null(path)) path <- bundle$.path %||% .cw_default_path()

  # Refuse to write to package extdata
  pkg <- tryCatch(.brfss_package_extdata(), error = function(e) NULL)
  if (!is.null(pkg) && normalizePath(path, mustWork = FALSE) ==
                       normalizePath(pkg, mustWork = FALSE)) {
    stop(
      "Refusing to write to the package's shipped extdata at:\n  ", path, "\n",
      "Run brfss_init_state() to copy the sample into your user config dir, ",
      "then retry. Alternatively, pass an explicit `path =` to write elsewhere.",
      call. = FALSE
    )
  }

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
# Find rows in the crosswalk matching (year, state_var, cdc_var).
# - NA values are treated as literal NA matches (i.e. is.na(state_var) == TRUE).
# - Pass `NULL` to skip filtering on a field.
.cw_find <- function(bundle, year, state_var = NULL, cdc_var = NULL) {
  cw <- bundle$crosswalk
  matches <- cw$year == year
  if (!is.null(state_var)) {
    if (is.na(state_var)) {
      matches <- matches & is.na(cw$state_var)
    } else {
      matches <- matches & !is.na(cw$state_var) & cw$state_var == state_var
    }
  }
  if (!is.null(cdc_var)) {
    if (is.na(cdc_var)) {
      matches <- matches & is.na(cw$cdc_var)
    } else {
      matches <- matches & !is.na(cw$cdc_var) & cw$cdc_var == cdc_var
    }
  }
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
#' @param concept_id  The conceptual identifier this pair belongs to. Defaults
#'   to `cdc_var`. Must be R-friendly: alphanumeric + underscore, no leading
#'   digit.
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
                        concept_id = NULL,
                        is_primary = NA, source = "manual_edit",
                        score = NA_real_, notes = "",
                        allow_unknown = FALSE) {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, state_var, "state", allow_unknown)
  .cw_validate_var(bundle, year, cdc_var,   "cdc",   allow_unknown)

  if (is.null(concept_id) || is.na(concept_id) || !nzchar(concept_id)) {
    concept_id <- cdc_var
  }
  .cw_validate_concept_id(concept_id)

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
    concept_id = concept_id,
    year       = year,
    state_var  = state_var,
    cdc_var    = cdc_var,
    is_primary = as.integer(is_primary),
    source     = source,
    score      = score,
    notes      = notes,
    unverified = 0L
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

#' Map a seeded CDC variable to a state variable
#'
#' Workflow: a CDC-seeded crosswalk row has `state_var = NA` because
#' the user hasn't yet identified the state column for it. This function
#' updates the seeded row in place by setting its `state_var`, optionally
#' updating other fields (concept_id, source, notes), and clearing the
#' `unverified` flag.
#'
#' If you're starting from an unseeded crosswalk (no row exists for this
#' CDC variable in this year), use `cw_add_pair()` instead.
#'
#' @param bundle      Bundle from `cw_load()`.
#' @param year        Integer year.
#' @param cdc_var     CDC variable name (must exist as a seeded row).
#' @param state_var   State variable name (must exist in state_codebook
#'   unless `allow_unknown = TRUE`).
#' @param concept_id  Optional new concept_id. If NULL, leaves it unchanged.
#' @param source      Source label. Default "manual_edit" (replaces "cdc_seed").
#' @param notes       Optional notes string.
#' @param allow_unknown If TRUE, won't error when state_var is missing
#'   from the state codebook for this year.
#' @return Updated bundle.
#' @export
#' @examples
#' \dontrun{
#' cw <- cw_load()
#' # 2024's ASTHMA3 seeded row has state_var = NA; map it to your codebook:
#' cw <- cw_map_seeded(cw, year = 2024, cdc_var = "ASTHMA3",
#'                     state_var = "ASTHMA3")
#' cw_save(cw)
#' }
#' @export
cw_map_seeded <- function(bundle, year, cdc_var, state_var,
                          concept_id = NULL,
                          source = "manual_edit",
                          notes = NULL,
                          allow_unknown = FALSE) {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, state_var, "state", allow_unknown)
  .cw_validate_var(bundle, year, cdc_var,   "cdc",   allow_unknown)

  # Find the seeded row (state_var IS NA, cdc_var matches).
  idx <- .cw_find(bundle, year, state_var = NA, cdc_var = cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf(
      "No seeded row found for year %d, cdc_var '%s'.\n",
      year, cdc_var),
      "If a row exists with a different state_var, use cw_update_pair() ",
      "or cw_add_pair() instead.", call. = FALSE)
  }
  if (length(idx) > 1L) {
    stop(sprintf("Found %d seeded rows for year %d, cdc_var '%s' — ",
                 length(idx), year, cdc_var),
         "crosswalk integrity violated.", call. = FALSE)
  }

  # Confirm no conflicting non-seeded row already exists for this state_var
  conflict <- .cw_find(bundle, year, state_var, cdc_var)
  if (length(conflict) > 0L && !identical(conflict, idx)) {
    stop(sprintf(
      "A non-seeded row already exists for year %d, %s <-> %s. ",
      year, state_var, cdc_var),
      "Remove or update it before mapping the seed.", call. = FALSE)
  }

  if (!is.null(concept_id)) {
    .cw_validate_concept_id(concept_id)
    bundle$crosswalk$concept_id[idx] <- concept_id
  }
  bundle$crosswalk$state_var[idx]  <- state_var
  bundle$crosswalk$source[idx]     <- source
  if (!is.null(notes)) bundle$crosswalk$notes[idx] <- notes
  bundle$crosswalk$unverified[idx] <- 0L

  bundle
}

#' Update fields on an existing pair
#'
#' Only `concept_id`, `is_primary`, `source`, `score`, and `notes` are
#' editable. To change `state_var` or `cdc_var`, remove the pair and add a
#' new one.
#'
#' @inheritParams cw_add_pair
#' @return Updated bundle.
#' @export
cw_update_pair <- function(bundle, year, state_var, cdc_var,
                           concept_id = NULL,
                           is_primary = NULL, source = NULL,
                           score = NULL, notes = NULL,
                           unverified = NULL) {
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

  if (!is.null(concept_id)) {
    .cw_validate_concept_id(concept_id)
    bundle$crosswalk$concept_id[idx] <- concept_id
  }
  if (!is.null(is_primary)) bundle$crosswalk$is_primary[idx] <- as.integer(is_primary)
  if (!is.null(source))     bundle$crosswalk$source[idx]     <- source
  if (!is.null(score))      bundle$crosswalk$score[idx]      <- as.numeric(score)
  if (!is.null(notes))      bundle$crosswalk$notes[idx]      <- notes
  if (!is.null(unverified)) bundle$crosswalk$unverified[idx] <- as.integer(unverified)

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

#' Mark a crosswalk row as verified (clear the unverified flag)
#'
#' Convenience wrapper around `cw_update_pair()` for the common case of
#' confirming a CDC-seeded row's mapping.
#'
#' @param bundle    Bundle from `cw_load()`.
#' @param year      Integer year.
#' @param state_var State variable name. Use `NA` for unmapped seeded rows.
#' @param cdc_var   CDC variable name.
#' @return Updated bundle.
#' @export
cw_verify_pair <- function(bundle, year, state_var, cdc_var) {
  year <- as.integer(year)
  idx <- .cw_find(bundle, year, state_var, cdc_var)
  if (length(idx) == 0L) {
    stop(sprintf("No such pair: %d %s <-> %s",
                 year,
                 ifelse(is.na(state_var), "<NA>", state_var),
                 ifelse(is.na(cdc_var), "<NA>", cdc_var)),
         call. = FALSE)
  }
  bundle$crosswalk$unverified[idx] <- 0L
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
#' @param concept_id Optional concept_id; defaults to `state_var`.
#' @param notes     Optional notes string.
#' @return Updated bundle.
#' @export
cw_mark_state_only <- function(bundle, year, state_var,
                               concept_id = NULL, notes = "") {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, state_var, "state")
  if (is.null(concept_id) || is.na(concept_id) || !nzchar(concept_id)) {
    concept_id <- state_var
  }
  .cw_validate_concept_id(concept_id)

  # Remove any existing crosswalk pairs
  drop <- which(bundle$crosswalk$year == year &
                bundle$crosswalk$state_var == state_var)
  if (length(drop) > 0L) bundle$crosswalk <- bundle$crosswalk[-drop, ]

  # Add to state_only if not already there
  if (is.null(bundle$state_only)) {
    bundle$state_only <- tibble::tibble(
      concept_id = character(), year = integer(), state_var = character(),
      source = character(), notes = character()
    )
  }

  exists <- bundle$state_only$year == year &
            bundle$state_only$state_var == state_var
  if (any(exists)) {
    if (nzchar(notes)) bundle$state_only$notes[which(exists)[1]] <- notes
    bundle$state_only$concept_id[which(exists)[1]] <- concept_id
  } else {
    bundle$state_only <- dplyr::bind_rows(
      bundle$state_only,
      tibble::tibble(concept_id = concept_id, year = year,
                     state_var = state_var,
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
#' @param concept_id Optional concept_id; defaults to `cdc_var`.
#' @param notes   Optional notes string.
#' @return Updated bundle.
#' @export
cw_mark_cdc_only <- function(bundle, year, cdc_var,
                             concept_id = NULL, notes = "") {
  year <- as.integer(year)
  .cw_validate_var(bundle, year, cdc_var, "cdc")
  if (is.null(concept_id) || is.na(concept_id) || !nzchar(concept_id)) {
    concept_id <- cdc_var
  }
  .cw_validate_concept_id(concept_id)

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
      concept_id = character(), year = integer(), cdc_var = character(),
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
    bundle$cdc_only$concept_id[which(exists)[1]] <- concept_id
  } else {
    bundle$cdc_only <- dplyr::bind_rows(
      bundle$cdc_only,
      tibble::tibble(
        concept_id = concept_id, year = year, cdc_var = cdc_var,
        cdc_label    = if (nrow(hit)) hit$label    else NA_character_,
        cdc_question = if (nrow(hit)) hit$question else NA_character_,
        source       = "manual_edit",
        research_finding = notes
      )
    )
  }
  bundle
}

#' Rename a concept across the crosswalk
#'
#' Updates every row in `crosswalk`, `state_only`, and `cdc_only` whose
#' `concept_id` matches `from` to use `to` instead. Useful for the
#' "ACEHURT1 -> ACEHURT" pattern: collapse year-suffixed CDC/state names
#' into a single semantic concept that brfss_pull() can address.
#'
#' @param bundle Bundle from `cw_load()`.
#' @param from   The current concept_id.
#' @param to     The new concept_id (validated for R-friendly naming).
#' @return Updated bundle.
#' @export
cw_rename_concept <- function(bundle, from, to) {
  if (is.null(from) || !nzchar(from)) {
    stop("'from' concept_id required.", call. = FALSE)
  }
  .cw_validate_concept_id(to)

  if (from == to) return(bundle)  # no-op

  hits <- 0L
  if (!is.null(bundle$crosswalk) && nrow(bundle$crosswalk) > 0) {
    sel <- bundle$crosswalk$concept_id == from & !is.na(bundle$crosswalk$concept_id)
    bundle$crosswalk$concept_id[sel] <- to
    hits <- hits + sum(sel)
  }
  if (!is.null(bundle$state_only) && nrow(bundle$state_only) > 0) {
    sel <- bundle$state_only$concept_id == from & !is.na(bundle$state_only$concept_id)
    bundle$state_only$concept_id[sel] <- to
    hits <- hits + sum(sel)
  }
  if (!is.null(bundle$cdc_only) && nrow(bundle$cdc_only) > 0) {
    sel <- bundle$cdc_only$concept_id == from & !is.na(bundle$cdc_only$concept_id)
    bundle$cdc_only$concept_id[sel] <- to
    hits <- hits + sum(sel)
  }

  if (hits == 0L) {
    warning(sprintf("No rows matched concept_id == '%s'", from), call. = FALSE)
  }
  bundle
}

#' Recompute concept_ids using union-find grouping
#'
#' Re-derives `concept_id` for every row by grouping rows that share an
#' OHA name OR a CDC name, then assigning each group the modal CDC name
#' (or modal state name if no CDC is present). Use this when you've
#' manually merged things and want to consolidate.
#'
#' @param bundle Bundle from `cw_load()`.
#' @return Updated bundle.
#' @export
cw_recompute_concept_ids <- function(bundle) {
  facts <- list()
  cw <- bundle$crosswalk
  if (!is.null(cw) && nrow(cw) > 0) {
    for (i in seq_len(nrow(cw))) {
      facts[[length(facts)+1L]] <- list(
        kind = "cw", row = i,
        state_var = cw$state_var[i], cdc_var = cw$cdc_var[i]
      )
    }
  }
  if (!is.null(bundle$state_only) && nrow(bundle$state_only) > 0) {
    for (i in seq_len(nrow(bundle$state_only))) {
      facts[[length(facts)+1L]] <- list(
        kind = "so", row = i,
        state_var = bundle$state_only$state_var[i], cdc_var = ""
      )
    }
  }
  if (!is.null(bundle$cdc_only) && nrow(bundle$cdc_only) > 0) {
    for (i in seq_len(nrow(bundle$cdc_only))) {
      facts[[length(facts)+1L]] <- list(
        kind = "co", row = i,
        state_var = "", cdc_var = bundle$cdc_only$cdc_var[i]
      )
    }
  }
  if (length(facts) == 0L) return(bundle)

  parent <- seq_along(facts)
  find <- function(x) { while (parent[x] != x) { parent[x] <<- parent[parent[x]]; x <- parent[x] }; x }
  union_ <- function(a,b) { ra <- find(a); rb <- find(b); if (ra != rb) parent[ra] <<- rb }

  by_state <- list(); by_cdc <- list()
  for (k in seq_along(facts)) {
    f <- facts[[k]]
    if (nzchar(f$state_var)) by_state[[f$state_var]] <- c(by_state[[f$state_var]], k)
    if (nzchar(f$cdc_var))   by_cdc  [[f$cdc_var]]   <- c(by_cdc  [[f$cdc_var]],   k)
  }
  for (ids in by_state) if (length(ids) > 1) for (k in 2:length(ids)) union_(ids[1], ids[k])
  for (ids in by_cdc)   if (length(ids) > 1) for (k in 2:length(ids)) union_(ids[1], ids[k])

  group_id <- vapply(seq_along(facts), find, integer(1))

  # For each group, choose modal cdc_var (prefer) or modal state_var
  group_label <- character(length(unique(group_id)))
  for (g in unique(group_id)) {
    members <- which(group_id == g)
    cdcs <- vapply(members, function(k) facts[[k]]$cdc_var, character(1))
    cdcs <- cdcs[nzchar(cdcs)]
    if (length(cdcs)) {
      label <- names(sort(table(cdcs), decreasing = TRUE))[1]
    } else {
      svs <- vapply(members, function(k) facts[[k]]$state_var, character(1))
      svs <- svs[nzchar(svs)]
      label <- if (length(svs)) names(sort(table(svs), decreasing = TRUE))[1]
               else NA_character_
    }
    # Sanitize
    label <- gsub("[^A-Za-z0-9_]", "_", label)
    if (grepl("^[0-9]", label)) label <- paste0("X", label)
    group_label[as.character(g)] <- label
  }

  for (k in seq_along(facts)) {
    f <- facts[[k]]
    new_cid <- group_label[as.character(group_id[k])]
    if (f$kind == "cw") bundle$crosswalk$concept_id[f$row]  <- new_cid
    if (f$kind == "so") bundle$state_only$concept_id[f$row] <- new_cid
    if (f$kind == "co") bundle$cdc_only$concept_id[f$row]   <- new_cid
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
