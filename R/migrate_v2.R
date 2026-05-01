# R/migrate_v2.R
#
# Migrates a v0.1.0 crosswalk to the v0.2.0 schema.

#' Migrate a v0.1.0 crosswalk to v0.2.0 schema
#'
#' Transforms the schema:
#' \itemize{
#'   \item Drops `cdc_var`, `is_primary`, `source`, `score`
#'   \item Adds `is_calculated = 0`, `calculation_yaml = NA`,
#'     `domain = "Unassigned"`, `subdomain = NA`
#'   \item Preserves `concept_id`, `year`, `state_var`, `unverified`,
#'     `notes`
#'   \item Where state_var is non-NA (a real mapping), sets
#'     unverified = 0 (treats existing mappings as user-confirmed).
#'   \item Auto-assigns domains via the CDC codebook lookup.
#' }
#'
#' Backs up the existing crosswalk to \code{<path>.v01.bak} before
#' writing.
#'
#' @param path Path to the v0.1.0 \code{crosswalk.csv} file. Required.
#' @param dry_run Logical. If TRUE, returns the migrated tibble
#'   without writing. Useful for inspection.
#' @return The migrated crosswalk tibble, invisibly.
#' @export
brfss_migrate_crosswalk_to_v2 <- function(path, dry_run = FALSE) {
  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("`path` is required: pass the full path to your v0.1.0 ",
          "crosswalk.csv file.", call. = FALSE)
  }
  cw_fp <- path
  if (!file.exists(cw_fp)) {
    stop("No crosswalk found at: ", cw_fp, call. = FALSE)
  }

  cw <- readr::read_csv(cw_fp, show_col_types = FALSE, progress = FALSE)

  # Detect schema version
  v2_cols <- c("is_calculated", "calculation_yaml", "domain", "subdomain")
  if (all(v2_cols %in% names(cw))) {
    message("Crosswalk is already v0.2.0 schema. Nothing to migrate.")
    return(invisible(cw))
  }

  v1_cols_to_drop <- c("cdc_var", "is_primary", "source", "score")
  message(sprintf("Migrating crosswalk from v0.1.0 to v0.2.0 schema (%d rows)",
                  nrow(cw)))

  # Drop v0.1.0-only columns
  cw <- cw[, !names(cw) %in% v1_cols_to_drop, drop = FALSE]

  # Add v0.2.0 columns
  cw$is_calculated    <- 0L
  cw$calculation_yaml <- NA_character_

  # Auto-assign domains. Use concept_id as both lookup key and raw name
  # (we don't have raw_var_name in v0.1.0 crosswalk).
  if (nrow(cw) > 0L) {
    unique_concepts <- unique(cw$concept_id)
    domains <- brfss_assign_domains(unique_concepts,
                                     raw_var_names = unique_concepts)
    dom_lookup <- setNames(domains$domain,    unique_concepts)
    sub_lookup <- setNames(domains$subdomain, unique_concepts)
    # unname() strips the carry-over names from named-vector indexing
    cw$domain    <- unname(dom_lookup[cw$concept_id])
    cw$subdomain <- unname(sub_lookup[cw$concept_id])
  } else {
    cw$domain    <- character(0)
    cw$subdomain <- character(0)
  }

  # Existing mappings (state_var non-NA) are treated as confirmed.
  if (!"unverified" %in% names(cw)) {
    cw$unverified <- 1L
  }
  cw$unverified <- as.integer(cw$unverified)
  has_mapping <- !is.na(cw$state_var) & nzchar(as.character(cw$state_var))
  cw$unverified[has_mapping] <- 0L

  # Type coercion for clean output
  cw$year       <- as.integer(cw$year)
  cw$concept_id <- as.character(cw$concept_id)
  cw$state_var  <- as.character(cw$state_var)
  if (!"notes" %in% names(cw)) cw$notes <- NA_character_
  cw$notes      <- as.character(cw$notes)

  # Reorder columns to canonical order
  canonical <- c("concept_id", "year", "state_var", "is_calculated",
                 "calculation_yaml", "domain", "subdomain",
                 "unverified", "notes")
  cw <- cw[, canonical, drop = FALSE]

  # Sort: by domain (Unassigned last), concept_id, year
  cw <- cw[order(cw$domain == "Unassigned",
                  cw$domain,
                  cw$concept_id,
                  cw$year), , drop = FALSE]

  if (dry_run) {
    message("Dry run — no files written. Migrated tibble returned.")
    return(invisible(cw))
  }

  # Backup and write
  bak <- paste0(cw_fp, ".v01.bak")
  file.copy(cw_fp, bak, overwrite = TRUE)
  message("Backed up v0.1.0 crosswalk to: ", bak)

  readr::write_csv(cw, cw_fp, na = "")
  message(sprintf("Migrated crosswalk written to: %s (%d rows)",
                  cw_fp, nrow(cw)))

  n_unassigned <- sum(cw$domain == "Unassigned")
  if (n_unassigned > 0L) {
    message(sprintf(
      "Note: %d row(s) have domain = 'Unassigned'. Open the editor to assign.",
      n_unassigned))
  }

  invisible(cw)
}
