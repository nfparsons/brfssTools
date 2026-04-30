# R/pull.R
#
# brfss_pull(): the workhorse data extraction function.
#
# Reads the user's crosswalk bundle (cw_load), resolves the requested
# concepts/domains/tags into a set of raw columns to fetch, loads the data
# from the registered pool, applies any user-defined transformations
# (YAML categorical maps and functional .R files), and returns a tidy
# tibble.

#' Pull harmonized BRFSS data
#'
#' Resolves a request into raw column lookups and transformation calls,
#' then loads, harmonizes, and returns the data.
#'
#' Three ways to specify what you want, all of which can combine (results
#' are the union):
#' \describe{
#'   \item{`concepts`}{Vector of concept IDs and/or transformation names.}
#'   \item{`domains`}{Vector of domain names from the taxonomy.}
#'   \item{`tags`}{Named list of tag filters,
#'     e.g., `list(population = "adult")`.}
#' }
#'
#' @param concepts Character vector of concept IDs and/or transformation names.
#' @param domains  Character vector of domain names.
#' @param tags     Named list of tag filters.
#' @param years    Integer vector. Default: all years in the crosswalk.
#' @param states   Character (USPS) or numeric (FIPS); for `source = "cdc"`.
#' @param source   `"state"` or `"cdc"`. Auto-detects from registered pools.
#' @param core_demographics If TRUE (default), include AGE, SEX, race
#'   (if user has a transformation), and survey design columns.
#' @param keep_inputs If FALSE (default), drops intermediate raw columns.
#' @param output   `"wide"` (default) or `"long"`.
#' @param path     Optional config dir override.
#' @return A tibble.
#' @export
brfss_pull <- function(concepts          = NULL,
                       domains           = NULL,
                       tags              = NULL,
                       years             = NULL,
                       states            = NULL,
                       source            = NULL,
                       core_demographics = TRUE,
                       keep_inputs       = FALSE,
                       output            = c("wide", "long"),
                       path              = NULL) {

  output <- match.arg(output)

  if (is.null(concepts) && is.null(domains) && is.null(tags) &&
      !isTRUE(core_demographics)) {
    stop("Specify at least one of: concepts, domains, tags. ",
         "Or set core_demographics = TRUE.", call. = FALSE)
  }

  bundle <- cw_load(path)
  source <- .resolve_source(source)

  request_names <- .build_request_set(
    bundle, concepts, domains, tags,
    core_demographics = core_demographics
  )
  if (length(request_names) == 0L) {
    stop("Nothing to pull \u2014 the request set is empty.", call. = FALSE)
  }

  classified <- .classify_request(bundle, request_names, path = path)

  if (is.null(years)) {
    years <- sort(unique(bundle$crosswalk$year))
  } else {
    years <- as.integer(years)
  }

  fips <- .resolve_states(states)

  per_year_dfs <- list()
  for (yr in years) {
    yr_df <- .pull_one_year(
      bundle      = bundle,
      year        = yr,
      classified  = classified,
      source      = source,
      fips        = fips,
      keep_inputs = keep_inputs,
      path        = path
    )
    if (!is.null(yr_df)) {
      yr_df$year <- as.integer(yr)
      per_year_dfs[[as.character(yr)]] <- yr_df
    }
  }

  if (length(per_year_dfs) == 0L) {
    warning("No data loaded for any requested year.", call. = FALSE)
    return(tibble::tibble())
  }

  result <- dplyr::bind_rows(per_year_dfs)

  design_cols <- c("year",
                   intersect(c("SEQNO", "_STATE", ".weight",
                               ".strata", ".psu"), names(result)))
  result <- result[, c(design_cols,
                        setdiff(names(result), design_cols))]

  if (output == "long") {
    result <- .pivot_to_long(result, request_names)
  }

  tibble::as_tibble(result)
}

# ============================================================================
# Internals: request resolution
# ============================================================================

.build_request_set <- function(bundle, concepts, domains, tags,
                               core_demographics = TRUE) {
  out <- character(0)
  if (!is.null(concepts)) out <- c(out, as.character(concepts))
  if (!is.null(domains))  out <- c(out, .concepts_in_domains(bundle, domains))
  if (!is.null(tags)) {
    if (!is.list(tags) || is.null(names(tags))) {
      stop("`tags` must be a named list (e.g., list(population = 'adult')).",
           call. = FALSE)
    }
    out <- c(out, .concepts_matching_tags(bundle, tags))
  }
  if (isTRUE(core_demographics)) {
    out <- c(out, .core_demographic_concepts(bundle))
  }
  unique(out)
}

.concepts_in_domains <- function(bundle, domains) {
  ct_path <- file.path(bundle$.path, "concept_taxonomy.csv")
  if (file.exists(ct_path)) {
    ct <- readr::read_csv(ct_path, show_col_types = FALSE)
    if (all(c("concept_id", "domain") %in% names(ct))) {
      hits <- ct$concept_id[ct$domain %in% domains & !is.na(ct$concept_id)]
      return(unique(hits))
    }
  }
  warning("No concept_taxonomy.csv found in config dir; ",
          "domain-based requests need this file.", call. = FALSE)
  character(0)
}

.concepts_matching_tags <- function(bundle, tags) {
  ct_path <- file.path(bundle$.path, "concept_taxonomy.csv")
  if (!file.exists(ct_path)) {
    warning("No concept_taxonomy.csv found in config dir; ",
            "tag-based requests need this file.", call. = FALSE)
    return(character(0))
  }
  ct <- readr::read_csv(ct_path, show_col_types = FALSE)
  hits <- rep(TRUE, nrow(ct))
  for (tag_name in names(tags)) {
    if (!tag_name %in% names(ct)) {
      warning(sprintf("Tag '%s' is not a column in concept_taxonomy.csv; ignoring.",
                      tag_name), call. = FALSE)
      next
    }
    hits <- hits & ct[[tag_name]] %in% tags[[tag_name]]
  }
  unique(ct$concept_id[hits & !is.na(ct$concept_id)])
}

.core_demographic_concepts <- function(bundle) {
  out <- c("AGE", "SEX")
  trans_dir <- file.path(bundle$.path, "transformations")
  if (dir.exists(trans_dir)) {
    if (file.exists(file.path(trans_dir, "race.yaml")) ||
        file.exists(file.path(trans_dir, "race.R"))) {
      out <- c(out, "race")
    }
  }
  unique(out)
}

# ============================================================================
# Internals: classification
# ============================================================================

.classify_request <- function(bundle, names_vec, path = NULL) {
  cfg <- path %||% bundle$.path
  trans_dir <- file.path(cfg, "transformations")
  cw_concepts <- unique(bundle$crosswalk$concept_id)

  classify_one <- function(nm) {
    yaml_fp <- file.path(trans_dir, paste0(nm, ".yaml"))
    r_fp    <- file.path(trans_dir, paste0(nm, ".R"))
    if (file.exists(yaml_fp)) {
      return(list(name = nm, kind = "yaml", path = yaml_fp))
    }
    if (file.exists(r_fp) && !.is_autogen_r(r_fp)) {
      return(list(name = nm, kind = "r", path = r_fp))
    }
    if (nm %in% cw_concepts) {
      return(list(name = nm, kind = "concept", path = NA_character_))
    }
    list(name = nm, kind = NA_character_, path = NA_character_)
  }

  classified <- lapply(names_vec, classify_one)
  unknown <- vapply(classified, function(x) is.na(x$kind), logical(1))
  if (any(unknown)) {
    nms <- vapply(classified[unknown], `[[`, character(1), "name")
    warning("Unknown name(s), skipped: ", paste(nms, collapse = ", "), "\n",
            "These don't appear as concept_ids in the crosswalk and don't ",
            "have a transformation file.", call. = FALSE)
  }
  classified[!unknown]
}

.is_autogen_r <- function(fp) {
  if (!file.exists(fp)) return(FALSE)
  head_lines <- readLines(fp, n = 6, warn = FALSE)
  any(grepl("AUTO-GENERATED from the corresponding .yaml",
            head_lines, fixed = TRUE))
}

# ============================================================================
# Internals: per-year loading and projection
# ============================================================================

.pull_one_year <- function(bundle, year, classified, source, fips,
                           keep_inputs, path) {

  raw_cols_needed <- character(0)
  any_r_transformation <- FALSE
  concept_to_raw <- list()

  cw <- bundle$crosswalk
  cw_year <- cw[cw$year == year, , drop = FALSE]

  for (cls in classified) {
    if (cls$kind == "concept") {
      hit <- cw_year[cw_year$concept_id == cls$name &
                       cw_year$is_primary == 1L, ]
      if (nrow(hit) >= 1L) {
        raw_col <- if (source == "state") hit$state_var[1] else hit$cdc_var[1]
        if (!is.na(raw_col) && nzchar(raw_col)) {
          raw_cols_needed <- c(raw_cols_needed, raw_col)
          concept_to_raw[[cls$name]] <- raw_col
        }
      }
    } else if (cls$kind == "yaml") {
      spec <- brfss_load_categorical_map(cls$path)
      year_inputs <- .resolve_inputs_for_year(spec, year)
      raw_cols_needed <- c(raw_cols_needed, unname(unlist(year_inputs)))
    } else if (cls$kind == "r") {
      any_r_transformation <- TRUE
    }
  }

  raw_cols_needed <- unique(raw_cols_needed[nzchar(raw_cols_needed)])

  design_cols <- .design_columns_for_source(source, year, bundle)
  raw_cols_needed <- unique(c(raw_cols_needed, design_cols))
  raw_cols_needed <- unique(c("SEQNO", "_STATE", raw_cols_needed))

  raw_df <- .load_year_data(year, source, raw_cols_needed,
                             load_all = any_r_transformation,
                             bundle = bundle)
  if (is.null(raw_df) || nrow(raw_df) == 0L) {
    return(NULL)
  }

  raw_df <- .filter_states(raw_df, fips)
  out <- raw_df

  for (cls in classified) {
    if (cls$kind == "concept") {
      raw_col <- concept_to_raw[[cls$name]]
      if (is.null(raw_col) || !raw_col %in% names(out)) next
      if (cls$name != raw_col) {
        out[[cls$name]] <- out[[raw_col]]
      }
    }
  }

  for (cls in classified) {
    if (cls$kind == "yaml") {
      spec <- brfss_load_categorical_map(cls$path)
      out <- .brfss_apply_categorical_map(out, spec, year)
    }
  }

  for (cls in classified) {
    if (cls$kind == "r") {
      fn <- .brfss_load_transformation(cls$path, cls$name)
      out <- .brfss_apply_transformation(out, cls$name, year, fn)
    }
  }

  requested_cols <- vapply(classified, `[[`, character(1), "name")

  if (length(design_cols)) {
    weight_col <- design_cols[grepl("WT|wt$|weight",
                                     design_cols, ignore.case = TRUE)][1]
    strata_col <- design_cols[grepl("STSTR|strat",
                                     design_cols, ignore.case = TRUE)][1]
    psu_col    <- design_cols[grepl("PSU",
                                     design_cols, ignore.case = TRUE)][1]
    if (!is.na(weight_col) && weight_col %in% names(out)) {
      out$.weight <- out[[weight_col]]
    }
    if (!is.na(strata_col) && strata_col %in% names(out)) {
      out$.strata <- out[[strata_col]]
    }
    if (!is.na(psu_col) && psu_col %in% names(out)) {
      out$.psu <- out[[psu_col]]
    }
  }

  keep_cols <- unique(c(
    "SEQNO", "_STATE",
    requested_cols,
    intersect(c(".weight", ".strata", ".psu"), names(out))
  ))
  if (keep_inputs) {
    keep_cols <- unique(c(keep_cols, raw_cols_needed))
  }
  keep_cols <- intersect(keep_cols, names(out))
  out[, keep_cols, drop = FALSE]
}

.design_columns_for_source <- function(source, year, bundle) {
  if (source == "cdc") {
    return(c("_LLCPWT", "_STSTR", "_PSU"))
  }
  cb <- bundle$state_codebook
  yr_cb <- cb[cb$year == year, , drop = FALSE]
  if (nrow(yr_cb) == 0L) return(character(0))

  weight_hits <- yr_cb$raw_var_name[grepl("WT|weight",
                                           yr_cb$raw_var_name,
                                           ignore.case = TRUE)]
  strata_hits <- yr_cb$raw_var_name[grepl("STSTR|strat",
                                           yr_cb$raw_var_name,
                                           ignore.case = TRUE)]
  psu_hits    <- yr_cb$raw_var_name[grepl("PSU",
                                           yr_cb$raw_var_name,
                                           ignore.case = TRUE)]
  unique(c(head(weight_hits, 1), head(strata_hits, 1), head(psu_hits, 1)))
}

.load_year_data <- function(year, source, cols_needed, load_all = FALSE,
                            bundle = NULL) {
  dataset <- if (source == "cdc") "National" else .infer_state_dataset(bundle)
  df <- tryCatch(
    .fetch_from_pool(dataset, year),
    error = function(e) {
      warning(sprintf(
        "Failed to load year %d from pool '%s': %s",
        year, dataset, conditionMessage(e)),
        call. = FALSE)
      NULL
    }
  )
  if (is.null(df)) return(NULL)

  if (!load_all) {
    have_cols <- intersect(cols_needed, names(df))
    df <- df[, have_cols, drop = FALSE]
  }
  df
}

.infer_state_dataset <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$.path)) return("OR")
  cfg_yaml <- file.path(bundle$.path, "config.yaml")
  if (!file.exists(cfg_yaml)) return("OR")
  cfg <- tryCatch(.read_simple_yaml(cfg_yaml), error = function(e) NULL)
  if (is.null(cfg) || is.null(cfg$state)) return("OR")
  cfg$state
}

# ============================================================================
# Helpers: source resolution, state filter
# ============================================================================

.resolve_source <- function(source) {
  if (is.null(source)) {
    pools <- tryCatch(.brfss_list_pools(), error = function(e) character(0))
    if (any(toupper(pools) != "NATIONAL")) {
      return("state")
    }
    return("cdc")
  }
  match.arg(source, c("state", "cdc"))
}

.brfss_list_pools <- function() {
  pool_env <- tryCatch(get(".brfss_pool_env", envir = asNamespace("brfssTools")),
                       error = function(e) NULL)
  if (is.null(pool_env)) return(character(0))
  ls(pool_env)
}

.brfss_state_fips <- function() {
  tibble::tribble(
    ~fips, ~usps, ~name,
    1L,"AL","Alabama", 2L,"AK","Alaska", 4L,"AZ","Arizona",
    5L,"AR","Arkansas", 6L,"CA","California", 8L,"CO","Colorado",
    9L,"CT","Connecticut", 10L,"DE","Delaware",
    11L,"DC","District of Columbia", 12L,"FL","Florida",
    13L,"GA","Georgia", 15L,"HI","Hawaii", 16L,"ID","Idaho",
    17L,"IL","Illinois", 18L,"IN","Indiana", 19L,"IA","Iowa",
    20L,"KS","Kansas", 21L,"KY","Kentucky", 22L,"LA","Louisiana",
    23L,"ME","Maine", 24L,"MD","Maryland", 25L,"MA","Massachusetts",
    26L,"MI","Michigan", 27L,"MN","Minnesota", 28L,"MS","Mississippi",
    29L,"MO","Missouri", 30L,"MT","Montana", 31L,"NE","Nebraska",
    32L,"NV","Nevada", 33L,"NH","New Hampshire", 34L,"NJ","New Jersey",
    35L,"NM","New Mexico", 36L,"NY","New York", 37L,"NC","North Carolina",
    38L,"ND","North Dakota", 39L,"OH","Ohio", 40L,"OK","Oklahoma",
    41L,"OR","Oregon", 42L,"PA","Pennsylvania", 44L,"RI","Rhode Island",
    45L,"SC","South Carolina", 46L,"SD","South Dakota", 47L,"TN","Tennessee",
    48L,"TX","Texas", 49L,"UT","Utah", 50L,"VT","Vermont",
    51L,"VA","Virginia", 53L,"WA","Washington", 54L,"WV","West Virginia",
    55L,"WI","Wisconsin", 56L,"WY","Wyoming",
    66L,"GU","Guam", 72L,"PR","Puerto Rico", 78L,"VI","U.S. Virgin Islands"
  )
}

.resolve_states <- function(states) {
  if (is.null(states)) return(NULL)
  xref <- .brfss_state_fips()
  if (is.numeric(states)) {
    fips <- as.integer(states)
    bad <- setdiff(fips, xref$fips)
    if (length(bad)) stop(sprintf("Unknown FIPS code(s): %s",
                                   paste(bad, collapse = ", ")), call. = FALSE)
    return(fips)
  }
  if (is.character(states)) {
    up <- toupper(states)
    fips <- xref$fips[match(up, xref$usps)]
    bad <- up[is.na(fips)]
    if (length(bad)) stop(sprintf("Unknown USPS code(s): %s",
                                   paste(bad, collapse = ", ")), call. = FALSE)
    return(as.integer(fips))
  }
  stop("`states` must be numeric (FIPS) or character (USPS).", call. = FALSE)
}

.filter_states <- function(df, fips) {
  if (is.null(fips)) return(df)
  cand <- c("_STATE", "X_STATE", "_state", "x_state")
  hit <- intersect(cand, names(df))
  if (length(hit) == 0L) {
    warning("State filter requested but no `_STATE` column. ",
            "Returning all rows.", call. = FALSE)
    return(df)
  }
  state_col <- df[[hit[1]]]
  keep <- as.integer(state_col) %in% fips
  df[keep, , drop = FALSE]
}

.pivot_to_long <- function(wide_df, requested_cols) {
  value_cols <- intersect(requested_cols, names(wide_df))
  tidyr::pivot_longer(
    wide_df,
    cols      = dplyr::all_of(value_cols),
    names_to  = "concept_id",
    values_to = "value",
    values_transform = list(value = as.character)
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
