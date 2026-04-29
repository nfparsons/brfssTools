# R/matcher.R
#
# Concept matcher engine. Given the year-specific OHA codebook and the
# year-specific CDC National codebook, propose candidate matches for each
# OHA variable to its likely National counterpart (if any), with a score
# and a human-readable reason string.
#
# This produces a *proposal* tibble for human review, not a finalized
# crosswalk. Acceptance is a separate, deliberate step.
#
# Usage:
#   oha <- readr::read_csv(system.file("extdata", "oha_codebook_long.csv",
#                                       package = "brfssTools"))
#   cdc <- readr::read_csv(system.file("extdata", "cdc_codebook_long.csv",
#                                       package = "brfssTools"))
#   matches <- brfss_propose_matches(oha, cdc, year = 2018)
#   matches |> dplyr::group_by(oha_var) |> dplyr::slice_max(score, n = 1)


# ============================================================================
# Internal normalization + parsing
# ============================================================================

# Normalize a variable name for comparison. CDC sometimes prefixes calculated
# variables with `_` (e.g. `_BMI5`) where OHA strips it (e.g. `BMI5`).
.norm_var_name <- function(x) {
  x <- as.character(x)
  x <- sub("^_+", "", x)
  tolower(x)
}

# Normalize a question label for token-level comparison. Strips OHA's
# leading questionnaire-position prefix (e.g. "M31.1.", "MACE.01.",
# "OR91.04.", "Q2.1"), lowercases, and reduces punctuation/whitespace.
.norm_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  # Strip leading "M31.1." / "MACE.01." / "OR195.01D." / "C8.2." / "Q2.1" etc.
  x <- sub("^\\s*[A-Z]+[0-9]*[A-Z]*\\.[0-9]+[A-Za-z]?\\.?\\s*", "", x)
  x <- tolower(x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Tokenize a normalized label, dropping common stopwords + survey filler.
.tokenize <- function(x) {
  x <- .norm_label(x)
  toks <- strsplit(x, "\\s+")[[1]]
  stop <- c("a","an","the","of","to","in","is","are","was","were","be","been",
            "do","did","does","not","you","your","yours","i","me","my","we",
            "us","our","ours","they","them","their","this","that","these",
            "those","for","or","and","at","on","by","with","about","as","if",
            "then","than","so","but","have","had","has","ever","told","still",
            "any","some","many","much","how","what","which","when","where",
            "would","could","should","say","said","please","past","days","day",
            "during","question","prologue","read")
  toks <- toks[nchar(toks) > 1L & !(toks %in% stop)]
  toks
}

# Token-set similarity (Jaccard).
.token_sim <- function(a, b) {
  ta <- unique(.tokenize(a))
  tb <- unique(.tokenize(b))
  if (length(ta) == 0L || length(tb) == 0L) return(0)
  inter <- length(intersect(ta, tb))
  union <- length(union(ta, tb))
  if (union == 0L) 0 else inter / union
}

# Parse a value-labels string like "1 - Yes; 2 - No; 7a - Don't know"
# into a named character vector of code -> label, lowercase.
.parse_value_labels <- function(s) {
  if (is.na(s) || !nzchar(s)) return(setNames(character(), character()))
  pairs <- strsplit(s, "\\s*;\\s*")[[1]]
  out   <- character()
  for (p in pairs) {
    kv <- strsplit(p, "\\s*-\\s*", n = 2L)[[1]]
    if (length(kv) != 2L) next
    code <- sub("[a-zA-Z]+$", "", trimws(kv[1]))   # "7a" -> "7"
    lbl  <- tolower(trimws(kv[2]))
    if (nzchar(code) && nzchar(lbl)) out[code] <- lbl
  }
  out
}

# Compare two parsed value-label sets. Returns a similarity in [0, 1].
# Counts a code matching iff both code and label agree (after lowercasing
# and a small synonym map).
.value_sim <- function(a, b) {
  if (length(a) == 0L || length(b) == 0L) return(0)
  syn <- c(
    "don't know" = "dontknow", "don\u2019t know" = "dontknow",
    "don't know/not sure" = "dontknow", "don\u2019t know/not sure" = "dontknow",
    "don't know / not sure" = "dontknow",
    "not sure" = "dontknow",
    "refused" = "refused", "no" = "no", "yes" = "yes"
  )
  canon <- function(v) {
    v <- gsub("\\s+", " ", trimws(v))
    ifelse(v %in% names(syn), unname(syn[v]), v)
  }
  shared_codes <- intersect(names(a), names(b))
  if (length(shared_codes) == 0L) return(0)
  agree <- sum(canon(a[shared_codes]) == canon(b[shared_codes]))
  total <- length(union(names(a), names(b)))
  agree / total
}

# Damerau-Levenshtein distance via base R `adist`, normalized to similarity.
.name_sim <- function(a, b) {
  a <- .norm_var_name(a); b <- .norm_var_name(b)
  if (!nzchar(a) || !nzchar(b)) return(0)
  d <- as.numeric(utils::adist(a, b))
  m <- max(nchar(a), nchar(b))
  if (m == 0) 0 else 1 - (d / m)
}


# ============================================================================
# brfss_propose_matches()
# ============================================================================

#' Propose National BRFSS matches for OHA Oregon variables
#'
#' @description
#' For each (year, OHA variable), score candidate National (CDC) variables
#' and return a long tibble of proposals for human review. Scoring blends
#' four signals: exact-name, fuzzy-name, question-label similarity, and
#' value-label overlap.
#'
#' @param oha_cb   Tibble of the OHA codebook (columns: year, raw_var_name,
#'   label, value_labels, ...).
#' @param cdc_cb   Tibble of the CDC National codebook with the same columns.
#' @param year     Optional integer vector of years to restrict to. `NULL`
#'   (default) considers all years that appear in both inputs.
#' @param top_k    Maximum candidates returned per (year, OHA var). Default 5.
#' @param min_score Drop candidates whose total score is below this. Default
#'   0.30 to leave the noise floor visible.
#' @param weights  Named list of weights for the four signals.
#'
#' @return A tibble with columns: year, oha_var, oha_label, cdc_var,
#'   cdc_label, name_sim, label_sim, values_sim, score, reason. One row per
#'   surviving candidate.
#' @export
brfss_propose_matches <- function(oha_cb,
                                  cdc_cb,
                                  year      = NULL,
                                  top_k     = 5L,
                                  min_score = 0.30,
                                  weights   = list(
                                    name_exact = 1.0,
                                    name_fuzzy = 0.4,
                                    label_sim  = 0.5,
                                    values_sim = 0.5
                                  )) {

  required <- c("year", "raw_var_name", "label", "value_labels")
  for (col in required) {
    if (!col %in% names(oha_cb)) stop("oha_cb missing column: ", col, call. = FALSE)
    if (!col %in% names(cdc_cb)) stop("cdc_cb missing column: ", col, call. = FALSE)
  }

  years_to_use <- if (is.null(year)) {
    sort(intersect(unique(oha_cb$year), unique(cdc_cb$year)))
  } else {
    sort(intersect(as.integer(year),
                   intersect(unique(oha_cb$year), unique(cdc_cb$year))))
  }
  if (length(years_to_use) == 0L) {
    warning("No overlapping years between OHA and CDC codebooks.", call. = FALSE)
    return(.empty_proposal())
  }

  out_parts <- vector("list", length(years_to_use))

  for (i in seq_along(years_to_use)) {
    yr   <- years_to_use[i]
    oha  <- dplyr::filter(oha_cb, .data$year == yr)
    cdc  <- dplyr::filter(cdc_cb, .data$year == yr)
    if (nrow(oha) == 0L || nrow(cdc) == 0L) next

    # Pre-parse value labels for CDC side once per year.
    cdc$.values_parsed <- lapply(cdc$value_labels, .parse_value_labels)
    oha$.values_parsed <- lapply(oha$value_labels, .parse_value_labels)

    # Pre-normalize CDC var names once per year.
    cdc$.name_norm <- .norm_var_name(cdc$raw_var_name)

    # For each OHA variable, score against every CDC variable in the same year.
    proposals <- vector("list", nrow(oha))
    for (j in seq_len(nrow(oha))) {
      o_row    <- oha[j, ]
      o_name_n <- .norm_var_name(o_row$raw_var_name)

      n_sim <- vapply(cdc$.name_norm, function(c_name_n) {
        if (identical(o_name_n, c_name_n)) return(1)
        if (!nzchar(o_name_n) || !nzchar(c_name_n)) return(0)
        d <- as.numeric(utils::adist(o_name_n, c_name_n))
        m <- max(nchar(o_name_n), nchar(c_name_n))
        1 - (d / m)
      }, numeric(1))

      l_sim <- vapply(cdc$label, function(cl) .token_sim(o_row$label, cl),
                      numeric(1))

      v_sim <- vapply(seq_along(cdc$.values_parsed), function(k) {
        .value_sim(o_row$.values_parsed[[1]], cdc$.values_parsed[[k]])
      }, numeric(1))

      # Compose: exact-name weight kicks in only at full identity.
      name_exact_term <- ifelse(n_sim >= 0.999, weights$name_exact, 0)
      name_fuzzy_term <- weights$name_fuzzy * n_sim
      label_term      <- weights$label_sim  * l_sim
      values_term     <- weights$values_sim * v_sim

      total <- name_exact_term + name_fuzzy_term + label_term + values_term

      # Build candidate frame for this OHA row
      cand <- tibble::tibble(
        year       = yr,
        oha_var    = o_row$raw_var_name,
        oha_label  = o_row$label,
        cdc_var    = cdc$raw_var_name,
        cdc_label  = cdc$label,
        name_sim   = n_sim,
        label_sim  = l_sim,
        values_sim = v_sim,
        score      = total
      )
      cand <- dplyr::filter(cand, .data$score >= min_score)
      cand <- dplyr::slice_max(cand, .data$score, n = top_k, with_ties = FALSE)
      proposals[[j]] <- cand
    }

    out_parts[[i]] <- dplyr::bind_rows(proposals)
  }

  out <- dplyr::bind_rows(out_parts)
  if (nrow(out) == 0L) return(.empty_proposal())

  out$reason <- vapply(seq_len(nrow(out)), function(i) {
    bits <- c()
    if (out$name_sim[i]   >= 0.999) bits <- c(bits, "name=exact")
    else if (out$name_sim[i] >= 0.7) bits <- c(bits, sprintf("name~%.2f", out$name_sim[i]))
    if (out$label_sim[i]  >= 0.5)   bits <- c(bits, sprintf("label~%.2f", out$label_sim[i]))
    if (out$values_sim[i] >= 0.5)   bits <- c(bits, sprintf("values~%.2f", out$values_sim[i]))
    paste(bits, collapse = ", ")
  }, character(1))

  dplyr::arrange(out, .data$year, .data$oha_var, dplyr::desc(.data$score))
}

.empty_proposal <- function() {
  tibble::tibble(
    year       = integer(),
    oha_var    = character(),
    oha_label  = character(),
    cdc_var    = character(),
    cdc_label  = character(),
    name_sim   = numeric(),
    label_sim  = numeric(),
    values_sim = numeric(),
    score      = numeric(),
    reason     = character()
  )
}
