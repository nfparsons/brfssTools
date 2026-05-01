# R/codebook.R
#
# Reads codebook documentation CSVs from the registered pool's
# codebook_path. Returns parsed contents with parsed value labels.

#' Load a codebook for one (dataset, year)
#'
#' Reads a properly-formatted codebook CSV from the registered pool's
#' codebook directory (default: \code{<data_path>/documentation/}).
#' See \code{system.file("codebook", "format_spec.md", package = "brfssTools")}
#' for the file format, and
#' \code{system.file("codebook", "codebook_prompt.md", package = "brfssTools")}
#' for an AI-agent prompt that converts your raw codebook (PDF, Word,
#' etc.) into the right format.
#'
#' @param dataset Pool dataset name. If NULL and exactly one pool is
#'   registered, uses that one.
#' @param year Integer year.
#' @param strict Logical. If TRUE (default), errors on validation
#'   failures. If FALSE, warns and returns whatever was parsed.
#' @return A tibble with columns \code{variable_name}, \code{year},
#'   \code{question_text}, \code{value_labels}, \code{notes}, plus a
#'   \code{value_labels_parsed} list-column. Returns NULL if no
#'   codebook is found and \code{strict = FALSE}.
#' @export
brfss_load_codebook <- function(dataset = NULL, year, strict = TRUE) {
  if (missing(year) || !is.numeric(year) || length(year) != 1L) {
    stop("`year` is required and must be a single integer.", call. = FALSE)
  }
  year <- as.integer(year)

  ds  <- .brfss_resolve_dataset(dataset)
  cfg <- .brfss_get_pool(ds)
  doc_dir <- cfg$codebook_path

  if (!dir.exists(doc_dir)) {
    if (strict) {
      stop(sprintf("Codebook directory does not exist: %s", doc_dir),
           "\nSee system.file(\"codebook\", \"format_spec.md\", ",
           "package = \"brfssTools\") for the expected layout.",
           call. = FALSE)
    }
    return(NULL)
  }

  fname <- sprintf("%s_%d_codebook.csv", ds, year)
  fp <- file.path(doc_dir, fname)
  if (!file.exists(fp)) {
    if (strict) {
      stop(sprintf("Codebook not found at: %s", fp), call. = FALSE)
    }
    return(NULL)
  }

  cb <- tryCatch(
    readr::read_csv(fp, show_col_types = FALSE,
                     locale = readr::locale(encoding = "UTF-8"),
                     progress = FALSE),
    error = function(e) {
      stop(sprintf("Failed to read codebook %s: %s",
                    fp, conditionMessage(e)), call. = FALSE)
    }
  )

  required <- c("variable_name", "year")
  optional <- c("question_text", "value_labels", "notes")
  missing_req <- setdiff(required, names(cb))
  if (length(missing_req) > 0L) {
    msg <- sprintf("Codebook %s missing required columns: %s",
                    fname, paste(missing_req, collapse = ", "))
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  for (col in optional) {
    if (!col %in% names(cb)) cb[[col]] <- NA_character_
  }

  cb$variable_name <- as.character(cb$variable_name)
  cb$year          <- as.integer(cb$year)

  bad_year <- cb$year != year & !is.na(cb$year)
  if (any(bad_year)) {
    msg <- sprintf(
      "Codebook %s: %d row(s) have a 'year' value that doesn't match the filename's year (%d).",
      fname, sum(bad_year), year)
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  if (anyDuplicated(cb$variable_name) > 0L) {
    dups <- unique(cb$variable_name[duplicated(cb$variable_name)])
    msg <- sprintf("Codebook %s: duplicate variable_name(s): %s",
                    fname, paste(head(dups, 5), collapse = ", "))
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  cb$value_labels_parsed <- lapply(cb$value_labels, .parse_value_labels)
  cb
}

# Parse "1=Yes;2=No" -> c("1" = "Yes", "2" = "No")
.parse_value_labels <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(character(0))
  parts <- strsplit(x, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(character(0))

  pairs <- strsplit(parts, "=", fixed = TRUE)
  ok <- vapply(pairs, function(p) length(p) == 2L, logical(1L))
  if (any(!ok)) {
    warning(sprintf("value_labels: %d malformed pair(s) skipped: %s",
                     sum(!ok), paste(parts[!ok], collapse = "; ")),
             call. = FALSE)
    pairs <- pairs[ok]
  }
  if (length(pairs) == 0L) return(character(0))

  codes  <- vapply(pairs, function(p) trimws(p[1]), character(1L))
  labels <- vapply(pairs, function(p) trimws(p[2]), character(1L))
  setNames(labels, codes)
}

#' Show the codebook format spec or AI-agent prompt
#'
#' @param what Either "spec" (default) or "prompt".
#' @return Invisible NULL; prints to console.
#' @export
brfss_codebook_help <- function(what = c("spec", "prompt")) {
  what <- match.arg(what)
  fname <- switch(what,
                  spec   = "format_spec.md",
                  prompt = "codebook_prompt.md")
  fp <- system.file("codebook", fname, package = "brfssTools")
  if (!nzchar(fp) || !file.exists(fp)) {
    stop(sprintf("Could not find codebook %s at: %s", what, fp),
          call. = FALSE)
  }
  cat(readLines(fp, warn = FALSE), sep = "\n")
  invisible(NULL)
}
