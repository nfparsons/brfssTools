# R/sanitize.R
#
# Helpers for sanitizing variable names into R-friendly concept_ids.
# A concept_id must be a valid R name (letters/digits/underscores, no
# leading digit) and ideally short enough to be readable in a heatmap.

#' Sanitize a variable name into an R-friendly concept_id
#'
#' Rules applied, in order:
#' \enumerate{
#'   \item Strip a single leading underscore (CDC's `_RACE` -> `RACE`)
#'   \item Replace any non-alphanumeric, non-underscore character with `_`
#'   \item Collapse runs of `_` to a single `_`
#'   \item Strip trailing `_`
#'   \item If result starts with a digit, prepend `X`
#'   \item Truncate to 63 characters (R's default name limit)
#' }
#'
#' @param x Character vector of raw variable names.
#' @param resolve_collisions Logical. If TRUE (default), and two raw names
#'   sanitize to the same concept_id, append `_2`, `_3`, etc. to
#'   disambiguate. If FALSE, returns whatever the rules produce (may have
#'   duplicates).
#' @return Character vector of sanitized concept_ids, same length as `x`.
#' @export
#' @examples
#' \dontrun{
#' sanitize_concept_id(c("_RACE", "MENTHLTH", "X_ACE_1", "1AGE", "FOO BAR"))
#' # "RACE", "MENTHLTH", "X_ACE_1", "X1AGE", "FOO_BAR"
#'
#' # Collision handling: both "_RACE" and "RACE" are present
#' sanitize_concept_id(c("_RACE", "RACE"))
#' # "RACE", "RACE_2"
#' }
sanitize_concept_id <- function(x, resolve_collisions = TRUE) {
  if (length(x) == 0L) return(character(0))
  out <- as.character(x)

  # Rule 1: strip single leading underscore
  out <- sub("^_", "", out)

  # Rule 2: replace non-[A-Za-z0-9_] with _
  out <- gsub("[^A-Za-z0-9_]", "_", out)

  # Rule 3: collapse multiple underscores
  out <- gsub("_+", "_", out)

  # Rule 4: strip trailing underscores
  out <- sub("_+$", "", out)

  # Rule 5: prepend X if starts with digit
  starts_with_digit <- grepl("^[0-9]", out)
  out[starts_with_digit] <- paste0("X", out[starts_with_digit])

  # Empty results (e.g., from "___" input) get a placeholder
  out[!nzchar(out)] <- "VAR"

  # Rule 6: truncate to 63 chars
  out <- substr(out, 1, 63)

  # Resolve collisions
  if (resolve_collisions && anyDuplicated(out)) {
    dup_table <- table(out)
    dup_names <- names(dup_table)[dup_table > 1L]
    for (nm in dup_names) {
      idx <- which(out == nm)
      # Leave the first occurrence; append _2, _3, ... to subsequent
      for (i in seq_along(idx)[-1]) {
        out[idx[i]] <- paste0(nm, "_", i)
      }
    }
  }

  out
}
