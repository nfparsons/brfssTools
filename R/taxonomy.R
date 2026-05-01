# R/taxonomy.R
#
# Domain / subdomain auto-assignment.
#
# The user's variables are named however their state names them. To
# pre-fill domain assignments, we look up the variable name in CDC's
# codebook. If the name matches a CDC variable, we inherit CDC's
# section name (normalized through a hand-curated dictionary). If it
# doesn't match, the domain is "Unassigned" and the user must assign
# from the editor.

# Dictionary mapping raw CDC section names to normalized subdomain names.
# Also encodes the section -> top-level domain hierarchy.
.brfss_section_norm <- function() {
  list(
    # Behavioral Health
    "Healthy Days"                                                   = "Healthy Days",
    "Healthy Days \u2014 Health Related Quality of Life"              = "Healthy Days",
    "Healthy Days - Health-Related Quality of Life"                  = "Healthy Days",
    "Healthy Days (Symptoms)"                                        = "Healthy Days",
    "Adverse Childhood Experience"                                   = "Adverse Childhood Experiences",
    "Adverse Childhood Experiences"                                  = "Adverse Childhood Experiences",
    "Sexual Orientation and Gender Identity (SOGI)"                  = "Sexual Orientation and Gender Identity",
    "Sexual Orientation and Gender Identity"                         = "Sexual Orientation and Gender Identity",
    "Sexual Orientation"                                             = "Sexual Orientation and Gender Identity",
    # Chronic Conditions
    "Arthritis"                                                      = "Arthritis",
    "Arthritis Burden"                                               = "Arthritis",
    "Arthritis Management"                                           = "Arthritis",
    # Cancer
    "Breast and Cervical Cancer Screening"                           = "Breast/Cervical Cancer Screening",
    "Breast/Cervical Cancer Screening"                               = "Breast/Cervical Cancer Screening",
    "Cancer Survivorship"                                            = "Cancer Survivorship",
    "Cancer Survivorship: Course of Treatment"                       = "Cancer Survivorship",
    "Cancer Survivorship: Type of Cancer"                            = "Cancer Survivorship",
    "Cancer Survivorship: Pain Management"                           = "Cancer Survivorship",
    # Other categories
    "Caregiver"                                                      = "Caregiving",
    "Caregiving"                                                     = "Caregiving",
    "Calculated Race Variables"                                      = "Race and Ethnicity",
    "Reactions to Race"                                              = "Race and Ethnicity",
    "Sugar Sweetened Beverages"                                      = "Sugar-Sweetened Beverages",
    "Sugar-Sweetened Beverage"                                       = "Sugar-Sweetened Beverages",
    "Influenza"                                                      = "Immunization",
    "Place of Flu Vaccination"                                       = "Immunization",
    "Tetanus Diphtheria (Tdap) (Adults)"                             = "Immunization",
    "Tetanus, Diphtheria, and Acellular Pertussis (Tdap) (Adults)"   = "Immunization",
    "Tetanus Vaccination"                                            = "Immunization",
    "Shingles"                                                       = "Immunization",
    "Shingles (ZOSTAVAX or ZOS)"                                     = "Immunization",
    "Shingles Vaccination"                                           = "Immunization",
    "HPV Vaccination"                                                = "Immunization",
    "Adult Human Papilloma Virus (HPV)"                              = "Immunization",
    "Adult Human Papillomavirus (HPV) - Vaccination"                 = "Immunization",
    "HPV - Vaccination"                                              = "Immunization",
    # Survey ops
    "Cell Phone Introduction"                                        = "Survey Operations",
    "Cell Phone County"                                              = "Survey Operations",
    "Land Line Introduction"                                         = "Survey Operations",
    "CDCCATI Hispanic Adult"                                         = "Survey Operations",
    "CDCCATI Hispanic Child"                                         = "Survey Operations"
  )
}

# Subdomain -> top-level domain mapping. Loaded from the shipped
# taxonomy.csv if present, else inferred from common knowledge.
.brfss_subdomain_to_domain <- function() {
  pkg_extdata <- .brfss_package_extdata()
  fp <- file.path(pkg_extdata, "taxonomy.csv")
  if (!file.exists(fp)) {
    return(setNames(character(), character()))
  }
  tax <- readr::read_csv(fp, show_col_types = FALSE)
  if (!all(c("domain", "subdomain") %in% names(tax))) {
    warning("taxonomy.csv missing expected 'domain'/'subdomain' columns",
            call. = FALSE)
    return(setNames(character(), character()))
  }
  # Some rows have empty subdomain; skip those for the lookup
  tax <- tax[!is.na(tax$subdomain) & nzchar(tax$subdomain), , drop = FALSE]
  setNames(tax$domain, tax$subdomain)
}

#' Auto-assign domain and subdomain for a vector of concept_ids
#'
#' Looks up each concept_id (or its un-sanitized form) in the shipped
#' CDC codebook's `section_name` column. Matches are normalized through
#' a hand-curated dictionary (\code{.brfss_section_norm}) to a
#' subdomain, then the subdomain -> domain mapping from
#' `taxonomy.csv` is applied.
#'
#' Concepts that don't match any CDC variable get
#' `domain = "Unassigned"`, `subdomain = NA`.
#'
#' @param concept_ids Character vector of concept_ids to assign.
#' @param raw_var_names Optional character vector of the un-sanitized
#'   names (same length as `concept_ids`). Used as a fallback lookup if
#'   the sanitized name doesn't match.
#' @return A tibble with columns `concept_id`, `domain`, `subdomain`.
#' @export
#' @examples
#' \dontrun{
#' brfss_assign_domains(c("MENTHLTH", "POORHLTH", "ACEDEPRS",
#'                        "WEIRD_CUSTOM_VAR"))
#' # MENTHLTH        Behavioral Health  Healthy Days
#' # POORHLTH        Behavioral Health  Healthy Days
#' # ACEDEPRS        Behavioral Health  Adverse Childhood Experiences
#' # WEIRD_CUSTOM_VAR Unassigned         <NA>
#' }
brfss_assign_domains <- function(concept_ids, raw_var_names = NULL) {
  pkg_extdata <- .brfss_package_extdata()
  cdc_fp <- file.path(pkg_extdata, "cdc_codebook.csv")
  if (!file.exists(cdc_fp)) {
    return(tibble::tibble(
      concept_id = concept_ids,
      domain     = "Unassigned",
      subdomain  = NA_character_
    ))
  }

  cdc <- readr::read_csv(cdc_fp, show_col_types = FALSE)
  if (!"section_name" %in% names(cdc)) {
    warning("cdc_codebook.csv missing 'section_name' column; ",
            "domain assignment will mark all as Unassigned.",
            call. = FALSE)
    return(tibble::tibble(
      concept_id = concept_ids,
      domain     = "Unassigned",
      subdomain  = NA_character_
    ))
  }

  # Build raw_var -> first-seen-section_name lookup. A variable can
  # appear in multiple years and (rarely) in different sections; take
  # the most recently seen section.
  cdc_sorted <- cdc[order(-cdc$year), , drop = FALSE]
  cdc_sorted <- cdc_sorted[!duplicated(cdc_sorted$raw_var_name), , drop = FALSE]
  raw_to_section <- setNames(cdc_sorted$section_name, cdc_sorted$raw_var_name)

  # Normalize section -> subdomain
  norm_map <- .brfss_section_norm()
  # Subdomain -> domain
  domain_map <- .brfss_subdomain_to_domain()

  if (is.null(raw_var_names)) {
    raw_var_names <- concept_ids
  }

  n <- length(concept_ids)
  domain    <- rep("Unassigned",  n)
  subdomain <- rep(NA_character_, n)

  # NULL-safe lookup helper (named char vec [[bad_key]] errors; we want NULL)
  safe_lookup <- function(map, key) {
    if (is.null(key) || is.na(key) || !nzchar(key)) return(NULL)
    if (!key %in% names(map)) return(NULL)
    map[[key]]
  }

  for (i in seq_len(n)) {
    # Try lookup by raw name first (more likely to match CDC's data)
    sec <- safe_lookup(raw_to_section, raw_var_names[i])
    if (is.null(sec) || is.na(sec)) {
      # Fall back to sanitized form
      sec <- safe_lookup(raw_to_section, concept_ids[i])
    }
    # Also try with leading underscore restored (e.g., "RACE" -> "_RACE")
    if ((is.null(sec) || is.na(sec)) && !startsWith(raw_var_names[i], "_")) {
      sec <- safe_lookup(raw_to_section, paste0("_", concept_ids[i]))
    }
    if (is.null(sec) || is.na(sec) || !nzchar(sec)) next

    # Normalize section -> subdomain (try literal match, else use raw)
    sd <- safe_lookup(norm_map, sec)
    if (is.null(sd)) sd <- sec
    subdomain[i] <- sd

    # Subdomain -> domain
    d <- safe_lookup(domain_map, sd)
    if (!is.null(d) && nzchar(d)) {
      domain[i] <- d
    }
  }

  tibble::tibble(
    concept_id = concept_ids,
    domain     = domain,
    subdomain  = subdomain
  )
}
