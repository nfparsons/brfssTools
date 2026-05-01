# inst/shiny/editor/server.R
#
# Reactive state model:
#   bundle()         - the loaded crosswalk bundle (reactiveVal)
#   dirty()          - has anything changed since last save
#   selected_cid()   - currently selected concept identifier (string)
#   selected_year()  - which year cell within the selected concept
#   active_domain()  - which domain is shown in the heatmap
#
# Concept identifier (cid): a stable string like
#   "PAIRED|state_var|cdc_var" or "STATE_ONLY|state_var" or "CDC_ONLY|cdc_var"
# We build it for every visible row.

library(shiny)
library(DT)
library(htmltools)
library(dplyr)

# Helpers loaded from package's R/ when devtools::load_all'd; when the app
# is run via system.file path, the package is installed and these are
# available via brfssTools::.
# We resolve via getNamespace() so this works under both devtools::load_all()
# and a fully installed package.
.brfsstools_ns <- function() {
  if ("brfssTools" %in% loadedNamespaces()) {
    return(asNamespace("brfssTools"))
  }
  getNamespace("brfssTools")
}
.ns <- .brfsstools_ns()

cw_load          <- .ns$cw_load
cw_save          <- .ns$cw_save
cw_add_pair      <- .ns$cw_add_pair
cw_remove_pair   <- .ns$cw_remove_pair
cw_update_pair   <- .ns$cw_update_pair
cw_mark_state_only <- .ns$cw_mark_state_only
cw_mark_cdc_only   <- .ns$cw_mark_cdc_only
cw_replace_cdc_partner   <- .ns$cw_replace_cdc_partner
cw_replace_state_partner <- .ns$cw_replace_state_partner
cw_rename_concept        <- .ns$cw_rename_concept
cw_map_seeded            <- .ns$cw_map_seeded
cw_verify_pair           <- .ns$cw_verify_pair

# Demographics tab functions (archived in v0.2.0; stubs if not exported).
# Note: editor's Demographics tab is being torn out in Stage B; these
# are tolerance shims for the in-between state.
.dummy_demographic_status <- function(...) {
  data.frame(demographic = character(0),
             status      = character(0),
             kind        = character(0),
             path        = character(0))
}
.dummy_demographic_templates <- function(...) {
  data.frame(demographic = character(0),
             template    = character(0),
             description = character(0),
             path        = character(0))
}
brfss_setup_demographic <- if (!is.null(.ns$brfss_setup_demographic))
                             .ns$brfss_setup_demographic else
                             function(...) stop(
                               "brfss_setup_demographic was archived in v0.2.0",
                               call. = FALSE)
brfss_list_demographic_templates <- if (!is.null(.ns$brfss_list_demographic_templates))
                                      .ns$brfss_list_demographic_templates else
                                      .dummy_demographic_templates
brfss_demographic_status <- if (!is.null(.ns$brfss_demographic_status))
                              .ns$brfss_demographic_status else
                              .dummy_demographic_status
brfss_render_transformation_code <- if (!is.null(.ns$brfss_render_transformation_code))
                                      .ns$brfss_render_transformation_code else
                                      function(...) invisible(NULL)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# Demographics tab renderers
# ============================================================================

# Left nav for the Demographics view: just a header, since the demographics
# checklist lives in the main pane. Future: could show "Status" links here.
.render_demographics_nav <- function() {
  tagList(
    h2("Demographics"),
    div(style = "padding: 0 16px; font-size: 12px; color: #666; line-height: 1.5;",
        "Set up your study's demographic variables. ",
        "These compose into derived columns that ",
        tags$code("brfss_pull()"),
        " resolves automatically."),
    div(style = "padding: 14px 16px; font-size: 11px; color: #888;",
        "Big six: age, sex, race, ethnicity, education, income.")
  )
}

# Main pane for the Demographics view.
.render_demographics_pane <- function(input, bundle) {
  status <- brfss_demographic_status()

  big_six <- c("age", "sex", "race", "ethnicity", "education", "income")
  pretty_names <- c(age = "Age", sex = "Sex", race = "Race",
                    ethnicity = "Ethnicity", education = "Education",
                    income = "Income")

  # Inferred current template name from the YAML basename of the original
  # template (we store the template name in a brief comment, but more
  # robustly, just say "configured" if a file exists).
  rows <- lapply(big_six, function(d) {
    info <- status[status$demographic == d, ]
    is_configured <- nrow(info) == 1 && info$status == "configured"
    kind <- if (is_configured) info$kind else NA_character_
    fp <- if (is_configured) info$path else NA_character_

    div(class = "demographic-row",
        style = paste0(
          "display:flex; align-items:center; padding:14px 16px; ",
          "background:white; border-radius:6px; margin-bottom:8px; ",
          "box-shadow:0 1px 2px rgba(0,0,0,0.05);"),
        div(style = "flex:0 0 140px; font-weight:600; font-size:14px;",
            pretty_names[[d]]),
        div(style = "flex:1; font-size:12px; color:#555;",
            if (is_configured) {
              tagList(
                tags$span(style = "color:#1f5025; font-weight:600;",
                          "\u2713 configured"),
                tags$span(style = "color:#888; margin-left:6px;",
                          sprintf("(%s)", kind))
              )
            } else {
              tags$span(style = "color:#aaa;", "\u2717 not configured")
            }
        ),
        div(class = "action-row", style = "flex:0 0 auto;",
            if (is_configured) {
              tagList(
                actionButton(paste0("demo_edit_", d), "Edit YAML",
                             class = "btn-sm btn-secondary"),
                actionButton(paste0("demo_change_", d), "Change template",
                             class = "btn-sm btn-secondary"),
                actionButton(paste0("demo_remove_", d), "Remove",
                             class = "btn-sm btn-outline-danger")
              )
            } else {
              actionButton(paste0("demo_setup_", d), "Set up...",
                           class = "btn-sm btn-primary")
            }
        )
    )
  })

  div(
    h2("Demographics setup", style = "margin-bottom:12px;"),
    div(style = "font-size:13px; color:#555; margin-bottom:18px; max-width:680px;",
        "All of your study's demographic variables are managed here. ",
        "Setting up a demographic creates a YAML transformation in your ",
        "config dir's ",
        tags$code("transformations/"),
        " folder. After setup, edit the YAML if your codebook uses ",
        "different column names than the template assumes."),
    do.call(tagList, rows)
  )
}

# Build a modal for selecting a template for a given demographic.
.demographic_setup_modal <- function(name) {
  templates <- brfss_list_demographic_templates(name = name)
  if (nrow(templates) == 0L) {
    return(modalDialog(
      title = sprintf("No templates for '%s'", name),
      "No shipped templates found. You'll need to write the transformation YAML by hand.",
      easyClose = TRUE
    ))
  }

  template_choices <- setNames(
    templates$template,
    sprintf("%s \u2014 %s", templates$template, templates$description)
  )

  modalDialog(
    title = sprintf("Set up '%s'", name),
    div(style = "font-size:13px; color:#555; margin-bottom:14px;",
        "Pick a template that matches how you want this demographic ",
        "defined. You can edit the YAML afterward to fine-tune."),
    selectInput(paste0("demo_template_", name),
                "Template:",
                choices = template_choices,
                width = "100%"),
    div(style = "font-size:11px; color:#888;",
        "After setup, the YAML opens for review. ",
        "Inputs may need editing to match your state codebook column names."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(paste0("demo_confirm_", name),
                   "Set up", class = "btn-primary")
    ),
    easyClose = TRUE
  )
}

# ============================================================================
# Build a "concept index" from a bundle for the heatmap.
# Uses the stored concept_id field as the grouping key (no union-find).
# Produces one row per concept_id with year cells and modal labels.
# ============================================================================

build_concept_index <- function(bundle) {
  cw <- bundle$crosswalk
  state_only <- bundle$state_only
  cdc_only <- bundle$cdc_only

  facts <- list()

  # Schema sanity: confirm columns exist before iterating
  required_cw_cols <- c("concept_id","year","state_var","cdc_var","is_primary")
  if (!is.null(cw) && nrow(cw) > 0) {
    missing <- setdiff(required_cw_cols, names(cw))
    if (length(missing)) {
      stop("crosswalk.csv missing columns: ",
           paste(missing, collapse = ", "),
           ". Try cw_load() again to auto-populate concept_id.",
           call. = FALSE)
    }
    for (i in seq_len(nrow(cw))) {
      # Determine kind:
      # - If state_var is NA / empty: this is a CDC-seeded row, not yet mapped.
      # - Else if is_primary == 1: standard paired.
      # - Else: secondary mapping.
      sv <- cw$state_var[i]
      is_seeded <- is.na(sv) || !nzchar(sv)
      row_kind <- if (is_seeded) {
        "seeded"
      } else if (isTRUE(cw$is_primary[i] == 1L)) {
        "paired"
      } else {
        "paired_alt"
      }
      # Row "unverified" flag (defaults to 0 if column absent).
      unv <- if ("unverified" %in% names(cw)) cw$unverified[i] else 0L
      facts[[length(facts) + 1L]] <- list(
        concept_id = cw$concept_id[i],
        year = cw$year[i],
        state_var = if (is_seeded) "" else sv,
        cdc_var = cw$cdc_var[i],
        kind = row_kind,
        unverified = as.integer(unv %||% 0L)
      )
    }
  }
  if (!is.null(state_only) && nrow(state_only) > 0) {
    if (!"state_var" %in% names(state_only)) {
      stop("state_only.csv missing 'state_var' column.", call. = FALSE)
    }
    if (!"concept_id" %in% names(state_only)) {
      state_only$concept_id <- state_only$state_var  # safety: should be filled by cw_load
    }
    for (i in seq_len(nrow(state_only))) {
      facts[[length(facts) + 1L]] <- list(
        concept_id = state_only$concept_id[i],
        year = state_only$year[i],
        state_var = state_only$state_var[i],
        cdc_var = "",
        kind = "state_only"
      )
    }
  }
  if (!is.null(cdc_only) && nrow(cdc_only) > 0) {
    if (!"cdc_var" %in% names(cdc_only)) {
      stop("cdc_only.csv missing 'cdc_var' column.", call. = FALSE)
    }
    if (!"concept_id" %in% names(cdc_only)) {
      cdc_only$concept_id <- cdc_only$cdc_var
    }
    for (i in seq_len(nrow(cdc_only))) {
      facts[[length(facts) + 1L]] <- list(
        concept_id = cdc_only$concept_id[i],
        year = cdc_only$year[i],
        state_var = "",
        cdc_var = cdc_only$cdc_var[i],
        kind = "cdc_only"
      )
    }
  }

  if (length(facts) == 0L) {
    return(list(
      concepts = data.frame(concept_id = character(),
                            state_modal = character(),
                            cdc_modal = character(),
                            n_years = integer(),
                            bucket = character(),
                            state_label = character(),
                            cdc_label = character(),
                            stringsAsFactors = FALSE),
      cells = data.frame(concept_id = character(),
                         year = integer(),
                         state_var = character(),
                         cdc_var = character(),
                         kind = character(),
                         stringsAsFactors = FALSE)
    ))
  }

  facts <- do.call(rbind, lapply(facts, as.data.frame, stringsAsFactors = FALSE))

  # Per-concept summary: modal var names + label lookups, grouped by concept_id
  state_cb <- bundle$state_codebook
  cdc_cb <- bundle$cdc_codebook

  concept_summary <- facts |>
    group_by(.data$concept_id) |>
    summarize(
      state_modal = {
        v <- .data$state_var[.data$state_var != ""]
        if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else ""
      },
      cdc_modal = {
        v <- .data$cdc_var[.data$cdc_var != ""]
        if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else ""
      },
      n_years = dplyr::n_distinct(.data$year),
      bucket = {
        kinds <- unique(.data$kind)
        if (any(kinds %in% c("paired","paired_alt"))) "paired"
        else if (all(kinds == "state_only")) "state_only"
        else if (all(kinds == "cdc_only")) "cdc_only"
        else "mixed"
      },
      .groups = "drop"
    )

  # Lookup labels (modal cdc_var -> most recent CDC label, etc.)
  concept_summary$state_label <- vapply(seq_len(nrow(concept_summary)), function(i) {
    sv <- concept_summary$state_modal[i]
    if (sv == "") return(NA_character_)
    hit <- state_cb[state_cb$raw_var_name == sv & !is.na(state_cb$label), ]
    if (nrow(hit) == 0) return(NA_character_)
    hit <- hit[order(-hit$year), ]
    hit$label[1]
  }, character(1))

  concept_summary$cdc_label <- vapply(seq_len(nrow(concept_summary)), function(i) {
    cv <- concept_summary$cdc_modal[i]
    if (cv == "") return(NA_character_)
    hit <- cdc_cb[cdc_cb$raw_var_name == cv & !is.na(cdc_cb$label), ]
    if (nrow(hit) == 0) return(NA_character_)
    hit <- hit[order(-hit$year), ]
    hit$label[1]
  }, character(1))

  list(concepts = concept_summary, cells = facts)
}


# ============================================================================
# Build per-concept (domain, subdomain) assignments using a CDC-anchored map.
# We read taxonomy from cdc_codebook$section_name and SECTION_TO_SUBDOMAIN +
# SUBDOMAIN_TO_DOMAIN mappings encoded once.
# ============================================================================

source_taxonomy_map <- function(cdc_codebook) {
  # Section normalization (CDC's section_name -> subdomain)
  norm <- list(
    "Healthy Days" = "Healthy Days",
    "Healthy Days \u2014 Health Related Quality of Life" = "Healthy Days",
    "Healthy Days - Health-Related Quality of Life" = "Healthy Days",
    "Healthy Days (Symptoms)" = "Healthy Days",
    "Adverse Childhood Experience" = "Adverse Childhood Experiences",
    "Adverse Childhood Experiences" = "Adverse Childhood Experiences",
    "Sexual Orientation and Gender Identity (SOGI)" = "Sexual Orientation and Gender Identity",
    "Sexual Orientation and Gender Identity" = "Sexual Orientation and Gender Identity",
    "Sexual Orientation" = "Sexual Orientation and Gender Identity",
    "Arthritis" = "Arthritis",
    "Arthritis Burden" = "Arthritis",
    "Arthritis Management" = "Arthritis",
    "Breast and Cervical Cancer Screening" = "Breast/Cervical Cancer Screening",
    "Breast/Cervical Cancer Screening" = "Breast/Cervical Cancer Screening",
    "Cancer Survivorship" = "Cancer Survivorship",
    "Cancer Survivorship: Course of Treatment" = "Cancer Survivorship",
    "Cancer Survivorship: Type of Cancer" = "Cancer Survivorship",
    "Cancer Survivorship: Pain Management" = "Cancer Survivorship",
    "Caregiver" = "Caregiving",
    "Caregiving" = "Caregiving",
    "Cell Phone Introduction" = "Survey Operations",
    "Cell Phone County" = "Survey Operations",
    "Land Line Introduction" = "Survey Operations",
    "Calculated Race Variables" = "Race and Ethnicity",
    "Reactions to Race" = "Race and Ethnicity",
    "Sugar Sweetened Beverages" = "Sugar-Sweetened Beverages",
    "Sugar-Sweetened Beverage" = "Sugar-Sweetened Beverages",
    "CDCCATI Hispanic Adult" = "Survey Operations",
    "CDCCATI Hispanic Child" = "Survey Operations",
    "Influenza" = "Immunization",
    "Place of Flu Vaccination" = "Immunization",
    "Tetanus Diphtheria (Tdap) (Adults)" = "Immunization",
    "Tetanus, Diphtheria, and Acellular Pertussis (Tdap) (Adults)" = "Immunization",
    "Tetanus Vaccination" = "Immunization",
    "Shingles" = "Immunization",
    "Shingles (ZOSTAVAX or ZOS)" = "Immunization",
    "Shingles Vaccination" = "Immunization",
    "HPV Vaccination" = "Immunization",
    "Adult Human Papilloma Virus (HPV)" = "Immunization",
    "Adult Human Papillomavirus (HPV) - Vaccination" = "Immunization",
    "HPV - Vaccination" = "Immunization",
    "COVID Vaccination" = "Immunization",
    "Long-term COVID Effects" = "Long COVID",
    "Tobacco Use" = "Tobacco Use",
    "Other Tobacco Use" = "Tobacco Use",
    "Tobacco Cessation" = "Tobacco Use",
    "E-Cigarettes" = "Tobacco Use",
    "Marijuana Use" = "Marijuana Use",
    "Marijuana" = "Marijuana Use",
    "Adult Asthma History" = "Asthma",
    "Childhood Asthma Prevalence" = "Asthma",
    "Asthma Follow-up Call Back" = "Asthma",
    "Respiratory Health" = "Respiratory Health",
    "Respiratory Health (COPD Symptoms)" = "Respiratory Health",
    "Sleep Disorder" = "Sleep",
    "Inadequate Sleep" = "Sleep",
    "Social Determinants" = "Social Determinants of Health",
    "Social Determinants and Health Equity" = "Social Determinants of Health",
    "Social Determinants of Health" = "Social Determinants of Health",
    "Seatbelt Use" = "Seatbelts and Drinking-Driving",
    "Seatbelt Use and Drinking and Driving" = "Seatbelts and Drinking-Driving",
    "Drinking and Driving" = "Seatbelts and Drinking-Driving",
    "Hypertension Awareness" = "Hypertension",
    "Actions to Control High Blood Pressure" = "Hypertension",
    "Home/ Self-measured Blood Pressure" = "Hypertension",
    "Cholesterol Awareness" = "Cholesterol",
    "Cardiovascular Health" = "Cardiovascular Disease",
    "Heart Attack and Stroke" = "Cardiovascular Disease",
    "Aspirin for CVD Prevention" = "Cardiovascular Disease",
    "Exercise (Physical Activity)" = "Physical Activity",
    "Exercise" = "Physical Activity",
    "Family Planning" = "Family Planning",
    "Preconception Health/Family Planning" = "Family Planning",
    "Diabetes" = "Diabetes",
    "Pre-Diabetes" = "Diabetes",
    "Hepatitis Treatment" = "Hepatitis",
    "Sex at Birth" = "Sex and Gender",
    "Respondent Sex" = "Sex and Gender",
    "Depression and Anxiety" = "Mental Health",
    "Emotional Support and Life Satisfaction" = "Mental Health",
    "Demographics" = "Demographics",
    "Child Demographic Variables" = "Child Demographics",
    "Random Child Selection" = "Survey Operations",
    "Calculated Variables" = "Calculated Variables",
    "Weighting Variables" = "Weighting Variables",
    "Child Weighting Variables" = "Weighting Variables",
    "Record Identification" = "Survey Operations",
    "Questionnaire Version" = "Survey Operations",
    "Questionnaire Language" = "Survey Operations",
    "End of Data Submission" = "Survey Operations",
    "StateQuestions" = "Survey Operations",
    "Urban Rural" = "Geography",
    "Health Status" = "Health Status",
    "Disability" = "Disability",
    "Cognitive Decline" = "Cognitive Decline",
    "ME/CFS" = "ME/CFS",
    "Falls" = "Falls",
    "Health Care Access" = "Health Care Access",
    "Chronic Health Conditions" = "Chronic Health Conditions",
    "Oral Health" = "Oral Health",
    "Indoor Tanning" = "Indoor Tanning",
    "Excess Sun Exposure" = "Sun Exposure",
    "Health Literacy" = "Health Literacy",
    "Firearm Safety" = "Firearm Safety",
    "HIV/AIDS" = "HIV/AIDS",
    "Fruits & Vegetables" = "Fruits and Vegetables",
    "Sodium or Salt-Related Behavior" = "Sodium",
    "Alcohol Consumption" = "Alcohol Use",
    "Alcohol Screening & Brief Intervention (ASBI)" = "Alcohol Use",
    "Lung Cancer Screening" = "Lung Cancer Screening",
    "Colorectal Cancer Screening" = "Colorectal Cancer Screening",
    "Prostate Cancer Screening" = "Prostate Cancer Screening",
    "Prostate Cancer Screening Decision Making" = "Prostate Cancer Screening",
    "Clinic Breast Exam" = "Breast/Cervical Cancer Screening",
    "Clinical Breast Exam for Women\u00b4s Health" = "Breast/Cervical Cancer Screening",
    "Industry and Occupation" = "Industry and Occupation",
    "Food Stamps" = "Food Insecurity",
    "Menu Labeling" = "Food Environment",
    "Immunization" = "Immunization"
  )

  domain_of <- list(
    "Demographics" = "Demographics",
    "Child Demographics" = "Demographics",
    "Sex and Gender" = "Demographics",
    "Sexual Orientation and Gender Identity" = "Demographics",
    "Race and Ethnicity" = "Demographics",
    "Geography" = "Demographics",
    "Industry and Occupation" = "Demographics",
    "Cardiovascular Disease" = "Chronic Conditions",
    "Hypertension" = "Chronic Conditions",
    "Cholesterol" = "Chronic Conditions",
    "Diabetes" = "Chronic Conditions",
    "Asthma" = "Chronic Conditions",
    "Respiratory Health" = "Chronic Conditions",
    "Arthritis" = "Chronic Conditions",
    "Chronic Health Conditions" = "Chronic Conditions",
    "Cognitive Decline" = "Chronic Conditions",
    "ME/CFS" = "Chronic Conditions",
    "Long COVID" = "Chronic Conditions",
    "Hepatitis" = "Chronic Conditions",
    "HIV/AIDS" = "Chronic Conditions",
    "Cancer Survivorship" = "Cancer",
    "Breast/Cervical Cancer Screening" = "Cancer",
    "Colorectal Cancer Screening" = "Cancer",
    "Prostate Cancer Screening" = "Cancer",
    "Lung Cancer Screening" = "Cancer",
    "Mental Health" = "Behavioral Health",
    "Healthy Days" = "Behavioral Health",
    "Adverse Childhood Experiences" = "Behavioral Health",
    "Alcohol Use" = "Substance Use",
    "Tobacco Use" = "Substance Use",
    "Marijuana Use" = "Substance Use",
    "Physical Activity" = "Health Behaviors",
    "Fruits and Vegetables" = "Health Behaviors",
    "Sodium" = "Health Behaviors",
    "Sugar-Sweetened Beverages" = "Health Behaviors",
    "Food Environment" = "Health Behaviors",
    "Sleep" = "Health Behaviors",
    "Sun Exposure" = "Health Behaviors",
    "Indoor Tanning" = "Health Behaviors",
    "Oral Health" = "Health Behaviors",
    "Seatbelts and Drinking-Driving" = "Health Behaviors",
    "Health Care Access" = "Health Care Access",
    "Health Literacy" = "Health Care Access",
    "Immunization" = "Health Care Access",
    "Social Determinants of Health" = "Social Determinants of Health",
    "Food Insecurity" = "Social Determinants of Health",
    "Caregiving" = "Social Determinants of Health",
    "Disability" = "Social Determinants of Health",
    "Family Planning" = "Reproductive Health",
    "Falls" = "Injury and Safety",
    "Firearm Safety" = "Injury and Safety",
    "Health Status" = "Health Status",
    "Survey Operations" = "Survey Operations",
    "Calculated Variables" = "Survey Operations",
    "Weighting Variables" = "Survey Operations"
  )

  # Build cdc_var -> subdomain lookup (most recent year wins). If the codebook
  # lacks section_name (older XPT-extracted codebook), return an empty lookup
  # so everything ends up in the "Unclassified" domain rather than crashing.
  if (!"section_name" %in% names(cdc_codebook)) {
    return(list(
      cdc_var_sd = data.frame(raw_var_name = character(),
                              subdomain = character(),
                              stringsAsFactors = FALSE),
      domain_of  = domain_of
    ))
  }

  cb_norm <- cdc_codebook |>
    dplyr::filter(!is.na(.data$section_name)) |>
    dplyr::mutate(subdomain = unlist(norm[.data$section_name]))

  cdc_var_sd <- cb_norm |>
    dplyr::group_by(.data$raw_var_name) |>
    dplyr::summarize(
      subdomain = {
        sd <- .data$subdomain[!is.na(.data$subdomain)]
        if (length(sd)) names(sort(table(sd), decreasing = TRUE))[1] else NA_character_
      },
      .groups = "drop"
    )

  list(cdc_var_sd = cdc_var_sd, domain_of = domain_of)
}

# ============================================================================
# Main server
# ============================================================================

server <- function(input, output, session) {

  ed_path <- Sys.getenv("BRFSSTOOLS_EDITOR_PATH",
                        unset = system.file("extdata", package = "brfssTools"))

  bundle <- reactiveVal(cw_load(ed_path))
  dirty <- reactiveVal(FALSE)
  selected_cid <- reactiveVal(NULL)
  selected_year <- reactiveVal(NULL)
  active_domain <- reactiveVal(NULL)
  demo_refresh <- reactiveVal(0L)
  bump_demo <- function() demo_refresh(demo_refresh() + 1L)

  taxonomy_map <- reactive({
    source_taxonomy_map(bundle()$cdc_codebook)
  })

  concept_index <- reactive({
    build_concept_index(bundle())
  })

  # Per-concept domain assignment via CDC modal var
  concept_with_domain <- reactive({
    ci <- concept_index()$concepts
    tm <- taxonomy_map()
    if (nrow(ci) == 0) return(ci)

    sd_lookup <- setNames(tm$cdc_var_sd$subdomain,
                          tm$cdc_var_sd$raw_var_name)
    ci$subdomain <- ifelse(ci$cdc_modal != "",
                            sd_lookup[ci$cdc_modal],
                            NA_character_)
    ci$domain <- vapply(ci$subdomain, function(sd) {
      if (is.na(sd)) return(NA_character_)
      tm$domain_of[[sd]] %||% NA_character_
    }, character(1))
    ci$domain[is.na(ci$domain)] <- "Unclassified"
    ci$subdomain[is.na(ci$subdomain)] <- "(none)"
    ci
  })

  # ------------------------------------------------------------------------
  # Header / status
  # ------------------------------------------------------------------------

  output$status_meta <- renderText({
    b <- bundle()
    n_pairs <- nrow(b$crosswalk)
    n_so <- if (!is.null(b$state_only)) nrow(b$state_only) else 0
    n_co <- if (!is.null(b$cdc_only)) nrow(b$cdc_only) else 0
    sprintf("%d pairs · %d state-only · %d cdc-only · path: %s",
            n_pairs, n_so, n_co, ed_path)
  })

  output$dirty_text <- renderText({
    if (dirty()) "unsaved changes" else ""
  })

  observe({
    session$sendCustomMessage("toggle_dirty", list(dirty = dirty()))
  })

  # ------------------------------------------------------------------------
  # Domain navigation (shared between CDC- and State-anchored views)
  # ------------------------------------------------------------------------

  view_mode <- reactive({ input$view_mode %||% "cdc" })

  # When mode changes, clear the selection so we don't end up showing edit
  # panel for a concept that's no longer visible.
  observeEvent(view_mode(), {
    selected_cid(NULL); selected_year(NULL)
  }, ignoreInit = TRUE)

  output$nav_pane_content <- renderUI({
    if (view_mode() == "demo") {
      return(.render_demographics_nav())
    }

    ci <- concept_with_domain()
    if (nrow(ci) == 0) return(div("No concepts."))

    # In State-anchored mode, hide cdc_only concepts since they're not
    # actionable from the state side.
    if (view_mode() == "state") {
      ci <- ci[ci$bucket != "cdc_only", , drop = FALSE]
    }

    counts <- ci |>
      dplyr::count(.data$domain, sort = FALSE) |>
      dplyr::arrange(.data$domain == "Unclassified", .data$domain)

    if (is.null(active_domain()) && nrow(counts) > 0) {
      active_domain(counts$domain[1])
    } else if (!is.null(active_domain()) &&
               !active_domain() %in% counts$domain &&
               nrow(counts) > 0) {
      # Active domain disappeared from this view (e.g., switched to State and
      # active was a CDC-only domain) - reset
      active_domain(counts$domain[1])
    }

    tagList(
      h2(if (view_mode() == "cdc") "Domains" else "Domains (state-anchored)"),
      lapply(seq_len(nrow(counts)), function(i) {
        d <- counts$domain[i]
        n <- counts$n[i]
        cls <- if (!is.null(active_domain()) && active_domain() == d)
          "nav-btn active" else "nav-btn"
        tags$button(class = cls, onclick = sprintf(
          "Shiny.setInputValue('select_domain', '%s', {priority:'event'})",
          gsub("'", "\\\\'", d)),
          span(d), span(class = "count", n))
      })
    )
  })

  observeEvent(input$select_domain, {
    active_domain(input$select_domain)
    selected_cid(NULL)
    selected_year(NULL)
  })

  # ------------------------------------------------------------------------
  # Heatmap render for active domain
  # ------------------------------------------------------------------------

  output$main_view <- renderUI({
    if (view_mode() == "demo") {
      # Depend on demo_refresh so the pane re-renders after setup/remove.
      demo_refresh()
      return(.render_demographics_pane(input, bundle))
    }

    dom <- active_domain()
    if (is.null(dom)) return(div(class = "placeholder", "Select a domain."))

    ci <- concept_with_domain()
    cells <- concept_index()$cells

    # Filter by mode
    if (view_mode() == "state") {
      ci <- ci[ci$bucket != "cdc_only", , drop = FALSE]
    }

    sub <- ci[ci$domain == dom, , drop = FALSE]
    if (nrow(sub) == 0) {
      return(div(class = "placeholder", "No concepts in this domain."))
    }
    years <- sort(unique(cells$year))

    # Sort: in CDC mode, sort by subdomain then cdc_modal then state_modal.
    # In State mode, sort by subdomain then state_modal then cdc_modal.
    if (view_mode() == "state") {
      sub <- sub[order(sub$subdomain, sub$state_modal, sub$cdc_modal), ]
    } else {
      sub <- sub[order(sub$subdomain, sub$cdc_modal, sub$state_modal), ]
    }

    blocks <- list()
    for (sd in unique(sub$subdomain)) {
      sub_sd <- sub[sub$subdomain == sd, ]
      rows <- list()
      for (i in seq_len(nrow(sub_sd))) {
        c_row <- sub_sd[i, ]
        cid <- c_row$concept_id
        sv <- c_row$state_modal
        cv <- c_row$cdc_modal
        # Concept name format depends on view mode
        nm <- if (view_mode() == "state") {
          # Lead with state_var
          if (sv != "" && cv != "") {
            if (sv == cv) sv else paste0(sv, " \u2194 ", cv)
          } else if (sv != "") {
            paste0(sv, " (state-only)")
          } else {
            paste0(cv, " (CDC)")  # rare in state mode (we filter out cdc_only)
          }
        } else {
          # CDC-anchored: lead with whichever is present, prefer pair display
          if (sv != "" && cv != "") {
            if (sv == cv) sv else paste0(sv, " \u2194 ", cv)
          } else if (sv != "") {
            paste0(sv, " (state)")
          } else {
            paste0(cv, " (CDC)")
          }
        }
        # Always show the concept_id alongside if it differs from both
        if (!is.null(cid) && nzchar(cid) && cid != sv && cid != cv) {
          nm <- paste0(nm, " [", cid, "]")
        }
        lbl <- if (!is.na(c_row$cdc_label)) c_row$cdc_label
               else c_row$state_label %||% ""
        if (!is.na(lbl) && nchar(lbl) > 70) lbl <- paste0(substr(lbl,1,70),"\u2026")

        # Year cells
        cell_tags <- lapply(years, function(yr) {
          ck <- cells[cells$concept_id == cid & cells$year == yr, , drop = FALSE]
          if (nrow(ck) == 0) {
            tags$td(class = "year-cell k-M", "")
          } else {
            kind <- ck$kind[1]
            klass <- switch(kind,
                            "paired" = "k-P", "paired_alt" = "k-A",
                            "state_only" = "k-O", "cdc_only" = "k-C",
                            "k-M")
            letter <- switch(kind,
                             "paired" = "\u25CF", "paired_alt" = "\u25CF",
                             "state_only" = "S", "cdc_only" = "C",
                             "")
            sv_y <- ck$state_var[1]
            cv_y <- ck$cdc_var[1]
            tip <- sprintf("%d: %s", yr,
                           if (sv_y != "" && cv_y != "")
                             paste0(sv_y, " \u2194 ", cv_y)
                           else if (sv_y != "") paste0(sv_y, " (state-only)")
                           else paste0(cv_y, " (CDC-only)"))
            tags$td(class = paste("year-cell", klass),
                    title = tip, letter)
          }
        })

        rows[[length(rows) + 1L]] <- tags$tr(
          class = "concept-row",
          `data-cid` = cid,
          onclick = sprintf("Shiny.setInputValue('select_cid', '%s', {priority:'event'})",
                           gsub("'", "\\\\'", cid)),
          tags$td(class = "label-cell",
                  tags$span(style = "font-weight: 600;", nm),
                  if (!is.na(lbl) && nzchar(lbl))
                    tags$span(class = "meta", lbl)),
          do.call(tagList, cell_tags)
        )
      }

      blocks[[length(blocks)+1L]] <- div(class = "subdomain-block",
        div(class = "subdomain-header",
            h3(sd),
            span(class = "badge", sprintf("%d concepts", nrow(sub_sd)))),
        tags$table(class = "heatmap",
          tags$thead(tags$tr(
            tags$th(class = "label-col", "Concept"),
            do.call(tagList, lapply(years, function(y) tags$th(y))))),
          tags$tbody(do.call(tagList, rows))),
        div(class = "legend",
            span(span(class = "swatch k-P"), " Paired"),
            span(span(class = "swatch k-A"), " Secondary"),
            span(span(class = "swatch k-O"), " State only"),
            span(span(class = "swatch k-C"), " CDC only"),
            span(span(class = "swatch", style = "background:white;"), " Not fielded"))
      )
    }

    tagList(
      h2(dom),
      div(style = "color:#666;font-size:13px;margin-bottom:10px;",
          sprintf("%d concepts in %d subdomain(s) \u00b7 view: %s",
                  nrow(sub), length(unique(sub$subdomain)),
                  if (view_mode() == "state") "state-anchored" else "CDC-anchored")),
      do.call(tagList, blocks)
    )
  })

  # ------------------------------------------------------------------------
  # Edit panel
  # ------------------------------------------------------------------------

  observeEvent(input$select_cid, {
    selected_cid(input$select_cid)
    # Default the year to the first paired year of this concept
    cells <- concept_index()$cells
    sub <- cells[cells$concept_id == input$select_cid, , drop = FALSE]
    if (nrow(sub) > 0) {
      paired <- sub[sub$kind %in% c("paired","paired_alt"), ]
      pick <- if (nrow(paired) > 0) paired[1, ] else sub[1, ]
      selected_year(pick$year)
    }
    session$sendCustomMessage("select_concept", list(cid = input$select_cid))
  })

  output$edit_panel <- renderUI({
    cid <- selected_cid()
    if (is.null(cid)) {
      return(div(class = "placeholder",
                 "Click a concept row in the heatmap to edit.",
                 tags$br(), tags$br(),
                 "Tips:", tags$br(),
                 "\u2022 Each row is one concept across years.",
                 tags$br(),
                 "\u2022 Click a year cell to focus on that year's pair.",
                 tags$br(),
                 "\u2022 Use 'Add new pair...' for a brand-new concept."))
    }

    ci <- concept_index()
    concepts <- concept_with_domain()
    c_row <- concepts[concepts$concept_id == cid, , drop = FALSE]
    if (nrow(c_row) == 0) return(div("Concept not found."))

    facts <- ci$cells[ci$cells$concept_id == cid, , drop = FALSE]
    facts <- facts[order(facts$year), ]

    # Year selector
    year_opts <- sort(unique(facts$year))
    sel_year <- selected_year() %||% year_opts[1]

    # Get the row(s) for the selected year
    yr_facts <- facts[facts$year == sel_year, , drop = FALSE]

    # Build year selector + per-year detail
    tagList(
      div(class = "field-group",
          div(style = "font-size:13px;color:#666;",
              "Concept (across years)"),
          div(class = "var-name",
              if (c_row$state_modal != "" && c_row$cdc_modal != "")
                paste0(c_row$state_modal, " \u2194 ", c_row$cdc_modal)
              else if (c_row$state_modal != "")
                paste0(c_row$state_modal, " (state-only)")
              else paste0(c_row$cdc_modal, " (CDC-only)")),
          div(style = "font-size:11px;color:#888;margin-top:4px;",
              sprintf("Domain: %s \u2192 %s",
                      c_row$domain, c_row$subdomain)),
          # concept_id displayed and editable
          div(style = "margin-top:10px;",
              tags$label("concept_id (used by brfss_pull as column name)"),
              textInput("edit_concept_id", NULL,
                        value = cid,
                        width = "100%"),
              div(class = "action-row",
                  actionButton("rename_concept_btn",
                               "Rename concept everywhere",
                               class = "btn-warning btn-sm"))),
          if (!is.na(c_row$cdc_label) && nzchar(c_row$cdc_label %||% ""))
            div(class = "question-text",
                tags$strong("CDC: "), c_row$cdc_label),
          if (!is.na(c_row$state_label) && nzchar(c_row$state_label %||% ""))
            div(class = "question-text",
                tags$strong("State: "), c_row$state_label)
      ),

      div(class = "field-group",
          tags$label("Year"),
          selectInput("year_select", NULL,
                      choices = year_opts,
                      selected = sel_year,
                      width = "100%"),
          # Show all rows for this concept-year (could be 1 or more)
          uiOutput("year_rows_panel")
      )
    )
  })

  observeEvent(input$year_select, {
    selected_year(as.integer(input$year_select))
  })

  output$year_rows_panel <- renderUI({
    cid <- selected_cid()
    yr <- selected_year()
    if (is.null(cid) || is.null(yr)) return(NULL)

    facts <- concept_index()$cells
    yr_facts <- facts[facts$concept_id == cid & facts$year == yr, , drop = FALSE]

    if (nrow(yr_facts) == 0) {
      return(div(class = "placeholder",
                 "No pair recorded for this year.",
                 tags$br(),
                 actionButton("add_for_year", "Add a pair for this year",
                              class = "btn-primary btn-sm",
                              style = "margin-top:8px;")))
    }

    tagList(lapply(seq_len(nrow(yr_facts)), function(i) {
      r <- yr_facts[i, ]
      kind <- r$kind
      sv <- r$state_var; cv <- r$cdc_var
      unv <- if ("unverified" %in% names(r)) r$unverified else 0L

      # Lookup current notes/source/score from crosswalk if paired or seeded
      # (both live in the crosswalk; seeded rows just have NA state_var).
      meta <- if (kind %in% c("paired","paired_alt","seeded")) {
        cw <- bundle()$crosswalk
        hit <- if (kind == "seeded") {
          cw[cw$year == yr & is.na(cw$state_var) & cw$cdc_var == cv, ]
        } else {
          cw[cw$year == yr & cw$state_var == sv & cw$cdc_var == cv, ]
        }
        if (nrow(hit) >= 1) hit[1, ] else NULL
      } else NULL

      div(style = "border-top:1px solid #ddd;padding-top:8px;margin-top:8px;",
          tags$strong(switch(kind,
            "paired" = "Primary pair",
            "paired_alt" = "Secondary pair",
            "state_only" = "State-only (no CDC partner)",
            "cdc_only" = "CDC-only (state didn't field)",
            "seeded" = if (unv == 1L) "CDC seed (unverified)" else "CDC seed (verified, not mapped)",
            "Pair")),
          div(class = "var-name", style = "margin-top:4px;",
              if (sv != "" && cv != "")
                sprintf("%s \u2194 %s", sv, cv)
              else if (sv != "") sv
              else cv),

          if (!is.null(meta)) {
            tagList(
              div(style = "margin-top:8px;",
                  tags$label("Notes"),
                  textAreaInput("edit_notes", NULL,
                                value = meta$notes %||% "",
                                width = "100%", rows = 2)),
              div(style = "margin-top:6px;",
                  tags$label("Source"),
                  textInput("edit_source", NULL,
                            value = meta$source %||% "",
                            width = "100%")),
              div(style = "margin-top:6px;",
                  tags$label("Is primary"),
                  checkboxInput("edit_primary", NULL,
                                value = meta$is_primary == 1L)),
              div(class = "action-row",
                  actionButton("save_metadata_btn", "Save metadata",
                               class = "btn-primary btn-sm")))
          },

          # Action buttons
          div(class = "action-row", style = "margin-top:12px;",
              if (kind %in% c("paired","paired_alt")) tagList(
                actionButton("change_cdc_btn", "Change CDC partner...",
                             class = "btn-outline-primary btn-sm"),
                actionButton("change_state_btn", "Change state partner...",
                             class = "btn-outline-primary btn-sm"),
                actionButton("remove_pair_btn", "Remove pair",
                             class = "btn-outline-danger btn-sm"),
                actionButton("mark_state_only_btn", "Mark state-only",
                             class = "btn-outline-warning btn-sm"),
                actionButton("mark_cdc_only_btn", "Mark CDC-only",
                             class = "btn-outline-warning btn-sm"))
              else if (kind == "state_only") tagList(
                actionButton("pair_with_cdc_btn", "Pair with CDC variable...",
                             class = "btn-primary btn-sm"))
              else if (kind == "cdc_only") tagList(
                actionButton("pair_with_state_btn", "Pair with state variable...",
                             class = "btn-primary btn-sm"))
              )
          )
    }))
  })

  # ------------------------------------------------------------------------
  # Edit actions
  # ------------------------------------------------------------------------

  current_pair <- reactive({
    cid <- selected_cid(); yr <- selected_year()
    req(cid, yr)
    facts <- concept_index()$cells
    facts[facts$concept_id == cid & facts$year == yr, , drop = FALSE]
  })

  observeEvent(input$save_metadata_btn, {
    pr <- current_pair()
    if (nrow(pr) == 0) return()
    r <- pr[1, ]
    if (!(r$kind %in% c("paired","paired_alt"))) return()
    b <- tryCatch(
      cw_update_pair(bundle(), r$year, r$state_var, r$cdc_var,
                     is_primary = if (input$edit_primary) 1L else 0L,
                     source = input$edit_source,
                     notes = input$edit_notes),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification("Metadata updated.", type = "message")
    }
  })

  # Rename the entire concept across the bundle
  observeEvent(input$rename_concept_btn, {
    cid <- selected_cid()
    new_cid <- input$edit_concept_id
    if (is.null(cid) || is.null(new_cid) || new_cid == cid) {
      showNotification("Edit the concept_id field first, then click Rename.",
                       type = "warning")
      return()
    }
    # Count affected rows for the confirmation message
    b <- bundle()
    n_cw <- if (!is.null(b$crosswalk))  sum(b$crosswalk$concept_id == cid, na.rm = TRUE)  else 0
    n_so <- if (!is.null(b$state_only)) sum(b$state_only$concept_id == cid, na.rm = TRUE) else 0
    n_co <- if (!is.null(b$cdc_only))   sum(b$cdc_only$concept_id == cid, na.rm = TRUE)   else 0
    showModal(modalDialog(
      title = "Rename concept everywhere?",
      sprintf("Rename '%s' \u2192 '%s' across %d crosswalk row(s), %d state-only row(s), %d cdc-only row(s)?",
              cid, new_cid, n_cw, n_so, n_co),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_rename_concept", "Rename",
                                    class = "btn-warning"))
    ))
  })
  observeEvent(input$confirm_rename_concept, {
    removeModal()
    cid <- selected_cid()
    new_cid <- input$edit_concept_id
    if (is.null(cid) || is.null(new_cid) || new_cid == cid) return()
    b <- tryCatch(
      cw_rename_concept(bundle(), from = cid, to = new_cid),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      selected_cid(new_cid)  # follow the rename so the UI keeps the same row selected
      showNotification(sprintf("Renamed '%s' \u2192 '%s'", cid, new_cid),
                       type = "message")
    }
  })

  observeEvent(input$remove_pair_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    showModal(modalDialog(
      title = "Remove pair?",
      sprintf("Remove %d  %s \u2194 %s ?", r$year, r$state_var, r$cdc_var),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_remove", "Remove",
                                    class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_remove, {
    removeModal()
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    b <- tryCatch(
      cw_remove_pair(bundle(), r$year, r$state_var, r$cdc_var),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Removed %d %s \u2194 %s", r$year, r$state_var, r$cdc_var),
                       type = "message")
    }
  })

  observeEvent(input$change_cdc_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    cb <- bundle()$cdc_codebook
    cdc_choices <- sort(unique(cb$raw_var_name[cb$year == r$year]))
    showModal(modalDialog(
      title = "Change CDC partner",
      sprintf("Pair: %d  %s \u2194 %s", r$year, r$state_var, r$cdc_var),
      selectInput("new_cdc_var_pick", "New CDC variable",
                  choices = cdc_choices, selected = r$cdc_var),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_change_cdc", "Change",
                                    class = "btn-primary"))
    ))
  })
  observeEvent(input$confirm_change_cdc, {
    removeModal()
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    new_cdc <- input$new_cdc_var_pick
    if (is.null(new_cdc) || new_cdc == r$cdc_var) return()
    b <- tryCatch(
      cw_replace_cdc_partner(bundle(), r$year, r$state_var, r$cdc_var, new_cdc),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("CDC partner changed: %s \u2192 %s", r$cdc_var, new_cdc),
                       type = "message")
    }
  })

  observeEvent(input$change_state_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    cb <- bundle()$state_codebook
    state_choices <- sort(unique(cb$raw_var_name[cb$year == r$year]))
    showModal(modalDialog(
      title = "Change state partner",
      sprintf("Pair: %d  %s \u2194 %s", r$year, r$state_var, r$cdc_var),
      selectInput("new_state_var_pick", "New state variable",
                  choices = state_choices, selected = r$state_var),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_change_state", "Change",
                                    class = "btn-primary"))
    ))
  })
  observeEvent(input$confirm_change_state, {
    removeModal()
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    new_sv <- input$new_state_var_pick
    if (is.null(new_sv) || new_sv == r$state_var) return()
    b <- tryCatch(
      cw_replace_state_partner(bundle(), r$year, r$state_var, r$cdc_var, new_sv),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("State partner changed: %s \u2192 %s", r$state_var, new_sv),
                       type = "message")
    }
  })

  observeEvent(input$mark_state_only_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    b <- tryCatch(
      cw_mark_state_only(bundle(), r$year, r$state_var,
                         notes = "via editor"),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Marked %s as state-only for %d", r$state_var, r$year),
                       type = "message")
    }
  })

  observeEvent(input$mark_cdc_only_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    b <- tryCatch(
      cw_mark_cdc_only(bundle(), r$year, r$cdc_var,
                       notes = "via editor"),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Marked %s as CDC-only for %d", r$cdc_var, r$year),
                       type = "message")
    }
  })

  observeEvent(input$pair_with_cdc_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    cb <- bundle()$cdc_codebook
    cdc_choices <- sort(unique(cb$raw_var_name[cb$year == r$year]))
    showModal(modalDialog(
      title = sprintf("Pair %s with a CDC variable (%d)", r$state_var, r$year),
      selectInput("pair_cdc_pick", "CDC variable", choices = cdc_choices),
      textInput("pair_cdc_notes", "Notes (optional)", ""),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_pair_cdc", "Add pair",
                                    class = "btn-primary"))
    ))
  })
  observeEvent(input$confirm_pair_cdc, {
    removeModal()
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    cv <- input$pair_cdc_pick
    if (is.null(cv) || cv == "") return()
    b <- tryCatch(
      cw_add_pair(bundle(), r$year, r$state_var, cv,
                  notes = input$pair_cdc_notes %||% ""),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Paired %s \u2194 %s for %d",
                               r$state_var, cv, r$year), type = "message")
    }
  })

  observeEvent(input$pair_with_state_btn, {
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    cb <- bundle()$state_codebook
    sv_choices <- sort(unique(cb$raw_var_name[cb$year == r$year]))
    showModal(modalDialog(
      title = sprintf("Pair %s with a state variable (%d)", r$cdc_var, r$year),
      selectInput("pair_sv_pick", "State variable", choices = sv_choices),
      textInput("pair_sv_notes", "Notes (optional)", ""),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_pair_sv", "Add pair",
                                    class = "btn-primary"))
    ))
  })
  observeEvent(input$confirm_pair_sv, {
    removeModal()
    pr <- current_pair(); if (nrow(pr) == 0) return()
    r <- pr[1, ]
    sv <- input$pair_sv_pick
    if (is.null(sv) || sv == "") return()
    b <- tryCatch(
      cw_add_pair(bundle(), r$year, sv, r$cdc_var,
                  notes = input$pair_sv_notes %||% ""),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Paired %s \u2194 %s for %d",
                               sv, r$cdc_var, r$year), type = "message")
    }
  })

  # ------------------------------------------------------------------------
  # Add brand-new pair (not from any existing concept)
  # ------------------------------------------------------------------------

  observeEvent(input$add_pair_btn, {
    b <- bundle()
    years <- sort(unique(b$cdc_codebook$year))
    showModal(modalDialog(
      title = "Add new pair",
      selectInput("new_year", "Year", choices = years),
      uiOutput("new_state_var_ui"),
      uiOutput("new_cdc_var_ui"),
      textInput("new_notes", "Notes (optional)", ""),
      checkboxInput("new_allow_unknown", "Allow unknown variables (skip codebook validation)", FALSE),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_new_pair", "Add",
                                    class = "btn-primary"))
    ))
  })

  output$new_state_var_ui <- renderUI({
    yr <- as.integer(input$new_year %||% NA)
    if (is.na(yr)) return(NULL)
    cb <- bundle()$state_codebook
    selectInput("new_state_var", "State variable",
                choices = c("", sort(unique(cb$raw_var_name[cb$year == yr]))))
  })

  output$new_cdc_var_ui <- renderUI({
    yr <- as.integer(input$new_year %||% NA)
    if (is.na(yr)) return(NULL)
    cb <- bundle()$cdc_codebook
    selectInput("new_cdc_var", "CDC variable",
                choices = c("", sort(unique(cb$raw_var_name[cb$year == yr]))))
  })

  observeEvent(input$confirm_new_pair, {
    removeModal()
    yr <- as.integer(input$new_year)
    sv <- input$new_state_var
    cv <- input$new_cdc_var
    if (is.null(sv) || is.null(cv) || sv == "" || cv == "") {
      showNotification("Pick both a state and CDC variable.", type = "error")
      return()
    }
    b <- tryCatch(
      cw_add_pair(bundle(), yr, sv, cv,
                  notes = input$new_notes %||% "",
                  allow_unknown = isTRUE(input$new_allow_unknown)),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(b)) {
      bundle(b); dirty(TRUE)
      showNotification(sprintf("Added %s \u2194 %s for %d", sv, cv, yr),
                       type = "message")
    }
  })

  # ------------------------------------------------------------------------
  # Save / reload
  # ------------------------------------------------------------------------

  observeEvent(input$save_btn, {
    if (!dirty()) {
      showNotification("Nothing to save.", type = "message")
      return()
    }
    res <- tryCatch({ cw_save(bundle(), ed_path); TRUE },
                    error = function(e) {
                      showNotification(conditionMessage(e), type = "error")
                      FALSE
                    })
    if (isTRUE(res)) {
      dirty(FALSE)
      showNotification("Saved (with .bak backup).", type = "message")
    }
  })

  observeEvent(input$reload_btn, {
    showModal(modalDialog(
      title = "Reload from disk?",
      if (dirty()) "You have unsaved changes. Reloading will discard them. Continue?"
      else "Reload the bundle from disk?",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_reload", "Reload",
                                    class = "btn-warning"))
    ))
  })
  observeEvent(input$confirm_reload, {
    removeModal()
    bundle(cw_load(ed_path))
    dirty(FALSE)
    selected_cid(NULL); selected_year(NULL)
    showNotification("Reloaded.", type = "message")
  })

  # ------------------------------------------------------------------------
  # Demographics tab observers
  # ------------------------------------------------------------------------
  # Buttons are dynamically named (demo_setup_age, demo_setup_sex, ...).
  # We register one observeEvent per (action, demographic).

  big_six <- c("age", "sex", "race", "ethnicity", "education", "income")

  for (d in big_six) {
    local({
      dem <- d  # capture in local scope per iteration

      # Set up: open template-picker modal
      observeEvent(input[[paste0("demo_setup_", dem)]], {
        showModal(.demographic_setup_modal(dem))
      })

      # Confirm: read the selected template, run setup
      observeEvent(input[[paste0("demo_confirm_", dem)]], {
        tmpl <- input[[paste0("demo_template_", dem)]]
        if (is.null(tmpl) || !nzchar(tmpl)) {
          showNotification("No template selected.", type = "warning")
          return()
        }
        result <- tryCatch(
          brfss_setup_demographic(name = dem, template = tmpl,
                                   path = ed_path, edit = FALSE,
                                   overwrite = TRUE),
          error = function(e) e
        )
        if (inherits(result, "error")) {
          showNotification(sprintf("Failed: %s", conditionMessage(result)),
                            type = "error", duration = 8)
        } else {
          showNotification(sprintf("Set up '%s' from template '%s'.",
                                    dem, tmpl),
                            type = "message")
          bump_demo()
        }
        removeModal()
      })

      # Edit: open the YAML in the user's editor
      observeEvent(input[[paste0("demo_edit_", dem)]], {
        fp <- file.path(ed_path, "transformations", paste0(dem, ".yaml"))
        if (!file.exists(fp)) {
          showNotification("Transformation file not found.", type = "warning")
          return()
        }
        tryCatch(utils::file.edit(fp),
                 error = function(e) {
                   showNotification(
                     sprintf("Couldn't open editor: %s. File at %s",
                             conditionMessage(e), fp),
                     type = "warning", duration = 8)
                 })
      })

      # Change template: open the same setup modal (overwrite=TRUE handles it)
      observeEvent(input[[paste0("demo_change_", dem)]], {
        showModal(.demographic_setup_modal(dem))
      })

      # Remove: delete the YAML (and .R companion)
      observeEvent(input[[paste0("demo_remove_", dem)]], {
        showModal(modalDialog(
          title = sprintf("Remove '%s' transformation?", dem),
          div("This deletes the YAML and its companion .R file from your ",
              "config dir's transformations/ folder. Cannot be undone."),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(paste0("demo_confirm_remove_", dem),
                          "Delete", class = "btn-danger")
          )
        ))
      })

      observeEvent(input[[paste0("demo_confirm_remove_", dem)]], {
        trans_dir <- file.path(ed_path, "transformations")
        files_to_remove <- c(
          file.path(trans_dir, paste0(dem, ".yaml")),
          file.path(trans_dir, paste0(dem, ".yml")),
          file.path(trans_dir, paste0(dem, ".R"))
        )
        for (fp in files_to_remove) {
          if (file.exists(fp)) {
            # .bak rotation
            file.copy(fp, paste0(fp, ".bak"), overwrite = TRUE)
            file.remove(fp)
          }
        }
        showNotification(sprintf("Removed '%s'. Backups in transformations/.",
                                  dem), type = "message")
        bump_demo()
        removeModal()
      })
    })
  }
}
