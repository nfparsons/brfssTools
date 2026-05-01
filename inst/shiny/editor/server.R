# inst/shiny/editor/server.R
#
# brfssTools editor v0.2.0 server.
#
# State:
#   bundle()         - reactive holder for the loaded crosswalk bundle
#   active_domain()  - which domain section is shown in the heatmap
#   selected_cell()  - list(concept_id=, year=) or NULL
#   dirty()          - TRUE if there are unsaved changes
#
# Per-cell editing is the core operation:
#   - cw_set_var()   sets a column lookup
#   - cw_set_calc()  sets an inline YAML calculation
#   - cw_verify()    clears the unverified flag

# ---------------------------------------------------------------------------
# Resolve path & namespace
# ---------------------------------------------------------------------------

ed_dataset <- Sys.getenv("BRFSSTOOLS_EDITOR_DATASET", unset = "")
if (!nzchar(ed_dataset)) {
  stop("BRFSSTOOLS_EDITOR_DATASET environment variable not set. ",
        "The editor should be launched via brfss_crosswalk_editor(dataset = ...).",
        call. = FALSE)
}

.ns <- asNamespace("brfssTools")

# Functions we use in the editor (all v0.2.0)
cw_load            <- .ns$cw_load
cw_save            <- .ns$cw_save
cw_set_var         <- .ns$cw_set_var
cw_set_calc        <- .ns$cw_set_calc
cw_assign_domain   <- .ns$cw_assign_domain
cw_verify          <- .ns$cw_verify
cw_rename_concept  <- .ns$cw_rename_concept
cw_add_concept     <- .ns$cw_add_concept
cw_remove_concept  <- .ns$cw_remove_concept
cw_merge_concepts  <- .ns$cw_merge_concepts
brfss_load_codebook <- .ns$brfss_load_codebook

`%||%` <- function(a, b) if (is.null(a)) b else a

# Load shinyAce if available; otherwise we use plain textareaInput
.has_shinyAce <- requireNamespace("shinyAce", quietly = TRUE)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Cell state classification:
#   "mapped"     - state_var present, unverified == 0
#   "unverified" - state_var present, unverified == 1
#   "unmapped"   - state_var NA, is_calculated == 0
#   "calc"       - is_calculated == 1
#   "unassigned" - domain == "Unassigned" (not editable)
.classify_cell <- function(row) {
  if (isTRUE(row$domain == "Unassigned")) return("unassigned")
  if (isTRUE(row$is_calculated == 1L)) return("calc")
  if (is.na(row$state_var) || !nzchar(row$state_var)) return("unmapped")
  if (isTRUE(row$unverified == 1L)) return("unverified")
  "mapped"
}

# Cell display text (short)
.cell_label <- function(row, kind) {
  switch(kind,
         "mapped"     = "\u2713",                     # check
         "unverified" = "?",
         "unmapped"   = "\u2014",                     # em dash
         "calc"       = "\u0192",                     # f hook (calc)
         "unassigned" = "\u00B7",                     # middle dot
         "")
}

# Variable list for a given (dataset, year), pulled from the state_codebook
# bundled into the loaded crosswalk bundle.
.year_var_list <- function(bundle, year) {
  cb <- bundle$state_codebook
  if (is.null(cb) || !"year" %in% names(cb) || !"raw_var_name" %in% names(cb)) {
    return(character(0))
  }
  vars <- cb$raw_var_name[cb$year == year]
  vars <- vars[!is.na(vars) & nzchar(vars)]
  sort(unique(vars))
}

# Variables already used by OTHER concepts in the same year
.vars_used_elsewhere <- function(bundle, year, exclude_concept = NULL) {
  cw <- bundle$crosswalk
  used <- cw$state_var[cw$year == year &
                       !is.na(cw$state_var) &
                       nzchar(cw$state_var) &
                       (is.null(exclude_concept) | cw$concept_id != exclude_concept)]
  unique(used)
}

# Available vars for a cell: full year list minus those used elsewhere
# (but always include the current cell's own state_var if present, so the
# user can see and re-confirm).
.available_vars_for_cell <- function(bundle, concept_id, year) {
  all_vars  <- .year_var_list(bundle, year)
  used      <- .vars_used_elsewhere(bundle, year, exclude_concept = concept_id)
  available <- setdiff(all_vars, used)

  # Always include the cell's current value (user might be re-confirming)
  cw  <- bundle$crosswalk
  idx <- which(cw$concept_id == concept_id & cw$year == year)
  if (length(idx) == 1L) {
    cur <- cw$state_var[idx]
    if (!is.na(cur) && nzchar(cur)) {
      available <- unique(c(cur, available))
    }
  }
  sort(available)
}

# Example calculation YAML to seed the editor
.example_calc_yaml <- function() {
  paste(c(
    "# Inline calculation for this (concept, year) cell.",
    "# Format follows brfssTools categorical_map / passthrough.",
    "",
    "type: categorical_map",
    "",
    "inputs:",
    "  raw1: SOME_RAW_VAR",
    "  raw2: ANOTHER_RAW_VAR",
    "",
    "levels:",
    "  - value: 1",
    "    label: \"First category\"",
    "    when: raw1 == 1 & raw2 == 2",
    "  - value: 2",
    "    label: \"Second category\"",
    "    when: raw1 == 2",
    "  - value: 9",
    "    label: \"Refused / Don't know\"",
    "    when: TRUE"),
    collapse = "\n")
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Reactive state -----------------------------------------------------

  bundle         <- reactiveVal(cw_load(dataset = ed_dataset))
  active_domain  <- reactiveVal(NULL)
  selected_cell  <- reactiveVal(NULL)   # list(concept_id, year)
  dirty          <- reactiveVal(FALSE)

  # Codebook cache: list keyed by year string. Lazily populated as
  # cells are clicked. NULL value means "tried, no codebook available";
  # a tibble means "loaded".
  codebook_cache <- reactiveVal(list())

  # Fetch (or pull from cache) the codebook for a given year.
  # Returns a tibble or NULL.
  .get_codebook_for_year <- function(yr) {
    cache <- codebook_cache()
    key <- as.character(yr)
    if (key %in% names(cache)) return(cache[[key]])

    # Try to load. Use strict = FALSE so absence -> NULL, not an error.
    cb <- tryCatch(
      brfss_load_codebook(dataset = ed_dataset, year = yr, strict = FALSE),
      error = function(e) NULL
    )
    cache[[key]] <- cb
    codebook_cache(cache)
    cb
  }

  # ---- Header text --------------------------------------------------------

  output$status_meta <- renderText({
    b   <- bundle()
    cw  <- b$crosswalk
    n   <- nrow(cw)
    n_un <- sum(cw$domain == "Unassigned")
    sprintf("%d concept-years \u00b7 %d unassigned \u00b7 %s",
            n, n_un, ed_dataset)
  })

  # ---- Domain summary (for nav and main view) -----------------------------

  domain_summary <- reactive({
    cw <- bundle()$crosswalk
    if (nrow(cw) == 0L) {
      return(data.frame(domain = character(0),
                        n_concepts = integer(0),
                        n_unverified = integer(0)))
    }
    # Unique concepts per domain
    by_dom <- split(cw, cw$domain)
    rows <- lapply(names(by_dom), function(d) {
      sub <- by_dom[[d]]
      data.frame(
        domain = d,
        n_concepts = length(unique(sub$concept_id)),
        n_unverified = sum(sub$unverified == 1L, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    # Sort: Unassigned last, others alphabetical
    out <- out[order(out$domain == "Unassigned", out$domain), ]
    out
  })

  # ---- Left nav: domain list ----------------------------------------------

  output$domain_nav <- renderUI({
    ds <- domain_summary()
    if (nrow(ds) == 0L) {
      return(div(class = "placeholder",
                 "Empty crosswalk.\n\nRun brfss_draft_crosswalk() to start."))
    }

    real_domains <- ds[ds$domain != "Unassigned", ]
    unassigned   <- ds[ds$domain == "Unassigned", ]

    nav_items <- lapply(seq_len(nrow(real_domains)), function(i) {
      r <- real_domains[i, ]
      cls <- "nav-link"
      if (!is.null(active_domain()) && active_domain() == r$domain) {
        cls <- paste(cls, "active")
      }
      tags$div(
        class    = cls,
        onclick  = sprintf("Shiny.setInputValue('select_domain','%s',{priority:'event'})",
                           gsub("'", "\\'", r$domain, fixed = TRUE)),
        span(r$domain),
        span(class = "nav-count",
             if (r$n_unverified > 0L)
               sprintf("%d (%d?)", r$n_concepts, r$n_unverified)
             else as.character(r$n_concepts))
      )
    })

    unassigned_block <- if (nrow(unassigned) > 0L) {
      cls <- "nav-link"
      if (!is.null(active_domain()) && active_domain() == "Unassigned") {
        cls <- paste(cls, "active")
      }
      tagList(
        div(class = "nav-divider"),
        div(class = "nav-section-label", "Needs review"),
        tags$div(
          class   = cls,
          onclick = "Shiny.setInputValue('select_domain','Unassigned',{priority:'event'})",
          span("Unassigned",
               style = "color:#a44;"),
          span(class = "nav-count", as.character(unassigned$n_concepts[1]))
        )
      )
    } else NULL

    div(
      div(class = "nav-section-label", "Domains"),
      do.call(tagList, nav_items),
      unassigned_block
    )
  })

  observeEvent(input$select_domain, {
    active_domain(input$select_domain)
    selected_cell(NULL)
  })

  # On first load, pick the first domain
  observe({
    if (is.null(active_domain())) {
      ds <- domain_summary()
      if (nrow(ds) > 0L) {
        # Pick the first non-Unassigned if possible
        real <- ds$domain[ds$domain != "Unassigned"]
        if (length(real) > 0L) {
          active_domain(real[1])
        } else {
          active_domain(ds$domain[1])
        }
      }
    }
  })

  # ---- Main heatmap -------------------------------------------------------

  output$main_view <- renderUI({
    dom <- active_domain()
    if (is.null(dom)) {
      return(div(class = "placeholder",
                 "Select a domain from the left to begin."))
    }

    cw <- bundle()$crosswalk
    sub <- cw[cw$domain == dom, , drop = FALSE]
    if (nrow(sub) == 0L) {
      return(div(class = "placeholder",
                 sprintf("No concepts in '%s'.", dom)))
    }

    concepts <- sort(unique(sub$concept_id))
    years    <- sort(unique(sub$year))
    is_unassigned <- dom == "Unassigned"

    sel <- selected_cell()

    # Build the heatmap table
    rows <- lapply(concepts, function(cid) {
      crows <- sub[sub$concept_id == cid, ]

      # One <td> per year
      year_cells <- lapply(years, function(yr) {
        cell <- crows[crows$year == yr, ]
        if (nrow(cell) == 0L) {
          return(tags$td(class = "cell-unmapped", "\u2014"))
        }
        cell <- cell[1, ]  # safety
        kind <- .classify_cell(cell)
        cls <- paste0("cell-", kind)

        # Highlight if selected
        is_selected <- !is.null(sel) &&
                       sel$concept_id == cid &&
                       sel$year == yr
        if (is_selected) cls <- paste(cls, "cell-selected")

        # Click handler — only for non-unassigned cells
        click <- if (kind == "unassigned") {
          NULL
        } else {
          sprintf("Shiny.setInputValue('select_cell','%s|%d',{priority:'event'})",
                  gsub("'", "\\'", cid, fixed = TRUE),
                  as.integer(yr))
        }

        tags$td(
          class   = cls,
          onclick = click,
          title   = if (!is.na(cell$state_var) && nzchar(cell$state_var))
                      paste0(cell$state_var) else NULL,
          .cell_label(cell, kind)
        )
      })

      # Concept name cell with click-to-rename hint via tooltip
      tags$tr(
        tags$th(class = "concept-col",
                title = sprintf("Click any cell to edit. Concept: %s", cid),
                cid),
        do.call(tagList, year_cells)
      )
    })

    section_class <- if (is_unassigned)
                       "domain-section unassigned" else "domain-section"

    intro <- if (is_unassigned) {
      div(style = "font-size:12px;color:#666;margin-bottom:12px;",
          paste("Concepts whose domain couldn't be auto-assigned.",
                "Click a concept name to assign a domain;",
                "until you do, cells are not editable."))
    } else NULL

    div(class = section_class,
        h3(sprintf("Domain: %s", dom)),
        intro,
        if (is_unassigned) {
          # For unassigned, show concepts as a list with assign buttons
          tagList(
            do.call(tagList, lapply(concepts, function(cid) {
              crows <- sub[sub$concept_id == cid, ]
              n_years <- nrow(crows)
              n_yes <- sum(!is.na(crows$state_var) & nzchar(crows$state_var))
              div(style = paste0(
                    "display:flex;align-items:center;",
                    "padding:8px 12px;background:white;",
                    "margin-bottom:6px;border-radius:4px;",
                    "border:1px solid #eaeaea;"),
                  div(style = "flex:1;font-weight:600;",
                      cid),
                  div(style = "color:#888;font-size:11px;margin-right:12px;",
                      sprintf("%d/%d years mapped", n_yes, n_years)),
                  actionButton(
                    paste0("assign_", digest_id(cid)),
                    "Assign domain...",
                    class = "btn-sm btn-primary",
                    onclick = sprintf(
                      "Shiny.setInputValue('open_assign','%s',{priority:'event'})",
                      gsub("'", "\\'", cid, fixed = TRUE))
                  )
              )
            }))
          )
        } else {
          # Normal heatmap
          tags$table(class = "heatmap-table",
            tags$thead(tags$tr(
              tags$th(class = "concept-col", "Concept"),
              do.call(tagList, lapply(years, function(yr) tags$th(yr)))
            )),
            tags$tbody(do.call(tagList, rows))
          )
        }
    )
  })

  # Used to construct stable input ids from concept names with weird chars
  digest_id <- function(s) {
    gsub("[^A-Za-z0-9]", "_", s)
  }

  # ---- Cell click ----------------------------------------------------------

  observeEvent(input$select_cell, {
    parts <- strsplit(input$select_cell, "|", fixed = TRUE)[[1]]
    if (length(parts) != 2L) return()
    selected_cell(list(concept_id = parts[1],
                       year = as.integer(parts[2])))
  })

  # ---- Right edit panel ---------------------------------------------------

  output$edit_panel <- renderUI({
    sel <- selected_cell()
    if (is.null(sel)) {
      return(div(class = "edit-panel-empty",
                 "Click a cell in the heatmap to edit."))
    }

    cw <- bundle()$crosswalk
    row <- cw[cw$concept_id == sel$concept_id & cw$year == sel$year, ]
    if (nrow(row) == 0L) {
      return(div(class = "edit-panel-empty",
                 "(cell no longer exists)"))
    }
    row <- row[1, ]

    is_calc <- isTRUE(row$is_calculated == 1L)

    tagList(
      h4(style = "margin-top:0;",
         "Edit cell"),
      div(class = "field-row",
          tags$label("Concept"),
          div(style = "display:flex;align-items:center;gap:6px;",
              div(style = "font-weight:600;flex:1;", row$concept_id),
              actionButton("concept_rename_btn", "Rename",
                            class = "btn-sm btn-outline-secondary",
                            style = "font-size:10px;padding:2px 6px;"),
              actionButton("concept_merge_btn",  "Merge\u2026",
                            class = "btn-sm btn-outline-secondary",
                            style = "font-size:10px;padding:2px 6px;"),
              actionButton("concept_delete_btn", "Delete",
                            class = "btn-sm btn-outline-danger",
                            style = "font-size:10px;padding:2px 6px;"))),
      div(class = "field-row",
          tags$label("Year"),
          div(row$year)),
      div(class = "field-row",
          tags$label("Domain"),
          div(row$domain,
              if (!is.na(row$subdomain) && nzchar(row$subdomain))
                tags$span(style = "color:#888;",
                          sprintf(" \u00b7 %s", row$subdomain)))),

      tags$hr(),

      # Calculated toggle
      div(class = "field-row",
          checkboxInput("cell_is_calc",
                        label = "This cell is calculated",
                        value = is_calc)),

      # Mode-specific UI
      uiOutput("cell_edit_body"),

      tags$hr(),
      div(style = "display:flex;gap:8px;",
          actionButton("cell_save",   "Save",   class = "btn-sm btn-primary"),
          actionButton("cell_verify", "Verify", class = "btn-sm btn-success"),
          actionButton("cell_clear",  "Clear",  class = "btn-sm btn-secondary"))
    )
  })

  output$cell_edit_body <- renderUI({
    sel <- selected_cell()
    if (is.null(sel)) return(NULL)

    cw  <- bundle()$crosswalk
    row <- cw[cw$concept_id == sel$concept_id & cw$year == sel$year, ]
    if (nrow(row) == 0L) return(NULL)
    row <- row[1, ]

    is_calc <- isTRUE(input$cell_is_calc) ||
               (is.null(input$cell_is_calc) && row$is_calculated == 1L)

    if (is_calc) {
      yaml_text <- if (!is.na(row$calculation_yaml) &&
                       nzchar(row$calculation_yaml))
                     row$calculation_yaml else .example_calc_yaml()

      if (.has_shinyAce) {
        editor <- shinyAce::aceEditor(
          outputId = "cell_calc_yaml",
          value    = yaml_text,
          mode     = "yaml",
          theme    = "github",
          height   = "300px",
          fontSize = 12
        )
      } else {
        editor <- tags$textarea(
          id    = "cell_calc_yaml",
          class = "calc-yaml-area form-control",
          rows  = 14,
          yaml_text
        )
      }

      tagList(
        div(class = "field-row",
            tags$label("Calculation YAML (for this year only)"),
            editor,
            tags$small(style = "color:#888;",
                       "When you save, this YAML defines how the cell's value ",
                       "is computed at pull time."))
      )

    } else {
      vars <- .available_vars_for_cell(bundle(),
                                        sel$concept_id,
                                        sel$year)
      current <- if (!is.na(row$state_var) && nzchar(row$state_var))
                   row$state_var else NULL

      tagList(
        div(class = "field-row",
            tags$label(sprintf("State variable (from %d codebook)",
                                sel$year)),
            selectizeInput(
              "cell_state_var",
              label = NULL,
              choices = c("(unmapped)" = "", vars),
              selected = current %||% "",
              options = list(
                placeholder = "Type to search...",
                searchField = list("text", "value")
              )
            ),
            tags$small(style = "color:#888;",
                       sprintf("%d variable(s) available; ",
                               length(vars)),
                       "vars used by other concepts are hidden.")),

        # Codebook context (question text + value labels)
        uiOutput("cell_codebook_context")
      )
    }
  })

  # Codebook context: pulls from the codebook cache for the cell's year,
  # looks up the currently-selected state_var.
  output$cell_codebook_context <- renderUI({
    sel <- selected_cell()
    if (is.null(sel)) return(NULL)

    sv <- input$cell_state_var
    if (is.null(sv) || !nzchar(sv)) return(NULL)

    cb <- .get_codebook_for_year(sel$year)
    if (is.null(cb) || nrow(cb) == 0L) {
      return(div(class = "field-row",
                 tags$small(style = "color:#aaa;",
                            sprintf("(No codebook found for %d. Drop one in the documentation/ folder.)",
                                    sel$year))))
    }

    entry <- cb[cb$variable_name == sv, , drop = FALSE]
    if (nrow(entry) == 0L) {
      return(div(class = "field-row",
                 tags$small(style = "color:#aaa;",
                            sprintf("(Variable '%s' not found in %d codebook.)",
                                    sv, sel$year))))
    }
    entry <- entry[1, ]

    # Build the value labels block
    parsed <- entry$value_labels_parsed[[1]]
    vl_block <- if (length(parsed) > 0L) {
      div(style = "margin-top:8px;",
          tags$div(style = "font-size:11px;color:#666;text-transform:uppercase;letter-spacing:0.04em;margin-bottom:4px;",
                   "Value labels"),
          tags$table(style = "font-size:11px;border-collapse:collapse;",
            do.call(tagList, lapply(seq_along(parsed), function(i) {
              tags$tr(
                tags$td(style = "padding:2px 8px 2px 0;color:#666;font-family:monospace;",
                        names(parsed)[i]),
                tags$td(style = "padding:2px 0;", parsed[i])
              )
            }))
          ))
    } else NULL

    notes_block <- if (!is.na(entry$notes) && nzchar(entry$notes)) {
      div(style = "margin-top:8px;font-size:11px;color:#666;font-style:italic;",
          entry$notes)
    } else NULL

    qtext_block <- if (!is.na(entry$question_text) &&
                       nzchar(entry$question_text)) {
      div(style = "font-size:12px;color:#333;line-height:1.4;",
          entry$question_text)
    } else {
      div(style = "font-size:11px;color:#aaa;font-style:italic;",
          "(No question text recorded.)")
    }

    div(class = "field-row",
        style = "margin-top:16px;padding:12px;background:#f8f8f8;border-radius:4px;border-left:3px solid #6699cc;",
        tags$div(style = "font-size:11px;color:#666;text-transform:uppercase;letter-spacing:0.04em;margin-bottom:6px;",
                 "Codebook entry"),
        qtext_block,
        vl_block,
        notes_block)
  })

  # ---- Cell actions -------------------------------------------------------

  observeEvent(input$cell_save, {
    sel <- selected_cell()
    if (is.null(sel)) return()

    is_calc <- isTRUE(input$cell_is_calc)

    b <- bundle()
    if (is_calc) {
      yaml_text <- input$cell_calc_yaml
      if (is.null(yaml_text) || !nzchar(trimws(yaml_text))) {
        showNotification("Calculation YAML cannot be empty.",
                         type = "warning")
        return()
      }
      b <- tryCatch(
        cw_set_calc(b, sel$concept_id, sel$year, yaml_text),
        error = function(e) {
          showNotification(sprintf("Save failed: %s", conditionMessage(e)),
                            type = "error", duration = 8)
          NULL
        }
      )
    } else {
      sv <- input$cell_state_var
      if (is.null(sv) || !nzchar(sv)) sv <- NA_character_
      b <- tryCatch(
        cw_set_var(b, sel$concept_id, sel$year, sv),
        error = function(e) {
          showNotification(sprintf("Save failed: %s", conditionMessage(e)),
                            type = "error", duration = 8)
          NULL
        }
      )
    }

    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      showNotification(sprintf("Saved %s @ %d (in-memory; click Save in header to persist).",
                                sel$concept_id, sel$year),
                        type = "message", duration = 3)
    }
  })

  observeEvent(input$cell_verify, {
    sel <- selected_cell()
    if (is.null(sel)) return()
    b <- tryCatch(
      cw_verify(bundle(), sel$concept_id, sel$year),
      error = function(e) {
        showNotification(sprintf("Verify failed: %s", conditionMessage(e)),
                          type = "error", duration = 6)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      showNotification("Verified.", type = "message", duration = 2)
    }
  })

  observeEvent(input$cell_clear, {
    sel <- selected_cell()
    if (is.null(sel)) return()
    b <- tryCatch(
      cw_set_var(bundle(), sel$concept_id, sel$year, NA_character_),
      error = function(e) {
        showNotification(sprintf("Clear failed: %s", conditionMessage(e)),
                          type = "error", duration = 6)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      showNotification("Cell cleared.", type = "message", duration = 2)
    }
  })

  # ---- Domain assignment (from Unassigned section) ------------------------

  observeEvent(input$open_assign, {
    cid <- input$open_assign
    cw  <- bundle()$crosswalk
    existing_domains <- sort(unique(cw$domain[cw$domain != "Unassigned"]))

    showModal(modalDialog(
      title = sprintf("Assign domain for '%s'", cid),
      div(class = "field-row",
          tags$label("Domain"),
          selectizeInput("assign_domain", label = NULL,
                          choices = c("(pick one)" = "", existing_domains),
                          options = list(create = TRUE,
                                          placeholder = "Pick or type new..."))
      ),
      div(class = "field-row",
          tags$label("Subdomain (optional)"),
          textInput("assign_subdomain", label = NULL)
      ),
      tags$small(style = "color:#888;",
                 "Once assigned, the concept moves to its domain section ",
                 "and its cells become editable."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_assign", "Assign",
                      class = "btn-primary",
                      onclick = sprintf(
                        "Shiny.setInputValue('confirm_assign_concept','%s',{priority:'event'})",
                        gsub("'", "\\'", cid, fixed = TRUE)))
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_assign_concept, {
    cid <- input$confirm_assign_concept
    dom <- input$assign_domain
    sub <- input$assign_subdomain

    if (is.null(dom) || !nzchar(dom)) {
      showNotification("Domain is required.", type = "warning")
      return()
    }

    if (is.null(sub) || !nzchar(sub)) sub <- NA_character_

    b <- tryCatch(
      cw_assign_domain(bundle(), cid, dom, sub),
      error = function(e) {
        showNotification(sprintf("Assign failed: %s", conditionMessage(e)),
                          type = "error", duration = 6)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      removeModal()
      showNotification(sprintf("'%s' assigned to '%s'.", cid, dom),
                        type = "message")
    }
  })

  # ---- Concept-level actions: rename, merge, delete ----------------------

  observeEvent(input$concept_rename_btn, {
    sel <- selected_cell()
    if (is.null(sel)) return()
    showModal(modalDialog(
      title = sprintf("Rename concept '%s'", sel$concept_id),
      div(class = "field-row",
          tags$label("New name"),
          textInput("rename_to_input", label = NULL,
                     value = sel$concept_id,
                     width = "100%")),
      tags$small(style = "color:#888;",
                 "R-friendly name: letters, digits, underscores; ",
                 "no leading digit. Renames across all years."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename", "Rename",
                      class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_rename, {
    sel <- selected_cell()
    if (is.null(sel)) {
      removeModal()
      return()
    }
    new_name <- input$rename_to_input
    if (is.null(new_name) || !nzchar(new_name) ||
        new_name == sel$concept_id) {
      removeModal()
      return()
    }
    b <- tryCatch(
      cw_rename_concept(bundle(), sel$concept_id, new_name),
      error = function(e) {
        showNotification(sprintf("Rename failed: %s",
                                  conditionMessage(e)),
                          type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      selected_cell(list(concept_id = new_name, year = sel$year))
      removeModal()
      showNotification(sprintf("Renamed: %s \u2192 %s",
                                sel$concept_id, new_name),
                        type = "message")
    }
  })

  observeEvent(input$concept_merge_btn, {
    sel <- selected_cell()
    if (is.null(sel)) return()

    cw <- bundle()$crosswalk
    other_concepts <- sort(unique(cw$concept_id))
    other_concepts <- setdiff(other_concepts, sel$concept_id)

    showModal(modalDialog(
      title = sprintf("Merge '%s' into another concept", sel$concept_id),
      div(class = "field-row",
          tags$label("Merge into (target concept)"),
          selectizeInput("merge_into_input", label = NULL,
                          choices = c("(pick one)" = "", other_concepts),
                          options = list(placeholder = "Type to search..."))),
      tags$p(style = "font-size:12px;color:#666;",
             tags$strong(sel$concept_id),
             " will be deleted. Its mappings move into the target. ",
             "If both have a mapping for the same year, you'll get an error ",
             "and need to clear one of the cells first."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_merge", "Merge",
                      class = "btn-warning")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_merge, {
    sel <- selected_cell()
    if (is.null(sel)) {
      removeModal()
      return()
    }
    target <- input$merge_into_input
    if (is.null(target) || !nzchar(target)) {
      showNotification("Pick a target concept.", type = "warning")
      return()
    }
    b <- tryCatch(
      cw_merge_concepts(bundle(), into = target, from = sel$concept_id),
      error = function(e) {
        showNotification(sprintf("Merge failed: %s",
                                  conditionMessage(e)),
                          type = "error", duration = 12)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      # Selected concept is gone; switch selection to the merge target
      selected_cell(list(concept_id = target, year = sel$year))
      removeModal()
      showNotification(sprintf("Merged: %s \u2192 %s",
                                sel$concept_id, target),
                        type = "message")
    }
  })

  observeEvent(input$concept_delete_btn, {
    sel <- selected_cell()
    if (is.null(sel)) return()

    cw <- bundle()$crosswalk
    n_mapped <- sum(cw$concept_id == sel$concept_id &
                    !is.na(cw$state_var) & nzchar(cw$state_var))

    showModal(modalDialog(
      title = sprintf("Delete concept '%s'?", sel$concept_id),
      tags$p(sprintf(
        "This removes ALL years for the concept '%s'.",
        sel$concept_id)),
      if (n_mapped > 0L)
        tags$p(style = "color:#a44;",
               sprintf("Warning: %d year(s) have non-empty mappings ",
                        n_mapped),
               "that will be lost. ",
               "If you want to preserve them, use Merge instead."),
      tags$p(style = "font-size:12px;color:#888;",
             "Backups are kept on save (.bak rotation in your config dir)."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete",
                      class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete, {
    sel <- selected_cell()
    if (is.null(sel)) {
      removeModal()
      return()
    }
    b <- tryCatch(
      cw_remove_concept(bundle(), sel$concept_id),
      error = function(e) {
        showNotification(sprintf("Delete failed: %s",
                                  conditionMessage(e)),
                          type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(b)) {
      bundle(b)
      dirty(TRUE)
      selected_cell(NULL)
      removeModal()
      showNotification(sprintf("Deleted concept '%s'.",
                                sel$concept_id),
                        type = "message")
    }
  })

  # ---- Save / Reload ------------------------------------------------------

  observeEvent(input$save_btn, {
    if (!dirty()) {
      showNotification("No changes to save.", type = "default")
      return()
    }
    res <- tryCatch({
      cw_save(bundle(), dataset = ed_dataset)
      TRUE
    }, error = function(e) {
      showNotification(sprintf("Save failed: %s", conditionMessage(e)),
                        type = "error", duration = 8)
      FALSE
    })
    if (isTRUE(res)) {
      dirty(FALSE)
      showNotification("Saved to disk (with .bak rotation).",
                        type = "message")
    }
  })

  observeEvent(input$reload_btn, {
    if (dirty()) {
      showModal(modalDialog(
        title = "Discard unsaved changes?",
        "You have unsaved changes. Reload from disk anyway?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_reload", "Reload",
                        class = "btn-warning")
        ),
        easyClose = TRUE
      ))
    } else {
      bundle(cw_load(dataset = ed_dataset))
      selected_cell(NULL)
      showNotification("Reloaded.", type = "message")
    }
  })

  observeEvent(input$confirm_reload, {
    removeModal()
    bundle(cw_load(dataset = ed_dataset))
    selected_cell(NULL)
    dirty(FALSE)
    showNotification("Reloaded.", type = "message")
  })
}
