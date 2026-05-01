# inst/shiny/editor/ui.R
#
# brfssTools editor v0.2.0
#
# Single view: domain-grouped heatmap with Unassigned pinned to the
# bottom. Click a cell to edit it via the right-side panel. No CDC-
# anchored / State-anchored / Demographics tabs.

library(shiny)
library(bslib)
library(htmltools)

# Try to load shinyAce if available; fall back to plain textarea otherwise.
.has_shinyAce <- requireNamespace("shinyAce", quietly = TRUE)

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5),
  title = "brfssTools editor",

  # ---- Top header -----------------------------------------------------------
  div(class = "editor-header",
      style = paste0(
        "display:flex;align-items:center;gap:14px;",
        "padding:8px 16px;border-bottom:1px solid #ddd;",
        "background:#fafafa;flex:0 0 auto;"),
      tags$strong(style = "font-size:14px;color:#333;",
                  "brfssTools editor"),
      div(style = "color:#777;font-size:12px;flex:1;",
          textOutput("status_meta", inline = TRUE)),
      actionButton("save_btn",   "Save",   class = "btn-sm btn-primary"),
      actionButton("reload_btn", "Reload", class = "btn-sm btn-secondary")
  ),

  # ---- Main area: 3-pane layout --------------------------------------------
  div(style = paste0(
        "display:grid;grid-template-columns:230px 1fr 360px;",
        "gap:0;flex:1;min-height:0;overflow:hidden;"),

      # Left nav: domain list
      div(class = "left-nav",
          style = paste0(
            "border-right:1px solid #ddd;background:#f5f5f5;",
            "overflow-y:auto;min-height:0;"),
          uiOutput("domain_nav")
      ),

      # Center: heatmap for active domain
      div(class = "main-pane",
          style = "overflow:auto;padding:16px;min-height:0;background:#fff;",
          uiOutput("main_view")
      ),

      # Right: edit panel
      div(class = "edit-pane",
          style = paste0(
            "border-left:1px solid #ddd;background:#fafafa;",
            "overflow-y:auto;padding:16px;min-height:0;"),
          uiOutput("edit_panel")
      )
  ),

  # ---- CSS ------------------------------------------------------------------
  tags$head(tags$style(HTML("
    .heatmap-table {
      border-collapse: collapse;
      width: auto;
      font-size: 12px;
      font-family: -apple-system, system-ui, sans-serif;
    }
    .heatmap-table th, .heatmap-table td {
      padding: 4px 8px;
      border: 1px solid #e5e5e5;
      text-align: center;
      vertical-align: middle;
      min-width: 56px;
    }
    .heatmap-table th.concept-col {
      text-align: left;
      font-weight: 600;
      min-width: 180px;
      max-width: 240px;
      padding-left: 12px;
      background: #fafafa;
      position: sticky;
      left: 0;
      z-index: 1;
    }
    .heatmap-table tbody tr:hover {
      background: #f8f8f8;
    }
    .cell-mapped     { background: #d1e7dd; cursor: pointer; }
    .cell-unverified { background: #fff3cd; cursor: pointer; }
    .cell-unmapped   { background: #fff;    cursor: pointer; color: #aaa; }
    .cell-calc       { background: #cfe2ff; cursor: pointer; font-weight: 600; }
    .cell-unassigned { background: #f5f5f5; cursor: not-allowed; color: #ccc; }
    .cell-selected   { outline: 2px solid #0d6efd; outline-offset: -1px; }

    .domain-section {
      margin-bottom: 24px;
    }
    .domain-section h3 {
      font-size: 14px;
      margin: 8px 0 6px 0;
      color: #444;
      padding: 4px 8px;
      background: #f0f0f0;
      border-radius: 3px;
    }
    .domain-section.unassigned h3 {
      background: #ffe5e5;
      color: #882222;
    }

    .nav-link {
      display: flex;
      justify-content: space-between;
      padding: 6px 12px;
      cursor: pointer;
      font-size: 13px;
      color: #333;
      border-bottom: 1px solid #eaeaea;
    }
    .nav-link:hover { background: #e8e8e8; }
    .nav-link.active {
      background: #d0e2ff;
      font-weight: 600;
      color: #0d3a82;
    }
    .nav-count {
      color: #888;
      font-size: 11px;
    }
    .nav-divider {
      border-top: 2px solid #aaa;
      margin: 8px 0;
    }
    .nav-section-label {
      padding: 6px 12px;
      font-size: 11px;
      color: #888;
      text-transform: uppercase;
      letter-spacing: 0.04em;
    }

    .placeholder {
      color: #999;
      font-style: italic;
      padding: 24px;
      text-align: center;
    }

    .edit-panel-empty {
      color: #999;
      font-size: 13px;
      padding: 16px 0;
      text-align: center;
    }
    .field-row {
      margin-bottom: 12px;
    }
    .field-row label {
      display: block;
      font-size: 11px;
      color: #666;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      margin-bottom: 4px;
    }
    .calc-yaml-area {
      font-family: 'Consolas', 'Monaco', monospace;
      font-size: 11px;
      width: 100%;
      min-height: 200px;
    }
  ")))
)
