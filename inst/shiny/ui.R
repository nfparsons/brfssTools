# inst/shiny/editor/ui.R
#
# UI for the brfssTools crosswalk editor.
# Pattern: left-rail nav (domains), main heatmap, right-rail edit panel.

library(shiny)
library(bslib)

# Heatmap CSS lifted from the static dashboard, with edit-affordance tweaks.
editor_css <- HTML("
* { box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
       'Helvetica Neue', Arial, sans-serif; }

.editor-header {
  background: #2F5496; color: white; padding: 12px 24px;
  display: flex; align-items: baseline; gap: 16px;
}
.editor-header h1 { margin: 0; font-size: 17px; font-weight: 600; }
.editor-header .meta { font-size: 13px; opacity: 0.85; }

.editor-layout {
  display: grid;
  grid-template-columns: 250px 1fr 380px;
  height: calc(100vh - 50px);
}
.nav-pane {
  background: white; border-right: 1px solid #e0e0e0;
  overflow-y: auto; padding: 12px 0;
}
.nav-pane h2 {
  margin: 0 0 6px 0; padding: 0 16px; font-size: 11px; font-weight: 600;
  text-transform: uppercase; color: #707070; letter-spacing: 0.5px;
}
.nav-btn {
  display: flex; justify-content: space-between; align-items: center;
  width: 100%; padding: 9px 16px; border: none; background: transparent;
  text-align: left; cursor: pointer; font-size: 13px; color: #1a1a1a;
}
.nav-btn:hover { background: #f0f4fa; }
.nav-btn.active { background: #2F5496; color: white; }
.nav-btn .count {
  font-size: 11px; color: #888; background: #f0f0f0;
  padding: 2px 7px; border-radius: 10px;
}
.nav-btn.active .count { background: rgba(255,255,255,0.2); color: white; }

.main-pane { overflow: auto; padding: 18px 24px; background: #f4f5f7; }
.main-pane h2 { margin: 0 0 6px 0; font-size: 20px; font-weight: 600; }

.subdomain-block {
  margin: 16px 0; background: white; border-radius: 6px;
  box-shadow: 0 1px 2px rgba(0,0,0,0.05); overflow: hidden;
}
.subdomain-header {
  padding: 10px 14px; background: #f8f9fa; border-bottom: 1px solid #e0e0e0;
  display: flex; justify-content: space-between; align-items: baseline;
}
.subdomain-header h3 { margin: 0; font-size: 14px; font-weight: 600; }
.subdomain-header .badge { font-size: 12px; color: #666; }

table.heatmap { border-collapse: collapse; width: 100%; font-size: 12px; }
table.heatmap th, table.heatmap td {
  text-align: left; padding: 4px 8px; border: none;
}
table.heatmap thead th {
  position: sticky; top: 0; background: #fafafa; z-index: 2;
  font-weight: 600; border-bottom: 1px solid #e0e0e0; text-align: center;
}
table.heatmap thead th.label-col { text-align: left; }
table.heatmap tbody tr:nth-child(odd) { background: #fbfbfb; }
table.heatmap tbody tr.row-selected { background: #fff8e1 !important; }
table.heatmap tbody tr.concept-row { cursor: pointer; }
table.heatmap tbody tr.concept-row:hover { background: #fff8e1; }

.label-cell {
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
  font-size: 11.5px; color: #1a1a1a;
}
.label-cell .meta {
  color: #888; font-style: italic; font-family: inherit;
  font-size: 11px; margin-left: 6px;
}
.year-cell {
  text-align: center; width: 56px; height: 22px;
  font-size: 10px; border-left: 1px solid white;
}
.k-P { background: #C6EFCE; color: #1f5025; }
.k-O { background: #FFEB9C; color: #6b5810; }
.k-C { background: #FCE4D6; color: #7a3a1a; }
.k-A { background: #DDEBF7; color: #244673; }
.k-B { background: #D9D9D9; color: #444; }
.k-M { background: transparent; }

.edit-pane {
  background: white; border-left: 1px solid #e0e0e0;
  padding: 16px 20px; overflow-y: auto;
}
.edit-pane h3 {
  margin: 0 0 6px 0; font-size: 14px; font-weight: 600;
  color: #2F5496; text-transform: uppercase; letter-spacing: 0.5px;
}
.edit-pane .placeholder {
  color: #888; font-size: 13px; padding: 20px 0; line-height: 1.5;
}
.edit-pane .field-group {
  margin: 14px 0; padding: 12px; background: #f8f9fa;
  border-radius: 5px; border: 1px solid #eee;
}
.edit-pane label { font-size: 12px; font-weight: 600; color: #333; }
.edit-pane .var-name {
  font-family: 'SFMono-Regular', Consolas, monospace;
  font-size: 13px; color: #2F5496; font-weight: 600;
}
.edit-pane .question-text {
  font-size: 12px; color: #555; line-height: 1.4;
  margin-top: 4px; max-height: 80px; overflow-y: auto;
}

.action-row {
  display: flex; gap: 8px; flex-wrap: wrap; margin-top: 6px;
}
.action-row button {
  font-size: 12px; padding: 5px 10px;
}

.dirty-indicator {
  background: #f0ad4e; color: white; padding: 2px 8px;
  border-radius: 10px; font-size: 11px; margin-left: 12px;
  display: none;
}
.dirty-indicator.show { display: inline-block; }

.legend {
  display: flex; gap: 12px; flex-wrap: wrap; padding: 10px 14px;
  font-size: 11px; background: #fafafa; border-top: 1px solid #e0e0e0;
}
.legend span { display: inline-flex; align-items: center; gap: 4px; }
.legend .swatch {
  display: inline-block; width: 12px; height: 12px;
  border: 1px solid #ccc; border-radius: 2px;
}
")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5),
  tags$head(tags$style(editor_css)),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('select_concept', function(payload) {
      // Highlight the row by its (year, state_var, cdc_var, idx) hash
      document.querySelectorAll('tr.concept-row').forEach(r => r.classList.remove('row-selected'));
      var sel = document.querySelector('tr[data-cid=\"' + payload.cid + '\"]');
      if (sel) sel.classList.add('row-selected');
    });
  ")),

  div(class = "editor-header",
      h1("brfssTools Crosswalk Editor"),
      span(class = "meta", textOutput("status_meta", inline = TRUE)),
      span(id = "dirty-indicator", class = "dirty-indicator",
           textOutput("dirty_text", inline = TRUE)),
      div(style = "margin-left: auto;",
          actionButton("save_btn", "Save",
                       class = "btn-success btn-sm"),
          actionButton("reload_btn", "Reload from disk",
                       class = "btn-secondary btn-sm",
                       style = "margin-left: 6px;"))
  ),

  div(class = "editor-layout",
      # Left: domain navigation
      div(class = "nav-pane",
          h2("Domains"),
          uiOutput("domain_nav"),
          h2("New", style = "margin-top: 24px;"),
          actionButton("add_pair_btn", "Add new pair...",
                       class = "btn-primary btn-sm",
                       style = "margin: 6px 16px; width: calc(100% - 32px);")
      ),
      # Middle: heatmap
      div(class = "main-pane",
          uiOutput("domain_view")
      ),
      # Right: edit panel
      div(class = "edit-pane",
          h3("Edit Pair"),
          uiOutput("edit_panel")
      )
  )
)
