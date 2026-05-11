# =============================================================================
# app/modules/mod_help.R
#
# In-app user manual. Renders docs/operator_runbook.md so the people
# actually using the app don't have to dig around the filesystem to find
# instructions.
#
# Read-only. Reads:
#   <project_root>/docs/operator_runbook.md
#
# This page deliberately surfaces ONLY the user-facing runbook — not
# h12_design.md, h7_design_notes.md, architecture.md, etc. Those are
# for code reviewers and stay in the docs/ folder. Mixing them into
# the in-app help would dilute the manual and confuse non-developer
# users.
# =============================================================================

mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("User manual"),
      p(class = "small-muted",
        "How to use this app, day-to-day. The first section ",
        "(\"Getting started\") is for whoever set up the app on this ",
        "machine; everything below is for everyday users.")
    )),
    fluidRow(column(12,
      uiOutput(ns("manual_body"))
    ))
  )
}


mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$manual_body <- renderUI({
      proj_root <- getOption("ifrs9.project_root", getwd())
      md_path <- file.path(proj_root, "docs", "operator_runbook.md")

      if (!file.exists(md_path)) {
        return(div(class = "alert alert-warning",
                    "User manual not found at ", tags$code(md_path),
                    ". Check the deployment."))
      }

      # Read + render. Use markdown::markdownToHTML if available; fall
      # back to commonmark::markdown_html (Shiny pulls this in via bslib
      # so it's almost always present); fall back to a plain <pre>
      # block as the last resort so the user always sees something.
      md_text <- paste(readLines(md_path, warn = FALSE), collapse = "\n")

      html <- NULL
      if (requireNamespace("markdown", quietly = TRUE)) {
        html <- tryCatch(
          markdown::markdownToHTML(text = md_text, fragment.only = TRUE),
          error = function(e) NULL
        )
      }
      if (is.null(html) && requireNamespace("commonmark", quietly = TRUE)) {
        html <- tryCatch(commonmark::markdown_html(md_text),
                          error = function(e) NULL)
      }
      if (is.null(html)) {
        return(tags$pre(style = "white-space: pre-wrap; font-size: 0.9em;",
                          md_text))
      }

      # The rendered HTML often comes wide; constrain its width and
      # add some breathing room so it reads like a document, not a
      # database dump.
      div(style = "max-width: 900px; line-height: 1.55;",
          HTML(html))
    })
  })
}
