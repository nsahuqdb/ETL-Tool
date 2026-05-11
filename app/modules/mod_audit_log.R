# =============================================================================
# app/modules/mod_audit_log.R
#
# The Audit log page: a filterable, human-friendly view of
# logs/etl_audit.jsonl. Every meaningful event in the system writes one
# JSON line there. Read-only.
#
# Display layout:
#   - 5 columns: ts, event, user, run_id, summary
#   - The `summary` column is computed from the event payload — one line
#     of plain English instead of raw JSON
#   - Three filter dropdowns at the top: event type, run ID, user
#
# Reads:
#   read_audit_log()      from R/audit_log.R
# =============================================================================

mod_audit_log_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Audit log"),
      p(class = "small-muted",
        textOutput(ns("audit_log_label"), inline = TRUE)),
      actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                    class = "btn-sm btn-outline-secondary")
    )),
    fluidRow(
      column(4, selectInput(ns("event_filter"), "Event type",
                              choices = c("(all)" = ""), selected = "")),
      column(4, selectInput(ns("run_filter"), "Run ID",
                              choices = c("(all)" = ""), selected = "")),
      column(4, selectInput(ns("user_filter"), "User",
                              choices = c("(all)" = ""), selected = ""))
    ),
    fluidRow(column(12,
      DT::DTOutput(ns("audit_table"))
    ))
  )
}


# Build a one-line human-friendly summary for an audit row.
.audit_summary <- function(row) {
  ev <- row$event %||% ""
  s <- function(field) {
    v <- row[[field]]
    if (is.null(v) || (length(v) == 1 && (is.na(v) || !nzchar(as.character(v)))))
      "—" else as.character(v)
  }
  switch(
    ev,
    "run_start" = sprintf(
      "Run started — snapshot=%s, code=%s",
      s("snapshot"), substr(s("code_sha"), 1, 8)
    ),
    "run_finish" = sprintf(
      "Run finished — %s outputs in %ss",
      s("n_outputs"),
      if (!is.null(row$duration_seconds))
        sprintf("%.1f", as.numeric(row$duration_seconds))
      else "?"
    ),
    "validation_summary" = sprintf(
      "%s validation: %s/%s passed (E:%s W:%s I:%s S:%s)",
      s("stage"), s("n_pass"), s("n_total"),
      s("n_error"), s("n_warn"), s("n_info"), s("n_suppressed")
    ),
    "pre_run_check" = sprintf(
      "Pre-run check: %s/%s passed",
      s("n_pass"), s("n_total")
    ),
    "snapshot_create" = sprintf(
      "Snapshot created: %s (parent=%s)",
      s("snapshot"), s("parent")
    ),
    "snapshot_promote" = sprintf(
      "Snapshot %s: %s → %s",
      s("snapshot"), s("from_status"), s("to_status")
    ),
    "suppression_add" = sprintf(
      "Suppression added: %s (until %s)",
      s("validator_id"), s("valid_until")
    ),
    # Unknown event type — render any extra fields generically
    {
      std <- c("ts", "event", "user", "run_id")
      extras <- setdiff(names(row), std)
      if (length(extras) == 0) "" else
        paste(sprintf("%s=%s", extras,
                      vapply(extras, function(k) s(k), character(1))),
              collapse = ", ")
    }
  )
}


mod_audit_log_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    observeEvent(input$refresh, refresh(refresh() + 1))

    audit <- reactive({
      refresh()
      tryCatch(read_audit_log(),
                error = function(e) {
                  showNotification(paste("Failed to read audit log:",
                                          conditionMessage(e)), type = "error")
                  tibble::tibble()
                })
    })

    observe({
      a <- audit()
      events <- if ("event" %in% names(a)) sort(unique(a$event)) else character()
      runs   <- if ("run_id" %in% names(a))
                  sort(unique(a$run_id[!is.na(a$run_id) & nzchar(a$run_id)]),
                       decreasing = TRUE)
                else character()
      users  <- if ("user" %in% names(a)) sort(unique(a$user)) else character()

      updateSelectInput(session, "event_filter",
                         choices = c("(all)" = "", events),
                         selected = isolate(input$event_filter) %||% "")
      updateSelectInput(session, "run_filter",
                         choices = c("(all)" = "", runs),
                         selected = isolate(input$run_filter) %||% "")
      updateSelectInput(session, "user_filter",
                         choices = c("(all)" = "", users),
                         selected = isolate(input$user_filter) %||% "")
    })

    output$audit_log_label <- renderText({
      p <- audit_log_path()
      p_norm <- normalizePath(p, mustWork = FALSE)
      if (!file.exists(p)) {
        sprintf("audit log: %s   (does not exist yet)", p_norm)
      } else {
        sprintf("audit log: %s   |   %d events", p_norm, nrow(audit()))
      }
    })

    output$audit_table <- DT::renderDT({
      a <- audit()
      if (nrow(a) == 0) {
        return(DT::datatable(
          data.frame(message =
            "No audit events yet. Run the pipeline (run_etl, pre_run_check) or interact with snapshots/suppressions to populate this log."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }

      if (nzchar(input$event_filter %||% ""))
        a <- a[a$event == input$event_filter, , drop = FALSE]
      if (nzchar(input$run_filter %||% "") && "run_id" %in% names(a))
        a <- a[!is.na(a$run_id) & a$run_id == input$run_filter, , drop = FALSE]
      if (nzchar(input$user_filter %||% "") && "user" %in% names(a))
        a <- a[a$user == input$user_filter, , drop = FALSE]

      if (nrow(a) == 0) {
        return(DT::datatable(
          data.frame(message = "No events match the current filter."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }

      summaries <- vapply(seq_len(nrow(a)),
                           function(i) .audit_summary(as.list(a[i, , drop = FALSE])),
                           character(1))

      display <- data.frame(
        ts      = a$ts %||% NA_character_,
        event   = a$event %||% NA_character_,
        user    = a$user %||% NA_character_,
        run_id  = a$run_id %||% NA_character_,
        summary = summaries,
        stringsAsFactors = FALSE
      )
      display <- display[order(display$ts, decreasing = TRUE), ]

      DT::datatable(
        display,
        rownames = FALSE,
        class = "narrow-table compact",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          deferRender = TRUE
        )
      )
    })
  })
}
