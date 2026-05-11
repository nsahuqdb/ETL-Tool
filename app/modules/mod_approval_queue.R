# =============================================================================
# app/modules/mod_approval_queue.R
#
# Approval queue: lists runs that finished and are waiting for reviewer
# sign-off. Drilling into a run shows its outputs, validation, applied
# overrides, and the audit-log entries that came from this run. Reviewer
# clicks Approve or Reject with a required reason.
#
# Calls these pipeline helpers (from R/run_approval.R + run_discovery.R):
#   list_runs_pending_approval
#   read_run_status
#   approve_run
#   reject_run
#   read_run_validation
#   list_run_outputs
# =============================================================================

mod_approval_queue_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Approval queue"),
      p(class = "small-muted",
        "Review and approve completed runs and snapshot promotions. ",
        "Click Refresh on each tab to pick up new items."),
      p(class = "small-muted",
        tags$strong("Run approval"), " accepts the entire run including any ",
        "overrides applied during it. ",
        tags$strong("Snapshot approval"), " promotes a config bundle from ",
        tags$em("pending"), " to ", tags$em("approved"), ", making it ",
        "available for use in production runs.")
    )),
    navset_card_tab(
      nav_panel("Pending",
        navset_card_pill(
          nav_panel("Runs",      uiOutput(ns("pending_block"))),
          nav_panel("Snapshots", uiOutput(ns("pending_snap_block")))
        )
      ),
      nav_panel("History",
        navset_card_pill(
          nav_panel("Runs",      uiOutput(ns("history_block"))),
          nav_panel("Snapshots", uiOutput(ns("history_snap_block")))
        )
      )
    )
  )
}


mod_approval_queue_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    # NOTE: Auto-refresh was attempted in H13b via reactivePoll on the
    # audit log's mtime. It caused the app to stall — pages went blank
    # and snapshot creation hung. Removed for now. The Refresh button
    # in each tab is still available; we'll revisit auto-refresh with
    # a different mechanism (probably an explicit invalidator triggered
    # from mod_run_trigger and mod_snapshot_manager) once we understand
    # what failed.

    queue_tbl <- reactive({
      refresh()
      # Cross-module: bumped by mod_run_trigger when phase 2 finishes,
      # so a freshly-completed run appears in the queue without
      # requiring a manual refresh click.
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed()
      }
      tryCatch(list_runs_pending_approval(),
                error = function(e) {
                  showNotification(paste("Failed:", conditionMessage(e)),
                                    type = "error")
                  list_runs.empty()
                })
    })

    history_tbl <- reactive({
      refresh()
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed()
      }
      tryCatch(list_runs_decided(),
                error = function(e) {
                  showNotification(paste("Failed to read history:",
                                          conditionMessage(e)),
                                    type = "error")
                  NULL
                })
    })

    # ---- Pending block -------------------------------------------------
    output$pending_block <- renderUI({
      tagList(
        div(style = "margin-bottom: 0.75em;",
          actionButton(ns("refresh_pending"), "Refresh",
                        icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary")
        ),
        DT::DTOutput(ns("queue_table")),
        uiOutput(ns("detail"))
      )
    })
    observeEvent(input$refresh_pending, refresh(refresh() + 1))

    output$queue_table <- DT::renderDT({
      r <- queue_tbl()
      if (nrow(r) == 0) {
        return(DT::datatable(
          data.frame(message = "No runs are awaiting approval."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      # Status pill: pending_approval is the normal case; "unknown"
      # Status pill: pending_checker (new) or pending_approval (legacy)
      # both render as "PENDING CHECKER". "unknown" indicates a run whose
      # run_status.yml is missing or unparseable.
      status_pill <- ifelse(
        r$status %in% c("pending_checker", "pending_approval"),
        '<span class="pill pill-pending">PENDING CHECKER</span>',
        sprintf('<span class="pill pill-suppr" title="run_status.yml missing or unreadable">%s</span>',
                toupper(r$status)))
      display <- data.frame(
        status     = status_pill,
        run_id     = r$run_id,
        started    = r$started_at,
        duration_s = round(r$duration_seconds, 1),
        user       = r$user,
        snapshot   = ifelse(is.na(r$snapshot_label),
                              "(live config)", r$snapshot_label),
        outputs    = r$n_outputs,
        val_fail   = r$n_validation_failures,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display, selection = "single", rownames = FALSE,
        escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    selected_run <- reactive({
      idx <- input$queue_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      r <- queue_tbl()
      if (idx > nrow(r)) return(NULL)
      r[idx, , drop = FALSE]
    })

    output$detail <- renderUI({
      sr <- selected_run()
      if (is.null(sr)) {
        return(div(class = "small-muted", style = "margin-top: 2em;",
                    "Select a run above to review and approve."))
      }
      # Approval card content depends on:
      #   - who ran the pipeline (maker, from manifest.json)
      #   - who's logged in now (checker)
      #   - whether separation of duties is enforced
      maker <- tryCatch(.maker_for_run(sr$path),
                         error = function(e) NA_character_)
      checker <- Sys.info()[["user"]] %||% "unknown"
      cfg <- tryCatch(.approval_cfg(),
                       error = function(e) list(enforce_separation_of_duties = FALSE))
      enforce <- isTRUE(cfg$enforce_separation_of_duties)
      same_user <- !is.na(maker) && nzchar(maker) &&
                    identical(tolower(maker), tolower(checker))
      can_approve <- !(enforce && same_user)

      banner <- if (!enforce) {
        div(class = "alert alert-info", style = "margin-bottom: 1em;",
            tags$strong("Dev mode: "),
            "Separation of duties is currently DISABLED ",
            tags$code("(approval.enforce_separation_of_duties: false"),
            "). The user who ran the pipeline can also approve it. ",
            "In production, set the flag to ", tags$code("true"),
            " to require a different user as approver.")
      } else {
        NULL
      }
      maker_block <- if (!is.na(maker) && nzchar(maker)) {
        tagList(
          tags$dt(class = "col-sm-3", "Maker (ran the pipeline)"),
            tags$dd(class = "col-sm-9", tags$code(maker))
        )
      } else {
        tagList(
          tags$dt(class = "col-sm-3", "Maker"),
            tags$dd(class = "col-sm-9",
                     tags$em(class = "small-muted",
                              "(not recorded in manifest)"))
        )
      }

      approve_btn <- if (can_approve) {
        actionButton(ns("do_approve"), "Approve",
                      icon = icon("check"), class = "btn-success")
      } else {
        # Disabled: render a non-interactive button + an explanation.
        tagList(
          tags$button(
            id = ns("do_approve_disabled"),
            class = "btn btn-success",
            disabled = NA,
            style = "opacity: 0.5; cursor: not-allowed;",
            icon("check"), " Approve"
          ),
          tags$span(class = "small-muted",
                    style = "margin-left: 0.75em;",
                    sprintf("(disabled: %s ran this pipeline; ",
                             maker),
                    "another user must approve)")
        )
      }

      tagList(
        hr(),
        h4(sprintf("Run %s", sr$run_id)),
        p(class = "small-muted", sprintf("path: %s", sr$path)),
        banner,
        navset_card_tab(
          nav_panel("Validation", uiOutput(ns("val_panel"))),
          nav_panel("Overrides applied", uiOutput(ns("ov_panel"))),
          nav_panel("Outputs", uiOutput(ns("out_panel")))
        ),
        hr(),
        card(
          card_header("Approve / Reject"),
          tags$dl(class = "row",
            maker_block,
            tags$dt(class = "col-sm-3", "Acting as (current user)"),
              tags$dd(class = "col-sm-9", tags$code(checker)),
            tags$dt(class = "col-sm-3", "Separation enforced"),
              tags$dd(class = "col-sm-9",
                       if (enforce) tags$strong("Yes")
                       else tags$em("No (dev mode)"))
          ),
          textAreaInput(ns("decision_reason"), "Reason (required)", rows = 3,
                         placeholder = "Document the basis for approval or rejection. Will appear in the audit log."),
          approve_btn,
          tags$span(style = "margin-left: 0.5em;",
            actionButton(ns("do_reject"), "Reject",
                          icon = icon("xmark"), class = "btn-warning"))
        )
      )
    })

    output$val_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path)
      if (is.null(v) || nrow(v) == 0) return(p(class = "small-muted", "No validation report."))
      n_pass <- sum(as.logical(v$passed))
      n_fail <- sum(!as.logical(v$passed))
      tagList(
        p(sprintf("%d/%d passed (%d failures)", n_pass, nrow(v), n_fail)),
        DT::DTOutput(ns("val_table"))
      )
    })

    output$val_table <- DT::renderDT({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path); if (is.null(v)) return(NULL)
      v$passed <- as.logical(v$passed)
      v$status <- ifelse(v$passed,
                          '<span class="pill pill-pass">PASS</span>',
                          sprintf('<span class="pill pill-%s">%s</span>',
                                  tolower(v$severity %||% "info"),
                                  toupper(v$severity %||% "INFO")))
      DT::datatable(
        data.frame(status=v$status, stage=v$stage, id=v$id,
                    description=v$description, message=v$message,
                    stringsAsFactors=FALSE),
        rownames = FALSE, escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15, dom = "tip")
      )
    })

    output$ov_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      ov_dir <- file.path(sr$path, "overrides")
      if (!dir.exists(ov_dir)) {
        return(p(class = "small-muted", "No overrides directory."))
      }
      files <- list.files(ov_dir, pattern = "\\.csv$", full.names = TRUE)
      if (length(files) == 0) {
        return(p(class = "small-muted", "(no override files)"))
      }
      sections <- lapply(files, function(f) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                        error = function(e) NULL)
        if (is.null(df)) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(could not read file)")))
        }
        if (nrow(df) == 0) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(empty)")))
        }
        # Render as a static HTML table — no DT inside renderUI
        rows <- lapply(seq_len(nrow(df)), function(i) {
          tags$tr(lapply(df[i, , drop = TRUE], function(v) tags$td(as.character(v))))
        })
        tagList(
          h6(basename(f)),
          tags$table(class = "table table-sm narrow-table",
            tags$thead(tags$tr(lapply(colnames(df), tags$th))),
            tags$tbody(rows)
          )
        )
      })
      do.call(tagList, sections)
    })

    output$out_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      paths <- list_run_outputs(sr$path)
      if (length(paths) == 0) return(p(class = "small-muted", "No outputs found."))
      sizes <- file.info(paths)$size
      df <- data.frame(file = basename(paths),
                        size_kb = round(sizes/1024, 1),
                        stringsAsFactors = FALSE)
      rows <- lapply(seq_len(nrow(df)), function(i) {
        tags$tr(tags$td(df$file[i]),
                 tags$td(sprintf("%.1f KB", df$size_kb[i])))
      })
      tags$table(class = "table table-sm narrow-table",
        tags$thead(tags$tr(tags$th("File"), tags$th("Size"))),
        tags$tbody(rows)
      )
    })

    # ---- Approve / Reject -----------------------------------------
    .decide <- function(target) {
      sr <- selected_run(); if (is.null(sr)) return()
      reason <- trimws(input$decision_reason %||% "")
      if (!nzchar(reason)) {
        showNotification("Reason is required.", type = "warning"); return()
      }
      result <- tryCatch(
        if (target == "approve") {
          approve_run(sr$path, Sys.info()[["user"]] %||% "unknown", reason)
        } else {
          reject_run(sr$path, Sys.info()[["user"]] %||% "unknown", reason)
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(sprintf("Run %s: %s", sr$run_id, target),
                          type = "message")
        updateTextAreaInput(session, "decision_reason", value = "")
        refresh(refresh() + 1)
        # Notify Runs page (status pill) that this run's status has
        # changed.
        if (!is.null(session$userData$runs_changed)) {
          session$userData$runs_changed(
            session$userData$runs_changed() + 1)
        }
      }
    }
    observeEvent(input$do_approve, .decide("approve"))
    observeEvent(input$do_reject,  .decide("reject"))

    # ---- History block ------------------------------------------------
    # All approved + rejected runs, with the four-way audit trail
    # (requester / requested_at / decider / decided_at + comments).
    # Click a history row to drill into that run's details — same tabs
    # as the pending detail block (validation, overrides, outputs).
    output$history_block <- renderUI({
      tagList(
        div(style = "margin-bottom: 0.75em;",
          actionButton(ns("refresh_history"), "Refresh",
                        icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary")
        ),
        p(class = "small-muted",
          "All runs that have been approved or rejected. Newest decision first."),
        DT::DTOutput(ns("history_table")),
        uiOutput(ns("history_detail"))
      )
    })
    observeEvent(input$refresh_history, refresh(refresh() + 1))

    output$history_table <- DT::renderDT({
      h <- history_tbl()
      if (is.null(h) || nrow(h) == 0) {
        return(DT::datatable(
          data.frame(message = "No approved or rejected runs yet."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      decision_pill <- ifelse(h$status == "approved",
                                '<span class="pill pill-pass">APPROVED</span>',
                                '<span class="pill pill-warn">REJECTED</span>')
      # Truncate comments for the overview row; full text is shown on drill-in
      .trunc <- function(s, n = 60) {
        s <- as.character(s); s[is.na(s)] <- ""
        ifelse(nchar(s) > n, paste0(substr(s, 1, n), "…"), s)
      }
      display <- data.frame(
        decision         = decision_pill,
        run_id           = h$run_id,
        decided_at       = h$decided_at,
        decided_by       = h$decided_by,
        decision_comment = .trunc(h$decision_comment),
        requested_by     = h$requested_by,
        requested_at     = h$requested_at,
        request_comment  = .trunc(h$request_comment),
        snapshot         = ifelse(is.na(h$snapshot_label),
                                    "(live config)", h$snapshot_label),
        n_overrides      = h$n_overrides_total,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display, selection = "single", rownames = FALSE,
        escape = FALSE, filter = "top",
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    history_selected <- reactive({
      idx <- input$history_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      h <- history_tbl()
      if (is.null(h) || idx > nrow(h)) return(NULL)
      h[idx, , drop = FALSE]
    })

    output$history_detail <- renderUI({
      hr_row <- history_selected()
      if (is.null(hr_row)) {
        return(div(class = "small-muted", style = "margin-top: 2em;",
                    "Select a row to see the full request/decision audit ",
                    "trail and the run's outputs."))
      }
      tagList(
        hr(),
        h4(sprintf("Run %s — %s", hr_row$run_id, toupper(hr_row$status))),
        p(class = "small-muted", sprintf("path: %s", hr_row$path)),
        card(
          card_header("Audit trail"),
          tags$dl(class = "row",
            tags$dt(class = "col-sm-3", "Requested by"),
              tags$dd(class = "col-sm-9", hr_row$requested_by),
            tags$dt(class = "col-sm-3", "Requested at"),
              tags$dd(class = "col-sm-9", hr_row$requested_at),
            tags$dt(class = "col-sm-3", "Request comment"),
              tags$dd(class = "col-sm-9",
                       if (is.na(hr_row$request_comment) ||
                           !nzchar(hr_row$request_comment))
                         tags$em(class = "small-muted", "(none)")
                       else tags$pre(style = "white-space: pre-wrap;",
                                       hr_row$request_comment)),
            tags$dt(class = "col-sm-3", "Decided by"),
              tags$dd(class = "col-sm-9", hr_row$decided_by),
            tags$dt(class = "col-sm-3", "Decided at"),
              tags$dd(class = "col-sm-9", hr_row$decided_at),
            tags$dt(class = "col-sm-3", "Decision comment"),
              tags$dd(class = "col-sm-9",
                       tags$pre(style = "white-space: pre-wrap;",
                                  hr_row$decision_comment %||% ""))
          )
        ),
        navset_card_tab(
          nav_panel("Validation",        uiOutput(ns("hist_val_panel"))),
          nav_panel("Overrides applied", uiOutput(ns("hist_ov_panel"))),
          nav_panel("Outputs",           uiOutput(ns("hist_out_panel")))
        )
      )
    })

    # The history detail tabs reuse the logic of the pending detail
    # tabs, parameterised on the history_selected() reactive. Each tab's
    # outer renderUI returns the placeholder + supporting text; the
    # actual DataTable register their own server-side render below.
    output$hist_val_panel <- renderUI({
      sr <- history_selected()
      if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path)
      if (is.null(v) || nrow(v) == 0) {
        return(p(class = "small-muted", "No validation report."))
      }
      n_pass <- sum(as.logical(v$passed))
      n_fail <- sum(!as.logical(v$passed))
      tagList(
        p(sprintf("%d/%d passed (%d failures)", n_pass, nrow(v), n_fail)),
        DT::DTOutput(ns("hist_val_table"))
      )
    })

    output$hist_val_table <- DT::renderDT({
      sr <- history_selected(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path); if (is.null(v)) return(NULL)
      v$passed <- as.logical(v$passed)
      v$status <- ifelse(v$passed,
                          '<span class="pill pill-pass">PASS</span>',
                          sprintf('<span class="pill pill-%s">%s</span>',
                                  tolower(v$severity %||% "info"),
                                  toupper(v$severity %||% "INFO")))
      DT::datatable(
        data.frame(status=v$status, stage=v$stage, id=v$id,
                    description=v$description, message=v$message,
                    stringsAsFactors=FALSE),
        rownames = FALSE, escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15, dom = "tip")
      )
    })

    output$hist_ov_panel <- renderUI({
      sr <- history_selected(); if (is.null(sr)) return(NULL)
      ov_dir <- file.path(sr$path, "overrides")
      if (!dir.exists(ov_dir)) {
        return(p(class = "small-muted", "No overrides directory."))
      }
      files <- list.files(ov_dir, pattern = "\\.csv$", full.names = TRUE)
      if (length(files) == 0) {
        return(p(class = "small-muted", "(no override files)"))
      }
      sections <- lapply(files, function(f) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE,
                                          check.names = FALSE),
                        error = function(e) NULL)
        if (is.null(df)) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(could not read)")))
        }
        if (nrow(df) == 0) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(none applied)")))
        }
        rows <- lapply(seq_len(nrow(df)), function(i) {
          tags$tr(lapply(df[i, , drop = TRUE],
                         function(v) tags$td(as.character(v))))
        })
        tagList(
          h6(basename(f)),
          tags$table(class = "table table-sm narrow-table",
            tags$thead(tags$tr(lapply(colnames(df), tags$th))),
            tags$tbody(rows))
        )
      })
      do.call(tagList, sections)
    })

    output$hist_out_panel <- renderUI({
      sr <- history_selected(); if (is.null(sr)) return(NULL)
      paths <- list_run_outputs(sr$path)
      if (length(paths) == 0) {
        return(p(class = "small-muted", "No outputs found."))
      }
      sizes <- file.info(paths)$size
      rows <- lapply(seq_along(paths), function(i) {
        tags$tr(tags$td(basename(paths[i])),
                 tags$td(sprintf("%.1f KB", sizes[i] / 1024)))
      })
      tags$table(class = "table table-sm narrow-table",
        tags$thead(tags$tr(tags$th("File"), tags$th("Size"))),
        tags$tbody(rows))
    })

    # ============== SNAPSHOT APPROVALS =================================
    # Snapshots have their own draft → pending → approved → archived
    # lifecycle managed by promote_snapshot(). The Approval queue
    # surfaces the pending and decided snapshots here.

    snap_pending_tbl <- reactive({
      refresh()
      tryCatch(list_snapshots_pending(),
                error = function(e) {
                  showNotification(paste("Failed to list pending snapshots:",
                                          conditionMessage(e)),
                                    type = "error")
                  NULL
                })
    })
    snap_history_tbl <- reactive({
      refresh()
      tryCatch(list_snapshots_decided(),
                error = function(e) NULL)
    })

    output$pending_snap_block <- renderUI({
      tagList(
        div(style = "margin-bottom: 0.75em;",
          actionButton(ns("refresh_snap_pending"), "Refresh",
                        icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary")
        ),
        p(class = "small-muted",
          "Snapshots that have been submitted (status: pending) and are ",
          "awaiting reviewer approval. Approving moves the snapshot to ",
          tags$em("approved"), "; rejecting sends it back to ",
          tags$em("draft"), " for the creator to fix."),
        DT::DTOutput(ns("snap_pending_table")),
        uiOutput(ns("snap_pending_detail"))
      )
    })
    observeEvent(input$refresh_snap_pending, refresh(refresh() + 1))

    output$snap_pending_table <- DT::renderDT({
      s <- snap_pending_tbl()
      if (is.null(s) || nrow(s) == 0) {
        return(DT::datatable(
          data.frame(message = "No snapshots are awaiting approval."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      display <- data.frame(
        label       = s$label,
        created_by  = s$created_by,
        created_at  = s$created_at,
        parent      = ifelse(is.na(s$parent), "—", s$parent),
        description = s$description,
        code_sha    = substr(s$code_sha_at_creation, 1, 8),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display, selection = "single", rownames = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    snap_pending_selected <- reactive({
      idx <- input$snap_pending_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      s <- snap_pending_tbl()
      if (is.null(s) || idx > nrow(s)) return(NULL)
      s[idx, , drop = FALSE]
    })

    output$snap_pending_detail <- renderUI({
      sel <- snap_pending_selected()
      if (is.null(sel)) {
        return(div(class = "small-muted", style = "margin-top: 2em;",
                    "Select a pending snapshot to review and decide."))
      }
      tagList(
        hr(),
        h4(sprintf("Snapshot %s", sel$label)),
        card(
          card_header("Submission details"),
          tags$dl(class = "row",
            tags$dt(class = "col-sm-3", "Created by"),
              tags$dd(class = "col-sm-9", sel$created_by),
            tags$dt(class = "col-sm-3", "Created at"),
              tags$dd(class = "col-sm-9", sel$created_at),
            tags$dt(class = "col-sm-3", "Description"),
              tags$dd(class = "col-sm-9", sel$description),
            tags$dt(class = "col-sm-3", "Parent snapshot"),
              tags$dd(class = "col-sm-9",
                       if (is.na(sel$parent)) "(none — first in lineage)" else sel$parent),
            tags$dt(class = "col-sm-3", "Code SHA at creation"),
              tags$dd(class = "col-sm-9", tags$code(sel$code_sha_at_creation %||% "—"))
          )
        ),
        p(class = "small-muted",
          "To inspect the snapshot's content, open the ",
          tags$strong("Snapshots"), " tab and pick this snapshot — the ",
          "Files panel there shows every config file the snapshot froze."),
        card(
          card_header("Approve / Reject"),
          textAreaInput(ns("snap_decision_reason"), "Reason (required)",
                         rows = 3,
                         placeholder = "Document the basis for approval or rejection."),
          actionButton(ns("snap_do_approve"), "Approve",
                        icon = icon("check"), class = "btn-success"),
          tags$span(style = "margin-left: 0.5em;",
            actionButton(ns("snap_do_reject"), "Reject (return to draft)",
                          icon = icon("xmark"), class = "btn-warning"))
        )
      )
    })

    .snap_decide <- function(target) {
      sel <- snap_pending_selected(); if (is.null(sel)) return()
      reason <- trimws(input$snap_decision_reason %||% "")
      if (!nzchar(reason)) {
        showNotification("Reason is required.", type = "warning"); return()
      }
      # promote_snapshot enforces the new state machine:
      #   approve -> approved       (separation of duties applies)
      #   reject  -> rejected       (terminal; clone-to-draft to fix)
      next_status <- if (target == "approve") "approved" else "rejected"
      result <- tryCatch(
        promote_snapshot(
          label = sel$label,
          status = next_status,
          approved_by = Sys.info()[["user"]] %||% "unknown",
          reason = reason,
          snapshots_root = getOption("ifrs9.snapshots_dir",
                                       file.path(getOption("ifrs9.project_root", getwd()),
                                                  "config_snapshots"))
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(sprintf("Snapshot %s: %s", sel$label, target),
                          type = "message")
        updateTextAreaInput(session, "snap_decision_reason", value = "")
        refresh(refresh() + 1)
        # Notify the Run pipeline dropdown so it picks up the new
        # status (esp. important after Approve — the user expects to
        # be able to start an Official run immediately after).
        if (!is.null(session$userData$snapshots_changed)) {
          session$userData$snapshots_changed(
            session$userData$snapshots_changed() + 1)
        }
      }
    }
    observeEvent(input$snap_do_approve, .snap_decide("approve"))
    observeEvent(input$snap_do_reject,  .snap_decide("reject"))

    # ---- Snapshot history block ---------------------------------------
    output$history_snap_block <- renderUI({
      tagList(
        div(style = "margin-bottom: 0.75em;",
          actionButton(ns("refresh_snap_history"), "Refresh",
                        icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary")
        ),
        p(class = "small-muted",
          "All snapshots that have been approved or archived. Newest first."),
        DT::DTOutput(ns("snap_history_table")),
        uiOutput(ns("snap_history_detail"))
      )
    })
    observeEvent(input$refresh_snap_history, refresh(refresh() + 1))

    output$snap_history_table <- DT::renderDT({
      s <- snap_history_tbl()
      if (is.null(s) || nrow(s) == 0) {
        return(DT::datatable(
          data.frame(message = "No approved or archived snapshots yet."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      status_pill <- ifelse(
        s$status == "approved",
        '<span class="pill pill-pass">APPROVED</span>',
        '<span class="pill pill-archived">ARCHIVED</span>')
      .trunc <- function(x, n = 60) {
        x <- as.character(x); x[is.na(x)] <- ""
        ifelse(nchar(x) > n, paste0(substr(x, 1, n), "…"), x)
      }
      display <- data.frame(
        status           = status_pill,
        label            = s$label,
        decided_at       = s$decided_at,
        decided_by       = s$decided_by,
        decision_comment = .trunc(s$decision_comment),
        requested_by     = s$requested_by,
        requested_at     = s$requested_at,
        description      = .trunc(s$description),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display, selection = "single", rownames = FALSE,
        escape = FALSE, filter = "top",
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    snap_history_selected <- reactive({
      idx <- input$snap_history_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      s <- snap_history_tbl()
      if (is.null(s) || idx > nrow(s)) return(NULL)
      s[idx, , drop = FALSE]
    })

    output$snap_history_detail <- renderUI({
      sr <- snap_history_selected()
      if (is.null(sr)) {
        return(div(class = "small-muted", style = "margin-top: 2em;",
                    "Select a snapshot to see the full submission/decision audit trail."))
      }
      tagList(
        hr(),
        h4(sprintf("Snapshot %s — %s", sr$label, toupper(sr$status))),
        card(
          card_header("Audit trail"),
          tags$dl(class = "row",
            tags$dt(class = "col-sm-3", "Created by"),
              tags$dd(class = "col-sm-9", sr$requested_by),
            tags$dt(class = "col-sm-3", "Created at"),
              tags$dd(class = "col-sm-9", sr$requested_at),
            tags$dt(class = "col-sm-3", "Description"),
              tags$dd(class = "col-sm-9",
                       if (is.na(sr$description) || !nzchar(sr$description))
                         tags$em(class = "small-muted", "(none)")
                       else tags$pre(style = "white-space: pre-wrap;",
                                       sr$description)),
            tags$dt(class = "col-sm-3", "Decided by"),
              tags$dd(class = "col-sm-9",
                       if (is.na(sr$decided_by)) tags$em(class = "small-muted",
                                                          "(no decider recorded)")
                       else sr$decided_by),
            tags$dt(class = "col-sm-3", "Decided at"),
              tags$dd(class = "col-sm-9", sr$decided_at %||% "—"),
            tags$dt(class = "col-sm-3", "Decision comment"),
              tags$dd(class = "col-sm-9",
                       tags$pre(style = "white-space: pre-wrap;",
                                  sr$decision_comment %||% ""))
          )
        )
      )
    })
  })
}
