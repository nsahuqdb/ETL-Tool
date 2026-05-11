# =============================================================================
# app/modules/mod_runs.R
#
# The Runs page: a table of all past runs newest-first, with a detail
# panel that renders manifest, validation, reconciliation, and an output
# browser for a selected run.
#
# Read-only. Reads:
#   list_runs(runs_dir_default())            from R/run_discovery.R
#   read_run_manifest(path)
#   read_run_validation(path)
#   read_run_reconciliation(path)
#   list_run_outputs(path)
# =============================================================================

mod_runs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Pipeline runs"),
        p(class = "small-muted",
          textOutput(ns("runs_dir_label"), inline = TRUE)),
        actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                      class = "btn-sm btn-outline-secondary")
      )
    ),
    fluidRow(
      column(12,
        DT::DTOutput(ns("runs_table"))
      )
    ),
    # Detail block — populated when a row is selected
    uiOutput(ns("detail_block"))
  )
}


mod_runs_server <- function(id, on_select_run = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    observeEvent(input$refresh, refresh_trigger(refresh_trigger() + 1))

    # Safety-net: poll the runs directory every 2 seconds and trigger a
    # refresh when any run_status.yml has been modified. This catches
    # the case where mod_approval_queue's cross-module signal is missed
    # by Shiny's invalidation graph (e.g. tab not yet rendered when the
    # signal fires). Cheap to compute — file.info() on the 18-or-so
    # run_status.yml files takes well under a millisecond.
    status_mtimes <- reactivePoll(
      intervalMillis = 2000,
      session = session,
      checkFunc = function() {
        d <- runs_dir_default()
        if (!dir.exists(d)) return("")
        paths <- list.files(d, pattern = "^run_status\\.yml$",
                             recursive = TRUE, full.names = TRUE)
        if (length(paths) == 0) return("")
        # Hash of (path, mtime) pairs — changes whenever any status file
        # changes or is added/removed.
        info <- file.info(paths)
        paste(paths, info$mtime, collapse = "|")
      },
      valueFunc = function() Sys.time()
    )

    runs_tbl <- reactive({
      refresh_trigger()
      # Cross-module signal: bump from mod_run_trigger when phase 2
      # finishes, and from mod_approval_queue when a run is approved
      # or rejected (so the status pill re-renders here).
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed()
      }
      # Safety net: also depend on the directory poll.
      status_mtimes()
      r <- tryCatch(list_runs(), error = function(e) {
        showNotification(paste("Failed to list runs:", conditionMessage(e)),
                          type = "error")
        list_runs.empty()
      })
      # Annotate each row with its approval status (pending_approval /
      # approved / rejected / unknown). Pre-H12 runs and runs whose
      # phase 2 errored before writing run_status.yml come back as
      # "unknown" so they're still visible.
      tryCatch(annotate_runs_with_status(r), error = function(e) {
        r$status <- rep("unknown", nrow(r))
        r
      })
    })

    output$runs_dir_label <- renderText({
      d <- runs_dir_default()
      d_norm <- normalizePath(d, mustWork = FALSE)
      exists <- dir.exists(d)
      n <- nrow(runs_tbl())
      sprintf("runs directory: %s   |   exists: %s   |   %d runs found",
              d_norm,
              if (exists) "yes" else "NO — run with keep_history=TRUE first",
              n)
    })

    output$runs_table <- DT::renderDT({
      r <- runs_tbl()
      if (nrow(r) == 0) {
        return(DT::datatable(
          data.frame(message = "No runs found. Run the pipeline with keep_history=TRUE."),
          options = list(dom = "t"), rownames = FALSE))
      }
      # Status pill: visual scan of which runs need review vs are signed off.
      status_pill <- function(s) {
        s <- as.character(s); s[is.na(s)] <- "unknown"
        # pending_checker (new) and pending_approval (legacy) both
        # display as "PENDING CHECKER" so historical and new runs
        # share a label.
        label <- ifelse(s %in% c("pending_checker", "pending_approval"),
                         "PENDING CHECKER",
                  ifelse(s == "unofficial", "UNOFFICIAL",
                         toupper(s)))
        cls <- ifelse(s == "approved",                              "pill-approved",
               ifelse(s %in% c("pending_checker", "pending_approval"), "pill-pending",
               ifelse(s == "unofficial",                            "pill-info",
               ifelse(s == "rejected",                              "pill-warn",
                                                                    "pill-suppr"))))
        sprintf('<span class="pill %s">%s</span>', cls, label)
      }
      display <- data.frame(
        status     = vapply(r$status %||% rep(NA_character_, nrow(r)),
                              status_pill, character(1)),
        run_id     = r$run_id,
        started    = r$started_at,
        duration_s = round(r$duration_seconds, 1),
        user       = r$user,
        snapshot   = ifelse(is.na(r$snapshot_label),
                              "(live config)",
                              sprintf("%s [%s]",
                                      r$snapshot_label, r$snapshot_status)),
        outputs    = r$n_outputs,
        val_fail   = r$n_validation_failures,
        recon      = ifelse(r$has_reconciliation, "yes", "no"),
        code       = substr(r$code_sha, 1, 8),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        class = "narrow-table compact",
        options = list(
          pageLength = 15,
          order = list(list(2, "desc")),  # started desc (column index +1 since status added)
          columnDefs = list(
            list(targets = "_all", className = "dt-left")
          )
        )
      )
    })

    # ---- Detail block --------------------------------------------------
    selected_idx <- reactive({
      idx <- input$runs_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      idx
    })
    selected_run <- reactive({
      idx <- selected_idx()
      if (is.null(idx)) return(NULL)
      r <- runs_tbl()
      if (idx > nrow(r)) return(NULL)
      r[idx, , drop = FALSE]
    })

    output$detail_block <- renderUI({
      sr <- selected_run()
      if (is.null(sr)) {
        return(div(class = "small-muted",
                    style = "margin-top: 2em;",
                    "Select a row above to see manifest, validation findings, ",
                    "reconciliation, and outputs for that run."))
      }
      tagList(
        hr(),
        # Header: which run + path
        h4(sprintf("Run %s", sr$run_id)),
        p(class = "small-muted",
          sprintf("path: %s", sr$path)),
        # Export card — always visible. Renders either the download
        # button (when approved) or an explanation (when not).
        # Wrapped in its own card so it's clearly distinct from the
        # detail tabs below; nothing about its render path can
        # interfere with the sub-tabs since they are independent
        # outputs at the same nesting level.
        card(
          card_header("Export"),
          uiOutput(ns("export_block"))
        ),
        # Detail tabs — always visible. Each tab's content is rendered
        # by its own dedicated reactive output, all of which depend
        # only on selected_run() and have no coupling to approval
        # status.
        navset_card_tab(
          # Each DT::DTOutput is kept as a STATIC child of its
          # nav_panel — NOT wrapped inside the corresponding *_panel
          # renderUI. When detail_block re-renders (e.g. after
          # run_status.yml changes from approval), nested DTOutputs
          # get fresh DOM placeholders, and Shiny does NOT reliably
          # re-fire the corresponding DT::renderDT. Keeping the
          # placeholder static means the table renderer's reactive
          # remains bound to the SAME placeholder across re-renders.
          # The summary-text portion (counts, file-not-found
          # messages, etc.) stays in the *_panel renderUI above the
          # table.
          nav_panel("Manifest",
            uiOutput(ns("manifest_panel")),
            DT::DTOutput(ns("manifest_inputs_table"))),
          nav_panel("Validation",
            uiOutput(ns("validation_panel")),
            DT::DTOutput(ns("validation_table"))),
          nav_panel("Overrides", uiOutput(ns("overrides_panel"))),
          nav_panel("Reconciliation",
            uiOutput(ns("reconciliation_panel")),
            DT::DTOutput(ns("mismatches_table"))),
          nav_panel("Outputs",
            uiOutput(ns("outputs_panel")),
            DT::DTOutput(ns("output_preview")))
        )
      )
    })

    # ---- Run export download handler ---------------------------------
    # Builds the zip via build_run_export() and streams it to the
    # browser. The zip is built on each click — we don't cache, so
    # the user always gets the freshest snapshot of the run's state
    # (e.g. if the run was approved between clicks, the new
    # run_status.yml is in the bundle).
    # ---- Export block: gated on approval ---------------------------
    # The selected run's status drives whether the download is offered.
    # Approval is recorded in runs/<id>/reports/run_status.yml; here
    # we read it via the helper from R/run_approval.R.
    .selected_run_status <- reactive({
      sr <- selected_run(); if (is.null(sr)) return(NA_character_)
      s <- tryCatch(read_run_status(sr$path), error = function(e) NULL)
      if (is.null(s)) return("unknown")
      s$status %||% "unknown"
    })

    output$export_block <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      status <- .selected_run_status()
      # Approved runs and unofficial runs are both exportable. Approved
      # runs are sanctioned deliverables. Unofficial runs are explicitly
      # marked UNOFFICIAL in the export bundle so consumers can't mistake
      # them for sanctioned outputs.
      can_export <- isTRUE(status == "approved") ||
                     isTRUE(status == "unofficial")
      if (can_export) {
        notice <- if (status == "unofficial") {
          div(class = "alert alert-info",
              style = "margin-bottom: 0.5em;",
              tags$strong("Unofficial run."),
              " The export bundle is clearly marked UNOFFICIAL so it ",
              "cannot be confused with a sanctioned deliverable.")
        } else NULL
        return(tagList(
          notice,
          div(style = "margin-bottom: 0.75em;",
            checkboxInput(ns("export_include_inputs"),
                           "Include input files (~30-80 MB extra)",
                           value = TRUE, width = "350px"),
            downloadButton(ns("export_run"),
                            "Export run package (zip)",
                            class = "btn-outline-primary btn-sm",
                            icon = icon("file-zipper"))
          )
        ))
      }
      # Not exportable -> show a status-aware explanation.
      pending_msg <- paste(
          "This run is awaiting checker approval. Export becomes",
          "available after a checker approves the run on the Approval",
          "queue tab.")
      explainer <- switch(
        as.character(status),
        "pending_checker"  = pending_msg,
        "pending_approval" = pending_msg,
        "rejected"         = paste(
          "This run was rejected. Rejected runs are not exportable.",
          "If the issue has been fixed and re-run, export the new run instead."),
        "unknown"          = paste(
          "This run has no recorded approval status (run_status.yml is",
          "missing or unparseable). Export is disabled until the approval",
          "trail is in place."),
        paste(
          "Export is disabled in status '", status, "'.", sep = ""))
      div(class = "alert alert-secondary", style = "margin-bottom: 0.75em;",
          tags$strong("Export not available"),
          tags$p(style = "margin-bottom: 0; margin-top: 0.4em;", explainer))
    })

    # ---- Run export download handler ---------------------------------
    # Defense in depth: even though the button is hidden for non-
    # approved runs, the handler also refuses to build a bundle for
    # them. This guards against the download URL being hit directly
    # (e.g. from a bookmark) on a run whose status changed since the
    # bookmark was made.
    output$export_run <- downloadHandler(
      filename = function() {
        sr <- selected_run()
        if (is.null(sr)) return("ifrs9_run_export.zip")
        sprintf("ifrs9_run_%s.zip", sr$run_id)
      },
      content = function(file) {
        sr <- selected_run()
        if (is.null(sr)) {
          writeLines("(no run selected)", file); return()
        }
        status <- .selected_run_status()
        # Defense in depth: handler refuses unless the run is approved
        # OR unofficial. Same rule as the export_block UI gate.
        if (!isTRUE(status %in% c("approved", "unofficial"))) {
          writeLines(c(
            "(this run is not exportable; export refused)",
            sprintf("run_id: %s", sr$run_id),
            sprintf("status: %s", status)
          ), file)
          showNotification(
            sprintf("Export refused: run is in status '%s' (not approved or unofficial).",
                     status),
            type = "warning", duration = 8)
          return()
        }
        withProgress(message = "Building run package", value = 0.3, {
          res <- tryCatch(
            build_run_export(
              run_path = sr$path,
              dest_zip = file,
              include_inputs = isTRUE(input$export_include_inputs)
            ),
            error = function(e) list(ok = FALSE,
                                       message = conditionMessage(e))
          )
          setProgress(1)
        })
        if (!isTRUE(res$ok)) {
          showNotification(paste("Export failed:", res$message),
                            type = "error", duration = 12)
        } else {
          showNotification(sprintf("Exported %s",
                                     basename(res$path %||% file)),
                            type = "message", duration = 4)
        }
      },
      contentType = "application/zip"
    )

    # ---- Manifest panel -----------------------------------------------
    output$manifest_panel <- renderUI({
      sr <- selected_run()
      if (is.null(sr)) return(NULL)
      m <- read_run_manifest(sr$path)
      if (is.null(m)) return(p("No manifest.json found."))

      run <- m$run
      snap <- m$snapshot
      tagList(
        h5("Run"),
        tags$dl(class = "row",
          tags$dt(class = "col-sm-3", "run_id"),       tags$dd(class = "col-sm-9", run$run_id %||% "—"),
          tags$dt(class = "col-sm-3", "started_at"),   tags$dd(class = "col-sm-9", run$started_at %||% "—"),
          tags$dt(class = "col-sm-3", "finished_at"),  tags$dd(class = "col-sm-9", run$finished_at %||% "—"),
          tags$dt(class = "col-sm-3", "duration"),     tags$dd(class = "col-sm-9", sprintf("%.1f s", as.numeric(run$duration_seconds %||% NA))),
          tags$dt(class = "col-sm-3", "user"),         tags$dd(class = "col-sm-9", run$user %||% "—"),
          tags$dt(class = "col-sm-3", "hostname"),     tags$dd(class = "col-sm-9", run$hostname %||% "—"),
          tags$dt(class = "col-sm-3", "code_sha"),     tags$dd(class = "col-sm-9", tags$code(run$code_sha %||% "—"))
        ),
        if (!is.null(snap)) {
          tagList(
            h5("Snapshot"),
            tags$dl(class = "row",
              tags$dt(class = "col-sm-3", "label"),      tags$dd(class = "col-sm-9", snap$label),
              tags$dt(class = "col-sm-3", "status"),     tags$dd(class = "col-sm-9",
                tags$span(class = sprintf("pill pill-%s", snap$status %||% "draft"), snap$status %||% "?")
              ),
              tags$dt(class = "col-sm-3", "code_sha at creation"),
                tags$dd(class = "col-sm-9", tags$code(snap$code_sha_at_creation %||% "—"))
            )
          )
        } else {
          p(class = "small-muted", "No snapshot — run used live config.")
        },
        h5("Inputs"),
        if (!is.null(m$inputs) && length(m$inputs) > 0) {
          # DT::DTOutput is in the static navset layout. Don't render
          # a duplicate placeholder here — Shiny would have two
          # competing target divs. Just emit a small label so the
          # section header has context if inputs exist.
          tags$p(class = "small-muted",
                  sprintf("%d input file(s) recorded — see table below.",
                          length(m$inputs)))
        } else {
          p(class = "small-muted", "(none recorded)")
        }
      )
    })

    output$manifest_inputs_table <- DT::renderDT({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      m <- read_run_manifest(sr$path); if (is.null(m)) return(NULL)
      i <- m$inputs
      if (is.null(i) || length(i) == 0) return(NULL)
      df <- if (is.data.frame(i)) i else as.data.frame(do.call(rbind, lapply(i, as.data.frame)))
      DT::datatable(df, options = list(pageLength = 10, dom = "tip"),
                     rownames = FALSE, class = "narrow-table compact")
    })

    # ---- Validation panel ---------------------------------------------
    output$validation_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path)
      if (is.null(v) || nrow(v) == 0) {
        return(p(class = "small-muted", "No validation report for this run."))
      }
      n_pass <- sum(as.logical(v$passed))
      n_fail <- sum(!as.logical(v$passed))
      # DT::DTOutput is in the static navset layout (see comment in
      # detail_block). Just emit the summary line here.
      p(sprintf("Total: %d  |  Pass: %d  |  Fail: %d", nrow(v), n_pass, n_fail))
    })

    output$validation_table <- DT::renderDT({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path); if (is.null(v)) return(NULL)

      sev_pill <- function(sev, suppressed) {
        cls <- tolower(as.character(sev))
        cls <- ifelse(cls %in% c("error","warn","info"), cls, "pass")
        ifelse(as.logical(suppressed) %in% TRUE,
                sprintf('<span class="pill pill-suppr">SUPPR</span>'),
                sprintf('<span class="pill pill-%s">%s</span>', cls, toupper(sev)))
      }

      v$passed <- as.logical(v$passed)
      v$suppressed <- as.logical(v$suppressed %||% FALSE)
      eff <- v$effective_severity %||% v$severity
      v$status <- ifelse(v$passed, '<span class="pill pill-pass">PASS</span>',
                          sev_pill(eff, v$suppressed))

      display <- data.frame(
        status      = v$status,
        stage       = v$stage,
        id          = v$id,
        context     = v$context,
        description = v$description,
        message     = v$message,
        rationale   = v$rationale,
        remediation = v$remediation,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        class = "narrow-table compact",
        options = list(
          pageLength = 25,
          order = list(list(0, "desc")),
          columnDefs = list(
            list(visible = FALSE, targets = c(6, 7))  # rationale + remediation hidden by default
          )
        )
      ) |>
        DT::formatStyle(columns = "status", target = "row")
    })

    # ---- Reconciliation panel -----------------------------------------
    output$reconciliation_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      r <- read_run_reconciliation(sr$path)
      if (is.null(r$md_path)) {
        return(p(class = "small-muted",
                  "No reconciliation for this run. Configure ",
                  tags$code("paths$reference_outputs"),
                  " in config.yml to enable."))
      }
      tagList(
        h5("Reconciliation summary"),
        # Render the markdown directly
        HTML(markdown::markdownToHTML(r$md_path, fragment.only = TRUE)),
        h5("Mismatches"),
        if (nrow(r$mismatches) == 0) {
          p(class = "small-muted", "(no mismatch CSVs)")
        } else {
          # DT::DTOutput is in the static navset layout. Just emit a
          # small label here.
          tags$p(class = "small-muted",
                  sprintf("%d mismatch file(s) — see table below.",
                          nrow(r$mismatches)))
        }
      )
    })

    output$mismatches_table <- DT::renderDT({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      r <- read_run_reconciliation(sr$path); if (is.null(r)) return(NULL)
      DT::datatable(
        r$mismatches[, c("file", "size_bytes")],
        rownames = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 10, dom = "tip")
      )
    })

    # ---- Overrides panel ----------------------------------------------
    # Shows the per-run override CSVs written by phase 2. Three files
    # (rating, stage, restructuring) — empty files just show "(none
    # applied)" so the run is always self-describing.
    output$overrides_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      ov_dir <- file.path(sr$path, "overrides")
      if (!dir.exists(ov_dir)) {
        return(p(class = "small-muted",
                  "No overrides directory for this run. Pre-H12 runs ",
                  "do not record overrides at the run level."))
      }
      files <- sort(list.files(ov_dir, pattern = "\\.csv$",
                                  full.names = TRUE))
      if (length(files) == 0) {
        return(p(class = "small-muted", "(no override files)"))
      }

      sections <- lapply(files, function(f) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE,
                                          check.names = FALSE),
                        error = function(e) NULL)
        if (is.null(df)) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(could not read file)")))
        }
        if (nrow(df) == 0) {
          return(tagList(h6(basename(f)),
                          p(class = "small-muted", "(none applied)")))
        }
        # Plain HTML table (cannot use renderDT inside renderUI; this also
        # mirrors how the Approval queue page renders the same data).
        rows <- lapply(seq_len(nrow(df)), function(i) {
          tags$tr(lapply(df[i, , drop = TRUE],
                         function(v) tags$td(as.character(v))))
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

    # ---- Outputs panel ------------------------------------------------
    # Note: DT::DTOutput("output_preview") is in the STATIC navset
    # layout, not in this renderUI. See the comment in detail_block's
    # navset_card_tab for why.
    output$outputs_panel <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      paths <- list_run_outputs(sr$path)
      if (length(paths) == 0) {
        return(p(class = "small-muted", "No output CSVs found."))
      }
      tagList(
        selectInput(ns("output_pick"), "File",
                     choices = basename(paths),
                     width = "350px"),
        uiOutput(ns("output_meta"))
      )
    })

    # Resolve the user-picked filename to a full path. Returns NULL when
    # nothing is picked yet OR the picked file isn't in this run.
    .picked_output_path <- reactive({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      pick <- input$output_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      paths <- list_run_outputs(sr$path)
      hit <- paths[basename(paths) == pick]
      if (length(hit) == 0) return(NULL)
      hit[1]
    })

    output$output_meta <- renderUI({
      full <- .picked_output_path()
      if (is.null(full)) {
        return(tags$p(class = "small-muted",
                       "(pick a file from the dropdown)"))
      }
      if (!file.exists(full)) {
        return(tags$p(class = "small-muted",
                       sprintf("(file not found on disk: %s)", full)))
      }
      info <- file.info(full)
      n_lines <- length(readLines(full, n = 5001L, warn = FALSE))
      hint <- if (n_lines > 5000) " (5000+; first 2000 shown)" else ""
      tags$p(class = "small-muted",
              sprintf("path: %s   |   size: %.1f KB   |   lines: %d%s",
                      full, info$size / 1024, n_lines, hint))
    })

    output$output_preview <- DT::renderDT({
      full <- .picked_output_path()
      if (is.null(full)) return(NULL)
      if (!file.exists(full)) {
        showNotification(sprintf("Output file disappeared from disk: %s",
                                   full),
                          type = "error", duration = 8)
        return(NULL)
      }

      # Use base read.csv — bullet-proof against the CRLF + "NA"/"NaN"
      # scrubbed values our writer produces, where readr::read_csv has
      # tripped on encoding or column-type guesses for some files.
      df <- tryCatch(
        utils::read.csv(full, nrows = 2000,
                          stringsAsFactors = FALSE,
                          check.names = FALSE,
                          na.strings = c("", "NA", "NaN")),
        error = function(e) {
          showNotification(sprintf("Could not read %s: %s",
                                     basename(full), conditionMessage(e)),
                            type = "error", duration = 10)
          NULL
        }
      )
      if (is.null(df)) return(NULL)
      if (nrow(df) == 0) {
        # Empty file is legitimate for a few outputs (e.g. an audit
        # table for a clean run). Render an empty 1-col table that
        # makes that explicit instead of failing silently.
        return(DT::datatable(
          data.frame(`(empty file — 0 rows)` = character(),
                      check.names = FALSE),
          options = list(dom = "t"), rownames = FALSE))
      }

      # IDs are numeric only by accident — they're identifiers, not
      # measurements. Cast any column whose name looks like an
      # identifier to character so DT renders a search box instead of
      # a range slider in the filter row.
      id_pattern <- "(?i)(^id$|_id$|id_|Id$|ID$|^contract|customer$|account)"
      id_cols <- grep(id_pattern, colnames(df), perl = TRUE)
      for (i in id_cols) df[[i]] <- as.character(df[[i]])

      DT::datatable(
        df,
        rownames = FALSE,
        filter = "top",
        class = "narrow-table compact",
        options = list(pageLength = 25, scrollX = TRUE,
                        deferRender = TRUE)
      )
    })
  })
}


# ---- Tiny helper ---------------------------------------------------------
list_runs.empty <- function() {
  tibble::tibble(
    run_id = character(), path = character(),
    started_at = character(), finished_at = character(),
    duration_seconds = numeric(),
    user = character(), code_sha = character(),
    snapshot_label = character(), snapshot_status = character(),
    n_outputs = integer(), n_validation_failures = integer(),
    has_reconciliation = logical()
  )
}
