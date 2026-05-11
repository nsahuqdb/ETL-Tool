# =============================================================================
# app/modules/mod_run_trigger.R
#
# H12: phased run workflow with mid-run pause for overrides.
#
# UX flow:
#   Step 1 — Pre-run check (input-stage validators only)
#   Step 2 — Click "Start run" → run_etl_phase1 runs synchronously
#               (~10s for 6.6k contracts). State held in module reactiveVal.
#   Step 3 — PAUSE PAGE: cm_view shown as filterable DataTable. User adds
#               overrides to a buffer (separate reactive). Three override
#               types: rating, stage, restructuring. Each row has a
#               required reason.
#   Step 4 — Click "Continue" → run_etl_phase2 runs with overrides
#               applied → outputs written → run lands as pending_approval.
#
# State machine (held in reactiveVal phase_state):
#   "idle"          — no run started, show config picker
#   "phase1_done"   — phase 1 finished, show pause page
#   "phase2_done"   — phase 2 finished, show summary
# =============================================================================

mod_run_trigger_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Run pipeline"),
      p(class = "small-muted",
        "Pick a snapshot (or live config), run a fast pre-check, then start ",
        "the run. The pipeline pauses after computing customer-level views ",
        "to let you make overrides. After Continue, the run finishes and ",
        "lands as ", tags$em("pending_approval"), ".")
    )),
    uiOutput(ns("page_body"))
  )
}


mod_run_trigger_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State machine
    phase_state <- reactiveVal("idle")
    phase1_state <- reactiveVal(NULL)
    phase2_result <- reactiveVal(NULL)
    pre_run_results <- reactiveVal(NULL)
    refresh_snapshots <- reactiveVal(0)

    # Pre-run check is computed against a specific (run_type, snapshot)
    # pair: unofficial + live config validates against the live config
    # file; official + an approved snapshot validates against the frozen
    # snapshot. If the user changes EITHER setting after the check has
    # passed, the previous result is stale and could let them bypass the
    # gate (e.g. validate against unofficial-relaxed rules, then switch
    # to official and run). Clear pre_run_results whenever either input
    # changes so the gate forces a fresh check.
    observeEvent(input$run_type, {
      if (!is.null(pre_run_results())) {
        pre_run_results(NULL)
        showNotification(
          "Run type changed — pre-run check cleared. Please run pre-run check again.",
          type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$snapshot_pick, {
      if (!is.null(pre_run_results())) {
        pre_run_results(NULL)
        showNotification(
          "Snapshot selection changed — pre-run check cleared. Please run pre-run check again.",
          type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)

    # ---- Input source state (H16a) -------------------------------------
    # input_validation: NULL until user clicks "Validate inputs". Then a
    #   tibble(check, status, detail) — status PASS or FAIL per check.
    # input_dir_override: the resolved input directory for THIS run if
    #   non-default. NULL when source = "configured".
    # input_source_meta: the {kind, details} dict that gets passed to
    #   run_etl_phase1 and persisted into reports/input_source.yml.
    # The "Pre-run check" button is gated on input_validation being
    #   non-NULL and having zero FAILs (i.e. user has explicitly run
    #   the structural check and it passed).
    input_validation   <- reactiveVal(NULL)
    input_dir_override <- reactiveVal(NULL)
    input_source_meta  <- reactiveVal(list(kind = "configured",
                                            details = list()))

    # Override buffers — three editable tables held in memory
    overrides_rating <- reactiveVal(.empty_override_buf("rating"))
    overrides_stage  <- reactiveVal(.empty_override_buf("stage"))
    overrides_restr  <- reactiveVal(.empty_override_buf("restructuring"))

    snaps_root <- function() {
      proj_root <- getOption("ifrs9.project_root", getwd())
      getOption("ifrs9.snapshots_dir",
                file.path(proj_root, "config_snapshots"))
    }

    # ============== TOP-LEVEL PAGE ROUTER =============================
    output$page_body <- renderUI({
      switch(phase_state(),
        "idle"        = .ui_idle(ns),
        "phase1_done" = .ui_pause(ns),
        "phase2_done" = .ui_summary(ns),
        .ui_idle(ns)
      )
    })

    # ============== STEP 1: IDLE PAGE =================================
    # Snapshot dropdown — rendered reactively. Re-fires whenever:
    #   * the local refresh_snapshots() reactiveVal is bumped (used by
    #     this module after a phase 2 run), OR
    #   * any other module bumps session$userData$snapshots_changed
    #     (e.g. snapshot manager promotes a snapshot, approval queue
    #     approves a snapshot). Without this cross-module signal,
    #     snapshots created or approved during the session don't
    #     appear here until the app is restarted.
    output$snapshot_pick_ui <- renderUI({
      refresh_snapshots()
      if (!is.null(session$userData$snapshots_changed)) {
        session$userData$snapshots_changed()
      }
      s <- tryCatch(list_snapshots(snaps_root()),
                     error = function(e) list_snapshots.empty())
      choices <- c("(live config)" = "__LIVE__")
      if (nrow(s) > 0) {
        # Sort order — approved first (best for official runs), then
        # in-flight states, then drafts, then terminal/legacy. The
        # rank vector covers ALL current statuses so order() doesn't
        # produce NAs that push valid snapshots to the bottom.
        rank <- c(approved      = 0,
                   pending_final = 1,
                   pending       = 1,   # legacy alias
                   tested        = 2,
                   draft         = 3,
                   rejected      = 4,
                   archived      = 5)
        ord  <- order(rank[s$status %||% rep("draft", nrow(s))], s$created_at)
        s <- s[ord, , drop = FALSE]
        labels <- sprintf("%s [%s] — %s",
                           s$label, s$status %||% "?",
                           substr(s$description %||% "", 1, 50))
        choices <- c(choices, setNames(s$label, labels))
      }
      selectInput(ns("snapshot_pick"), "Snapshot",
                   choices = choices,
                   selected = isolate(input$snapshot_pick) %||% "__LIVE__")
    })

    output$snapshot_meta <- renderUI({
      pick <- input$snapshot_pick
      if (is.null(pick) || pick == "__LIVE__") {
        return(p(class = "small-muted",
                  "Live config: reads ", tags$code("config/"), " and ",
                  tags$code("data-raw/static/"), " as they currently sit on disk."))
      }
      meta <- tryCatch(read_snapshot_metadata(pick, snaps_root()),
                        error = function(e) NULL)
      if (is.null(meta)) return(p(class = "small-muted",
                                    "(could not read snapshot metadata)"))
      tags$dl(class = "row",
        tags$dt(class = "col-sm-4", "status"),
          tags$dd(class = "col-sm-8",
            tags$span(class = sprintf("pill pill-%s", meta$status %||% "draft"),
                      meta$status %||% "?")),
        tags$dt(class = "col-sm-4", "description"),
          tags$dd(class = "col-sm-8", meta$description %||% "—")
      )
    })

    # ============== INPUT SOURCE (H16a) =====================================
    # When the source kind changes, reset the validation result. The
    # user has to revalidate after switching source.
    observeEvent(input$input_source_kind, {
      input_validation(NULL)
      input_dir_override(NULL)
      input_source_meta(list(kind = input$input_source_kind, details = list()))
    }, ignoreInit = TRUE)

    drop_root_path <- reactive({
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
      if (is.null(cfg)) return("")
      cfg$paths$data_drop_root %||% ""
    })

    # Renders a different sub-UI based on which input-source radio is
    # selected. For "configured" we just show a hint about the path;
    # for "drop_folder" a dropdown of available drops; for "upload" a
    # fileInput accepting .zip.
    output$input_source_picker <- renderUI({
      kind <- input$input_source_kind %||% "configured"
      if (kind == "configured") {
        cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                                "config.yml")
        cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
        path <- if (is.null(cfg)) "(unable to read config)" else cfg$paths$input_dir
        return(p(class = "small-muted",
                  "Reads from ", tags$code(path)))
      }
      if (kind == "drop_folder") {
        drop_root <- drop_root_path()
        drops <- tryCatch(list_data_drops(drop_root),
                           error = function(e) NULL)
        if (is.null(drops) || nrow(drops) == 0) {
          return(div(class = "alert alert-warning",
                      "No data drop folders found at ",
                      tags$code(drop_root %||% "(unset)"),
                      ". Set ", tags$code("paths.data_drop_root"),
                      " in config.yml to point at the data team's drop ",
                      "folder, or use the configured directory or zip ",
                      "upload instead."))
        }
        labels <- ifelse(drops$looks_complete,
                          sprintf("%s — %d files (complete)",
                                  drops$name, drops$n_files),
                          sprintf("%s — %d files (INCOMPLETE)",
                                  drops$name, drops$n_files))
        # Default selection: most recent (drops are pre-sorted newest first)
        return(tagList(
          selectInput(ns("drop_pick"), "Pick a drop folder",
                       choices = setNames(drops$path, labels),
                       selected = drops$path[1], width = "650px"),
          p(class = "small-muted",
            "Listing immediate subfolders of ",
            tags$code(drop_root), ".")
        ))
      }
      if (kind == "upload") {
        # Surface the configured upload limit so a user hitting it has
        # an immediate breadcrumb to fix it (config.yml::run.max_upload_size_mb).
        max_bytes <- getOption("shiny.maxRequestSize", 5 * 1024 * 1024)
        max_mb <- round(max_bytes / 1024 / 1024)
        return(tagList(
          fileInput(ns("zip_upload"), "Choose a zip file",
                     accept = c(".zip", "application/zip",
                                 "application/x-zip-compressed")),
          p(class = "small-muted",
            sprintf("Upload limit: %d MB. ", max_mb),
            "The zip should contain the 12 input files at the top level, ",
            "or wrapped in a single folder (e.g. ",
            tags$code("Input/AccountMaster.xlsx"), "). ",
            "Raise the limit in ", tags$code("config.yml"),
            " under ", tags$code("run.max_upload_size_mb"),
            " if you need more.")
        ))
      }
      NULL
    })

    # When user clicks "Validate inputs", resolve the directory based on
    # source kind, run the structural check, store the result.
    observeEvent(input$do_validate_inputs, {
      kind <- input$input_source_kind %||% "configured"
      resolved_dir <- NULL
      meta <- list(kind = kind, details = list())

      if (kind == "configured") {
        cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                                "config.yml")
        cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
        if (is.null(cfg)) {
          showNotification("Could not read config.yml", type = "error")
          return()
        }
        resolved_dir <- cfg$paths$input_dir
        meta$details <- list(path = resolved_dir)
      } else if (kind == "drop_folder") {
        sel <- input$drop_pick
        if (is.null(sel) || !nzchar(sel)) {
          showNotification("Pick a drop folder first.", type = "warning")
          return()
        }
        resolved_dir <- sel
        meta$details <- list(path = sel, drop_name = basename(sel))
      } else if (kind == "upload") {
        upload <- input$zip_upload
        if (is.null(upload) || nrow(upload) == 0) {
          showNotification("Pick a zip file first.", type = "warning")
          return()
        }
        result <- tryCatch(
          acquire_inputs_from_zip(upload$datapath[1]),
          error = function(e) e
        )
        if (inherits(result, "error")) {
          showNotification(paste("Zip extract failed:",
                                   conditionMessage(result)),
                            type = "error", duration = 10)
          return()
        }
        resolved_dir <- result$path
        meta$details <- list(
          path           = resolved_dir,
          source_zip     = upload$name[1],
          extracted_at   = result$extracted_at
        )
      }

      withProgress(message = "Validating inputs", value = 0.3, {
        check <- tryCatch(validate_input_directory(resolved_dir),
                           error = function(e) NULL)
        setProgress(1)
      })
      if (is.null(check)) {
        showNotification("Validation failed unexpectedly.", type = "error")
        input_validation(NULL); return()
      }

      input_validation(check)
      n_fail <- sum(check$status == "FAIL")
      if (n_fail == 0) {
        input_dir_override(if (kind == "configured") NULL else resolved_dir)
        input_source_meta(meta)
        showNotification(sprintf("Inputs OK (%d checks passed). Pre-run check is enabled.",
                                   nrow(check)),
                          type = "message", duration = 5)
      } else {
        input_dir_override(NULL)
        input_source_meta(list(kind = "configured", details = list()))
        showNotification(sprintf("%d structural check(s) failed. Fix and re-validate.",
                                   n_fail),
                          type = "warning", duration = 8)
      }
    })

    output$input_validation_result <- renderUI({
      v <- input_validation()
      if (is.null(v)) {
        return(p(class = "small-muted", style = "margin-top: 0.5em;",
                  "Click Validate inputs to enable Pre-run check."))
      }
      n_fail <- sum(v$status == "FAIL")
      n_pass <- sum(v$status == "PASS")
      pill <- if (n_fail == 0) {
        sprintf('<span class="pill pill-pass">All %d checks passed</span>',
                n_pass)
      } else {
        sprintf('<span class="pill pill-error">%d FAIL, %d PASS — Pre-run check disabled</span>',
                n_fail, n_pass)
      }
      # When there are failures, also surface them as an inline
      # bulleted list so the user sees what's missing without scrolling
      # the table or squinting at it. The full table renders below for
      # detail.
      fail_block <- NULL
      if (n_fail > 0) {
        failed <- v[v$status == "FAIL", , drop = FALSE]
        fail_block <- div(style = "margin-top: 0.5em;",
          tags$strong("Failures:"),
          tags$ul(class = "small-muted",
            lapply(seq_len(nrow(failed)), function(i) {
              tags$li(
                tags$code(failed$check[i]),
                if (nzchar(as.character(failed$detail[i] %||% ""))) {
                  tags$span(class = "small-muted",
                            sprintf(" — %s", failed$detail[i]))
                } else NULL
              )
            })
          )
        )
      }
      tagList(
        div(style = "margin-top: 0.75em;", HTML(pill)),
        fail_block,
        DT::DTOutput(ns("input_validation_table"))
      )
    })

    output$input_validation_table <- DT::renderDT({
      v <- input_validation()
      if (is.null(v) || nrow(v) == 0) return(NULL)
      v$status_pill <- ifelse(v$status == "PASS",
                                '<span class="pill pill-pass">PASS</span>',
                                '<span class="pill pill-error">FAIL</span>')
      DT::datatable(
        data.frame(status = v$status_pill,
                    check  = v$check,
                    detail = v$detail,
                    stringsAsFactors = FALSE),
        rownames = FALSE,
        escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 12, dom = "tip")
      )
    })

    # Pre-run check button: only enabled after structural validation
    # of the chosen input source has passed.
    output$pre_run_button_slot <- renderUI({
      v <- input_validation()
      can_run <- !is.null(v) && sum(v$status == "FAIL") == 0
      if (!can_run) {
        return(tags$button(
          id = ns("do_pre_run"),
          class = "btn btn-primary action-button",
          disabled = NA,
          style = "opacity: 0.5; cursor: not-allowed;",
          icon("magnifying-glass"), " Pre-run check",
          tags$span(class = "small-muted", style = "margin-left: 0.5em;",
                    "(validate inputs first)")
        ))
      }
      actionButton(ns("do_pre_run"), "Pre-run check",
                    icon = icon("magnifying-glass"),
                    class = "btn-primary")
    })

    # Pre-run check
    observeEvent(input$do_pre_run, {
      pick <- input$snapshot_pick
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      ovr <- input_dir_override()
      withProgress(message = "Pre-run check", value = 0.3, {
        res <- tryCatch(
          if (pick == "__LIVE__") {
            pre_run_check(config_path = cfg_path, verbose = FALSE,
                            input_dir_override = ovr)
          } else {
            pre_run_check(snapshot = pick, verbose = FALSE,
                            input_dir_override = ovr)
          },
          error = function(e) {
            showNotification(paste("Pre-run failed:", conditionMessage(e)),
                              type = "error", duration = 10)
            NULL
          }
        )
        setProgress(1)
      })
      pre_run_results(res)
    })

    output$pre_run_status <- renderUI({
      r <- pre_run_results()
      if (is.null(r)) {
        return(p(class = "small-muted",
                  "Click \"Pre-run check\" to run input-stage validators."))
      }
      n_pass <- sum(r$passed)
      n_err  <- sum(!r$passed & r$severity == "ERROR" & !(r$suppressed %||% FALSE))
      n_warn <- sum(!r$passed & r$severity == "WARN"  & !(r$suppressed %||% FALSE))
      summary_pill <- if (n_err > 0) {
        tags$span(class = "pill pill-error", sprintf("%d ERROR — Run blocked", n_err))
      } else if (n_warn > 0) {
        tags$span(class = "pill pill-warn", sprintf("%d WARN — review then proceed", n_warn))
      } else {
        tags$span(class = "pill pill-pass", sprintf("All %d checks passed", n_pass))
      }
      tagList(
        h5(summary_pill),
        DT::DTOutput(ns("pre_run_table"))
      )
    })

    output$pre_run_table <- DT::renderDT({
      r <- pre_run_results()
      if (is.null(r) || nrow(r) == 0) return(NULL)
      r$passed <- as.logical(r$passed)
      r$status <- ifelse(r$passed,
                          '<span class="pill pill-pass">PASS</span>',
                          sprintf('<span class="pill pill-%s">%s</span>',
                                  tolower(r$severity), toupper(r$severity)))
      msg <- vapply(r$details, function(d) {
        if (is.null(d)) return("")
        if (!is.null(d$message)) return(as.character(d$message))
        ""
      }, character(1))
      DT::datatable(
        data.frame(status=r$status, id=r$id, context=r$context,
                    description=r$description, message=msg,
                    stringsAsFactors=FALSE),
        rownames = FALSE, escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 25, dom = "tip")
      )
    })

    # ---- Run type gate (Official requires approved snapshot) ----------
    # Computes once: is the current (run_type, snapshot) combination
    # valid? Used both to render an explanation under the radio AND
    # to gate the Start run button.
    .run_type_validity <- reactive({
      run_type <- input$run_type %||% "unofficial"
      pick <- input$snapshot_pick %||% "__LIVE__"
      if (run_type == "unofficial") {
        return(list(ok = TRUE, reason = ""))
      }
      # Official rules:
      if (pick == "__LIVE__") {
        return(list(ok = FALSE,
                    reason = paste(
                      "Official runs require an approved snapshot.",
                      "Live config has no approval status \u2014 pick an",
                      "approved snapshot, or switch to Unofficial.")))
      }
      meta <- tryCatch(read_snapshot_metadata(pick, snaps_root()),
                        error = function(e) NULL)
      if (is.null(meta)) {
        return(list(ok = FALSE,
                    reason = "Could not read snapshot metadata."))
      }
      if (!isTRUE(meta$status == "approved")) {
        return(list(ok = FALSE,
                    reason = sprintf(paste(
                      "Official runs require an approved snapshot.",
                      "'%s' is currently '%s'. Promote it to approved",
                      "first, or switch to Unofficial."),
                      pick, meta$status %||% "unknown")))
      }
      list(ok = TRUE, reason = "")
    })

    output$run_type_gate <- renderUI({
      v <- .run_type_validity()
      if (isTRUE(v$ok)) {
        if ((input$run_type %||% "unofficial") == "official") {
          return(div(class = "alert alert-success", style = "margin-top: 0.5em;",
                      tags$strong("Official run"), " — output will be a sanctioned ",
                      "deliverable subject to approval workflow."))
        } else {
          return(div(class = "alert alert-secondary", style = "margin-top: 0.5em;",
                      tags$strong("Unofficial run"), " — useful for testing and ",
                      "what-if analysis. Skips approval workflow. Output is ",
                      "exportable but clearly marked UNOFFICIAL."))
        }
      }
      div(class = "alert alert-warning", style = "margin-top: 0.5em;",
          tags$strong("Cannot start an Official run: "),
          v$reason)
    })

    output$start_run_slot <- renderUI({
      r <- pre_run_results()
      pre_run_ok <- !is.null(r) && nrow(r) > 0 &&
                  sum(!r$passed & r$severity == "ERROR" &
                       !(r$suppressed %||% FALSE)) == 0
      type_v <- .run_type_validity()
      can_run <- pre_run_ok && isTRUE(type_v$ok)

      label <- if ((input$run_type %||% "unofficial") == "official") {
        "Start OFFICIAL run"
      } else {
        "Start unofficial run"
      }
      if (!can_run) {
        return(tags$button(
          id = ns("do_phase1"),
          class = "btn btn-success action-button",
          disabled = NA,
          style = "opacity: 0.5; cursor: not-allowed;",
          icon("play"), " ", label
        ))
      }
      actionButton(ns("do_phase1"), label,
                    icon = icon("play"), class = "btn-success")
    })

    # ============== START RUN -> PHASE 1 =============================
    observeEvent(input$do_phase1, {
      pick <- input$snapshot_pick
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      err <- NULL
      state <- NULL
      ovr <- input_dir_override()
      src_meta <- input_source_meta()
      run_type_val <- input$run_type %||% "unofficial"
      withProgress(message = "Phase 1: load + validate + transform", value = 0.1, {
        state <- tryCatch(
          if (pick == "__LIVE__") {
            run_etl_phase1(config_path = cfg_path, keep_history = TRUE,
                            verbose = FALSE,
                            input_dir_override = ovr,
                            input_source_meta = src_meta,
                            run_type = run_type_val)
          } else {
            run_etl_phase1(snapshot = pick, keep_history = TRUE,
                            verbose = FALSE,
                            input_dir_override = ovr,
                            input_source_meta = src_meta,
                            run_type = run_type_val)
          },
          error = function(e) { err <<- conditionMessage(e); NULL }
        )
        setProgress(1)
      })
      if (is.null(state)) {
        showNotification(paste("Phase 1 failed:", err),
                          type = "error", duration = 15)
        return()
      }
      phase1_state(state)
      # Reset override buffers for the new run
      overrides_rating(.empty_override_buf("rating"))
      overrides_stage(.empty_override_buf("stage"))
      overrides_restr(.empty_override_buf("restructuring"))
      phase_state("phase1_done")
    })

    # ============== STEP 2: PAUSE PAGE ==============================
    output$pause_summary <- renderUI({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      tagList(
        h4(sprintf("Run %s — paused for review", st$run_id)),
        p(class = "small-muted",
          sprintf("Customers: %d   |   Investments: %d   |   Validation findings (so far): %d",
                  nrow(st$cm_view %||% data.frame()),
                  nrow(st$inv_view %||% data.frame()),
                  if (!is.null(st$validation)) sum(!st$validation$passed) else 0))
      )
    })

    output$cm_view_table <- DT::renderDT({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      cm <- st$cm_view
      cols <- intersect(c("customer_id", "customer_name", "rating_final",
                            "stage_final", "restructuring_final",
                            "watchlist_status", "exposure_total",
                            "max_dpd"),
                         colnames(cm))
      df <- cm[, cols, drop = FALSE]
      # Cast ID columns to character so DT renders a search box, not a
      # numeric range slider. Same logic as the Outputs preview.
      id_pattern <- "(?i)(^id$|_id$|id_|Id$|ID$|^contract|customer$|account)"
      id_cols <- grep(id_pattern, colnames(df), perl = TRUE)
      for (i in id_cols) df[[i]] <- as.character(df[[i]])
      DT::datatable(
        df,
        rownames = FALSE, filter = "top",
        selection = "single",
        class = "narrow-table compact",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    # When a row is selected, populate the override editor below
    selected_customer <- reactive({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      idx <- input$cm_view_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      cm <- st$cm_view
      cm[idx, , drop = FALSE]
    })

    output$override_editor <- renderUI({
      sel <- selected_customer()
      if (is.null(sel)) {
        return(p(class = "small-muted",
                  "Select a customer above to add overrides."))
      }
      cid <- as.character(sel$customer_id)

      # ---- Compute valid override choices given the calculated state ----
      # Rating: all 21 internal QDB ratings (lending is all-internal in this
      # workbook). Read from the static reference loaded in phase 1.
      rating_choices <- c("(no change)" = "")
      st <- phase1_state()
      if (!is.null(st) && !is.null(st$static$master_rating_scale)) {
        mrs <- st$static$master_rating_scale
        internal <- mrs[mrs$rating_type == "Internal", , drop = FALSE]
        # Order by hierarchy ascending so the dropdown reads from best to worst
        internal <- internal[order(internal$hierarchy), , drop = FALSE]
        rating_choices <- c(rating_choices, setNames(internal$rating, internal$rating))
      }

      # Stage: only worsening transitions allowed.
      cur_stage <- as.character(sel$stage_final %||% "Stage 1")
      stage_choices <- c("(no change)" = "")
      if (cur_stage == "Stage 1") {
        stage_choices <- c(stage_choices, "Stage 2", "Stage 3")
      } else if (cur_stage == "Stage 2") {
        stage_choices <- c(stage_choices, "Stage 3")
      }
      # Stage 3 (or unknown) -> no transitions possible; dropdown shows only "(no change)"

      # Restructuring: flip whichever way is the opposite of current.
      cur_restr <- as.character(sel$restructuring_final %||% "")
      restr_choices <- c("(no change)" = "")
      if (cur_restr == "Restructured") {
        restr_choices <- c(restr_choices, "Not Restructured" = "Not Restructured")
      } else {
        # Treat anything-not-Restructured as the unrestructured case
        restr_choices <- c(restr_choices, "Restructured" = "Restructured")
      }

      tagList(
        h5(sprintf("Override for customer %s — %s", cid,
                    sel$customer_name %||% "")),
        tags$dl(class = "row",
          tags$dt(class = "col-sm-3", "calculated rating"),
          tags$dd(class = "col-sm-9", as.character(sel$rating_final %||% "—")),
          tags$dt(class = "col-sm-3", "calculated stage"),
          tags$dd(class = "col-sm-9", as.character(sel$stage_final %||% "—")),
          tags$dt(class = "col-sm-3", "calculated restructuring"),
          tags$dd(class = "col-sm-9",
                   if (nzchar(cur_restr)) cur_restr else "Not Restructured")
        ),
        fluidRow(
          column(4,
            selectInput(ns("ov_rating_val"), "Override rating",
                         choices = rating_choices, selected = "")),
          column(4,
            selectInput(ns("ov_stage_val"), "Override stage",
                         choices = stage_choices, selected = "")),
          column(4,
            selectInput(ns("ov_restr_val"), "Override restructuring",
                         choices = restr_choices, selected = ""))
        ),
        if (cur_stage == "Stage 3") {
          p(class = "small-muted",
            tags$em("Stage 3 cannot be overridden — IFRS 9 staging only worsens. Lower stages allow override to a worse stage; this customer is already at the worst."))
        },
        textAreaInput(ns("ov_reason"), "Reason (required)",
                       rows = 2,
                       placeholder = "e.g. credit committee decision 2026-Q1, see ticket #..."),
        actionButton(ns("ov_apply"), "Add override",
                      icon = icon("plus"), class = "btn-primary")
      )
    })

    observeEvent(input$ov_apply, {
      sel <- selected_customer()
      if (is.null(sel)) return()
      cid    <- as.character(sel$customer_id)
      reason <- trimws(input$ov_reason %||% "")
      r_val  <- trimws(input$ov_rating_val %||% "")
      s_val  <- input$ov_stage_val %||% ""
      x_val  <- input$ov_restr_val %||% ""

      if (!nzchar(reason)) {
        showNotification("Reason is required for any override.",
                          type = "warning")
        return()
      }
      if (!nzchar(r_val) && !nzchar(s_val) && !nzchar(x_val)) {
        showNotification("Pick at least one value to override.",
                          type = "warning")
        return()
      }

      if (nzchar(r_val)) {
        overrides_rating(.add_override_row(overrides_rating(),
                                             cid, r_val, reason,
                                             prior = as.character(sel$rating_final %||% "")))
      }
      if (nzchar(s_val)) {
        overrides_stage(.add_override_row(overrides_stage(),
                                            cid, s_val, reason,
                                            prior = as.character(sel$stage_final %||% "")))
      }
      if (nzchar(x_val)) {
        overrides_restr(.add_override_row(overrides_restr(),
                                            cid, x_val, reason,
                                            prior = as.character(sel$restructuring_final %||% "")))
      }
      showNotification(sprintf("Override added for %s", cid),
                        type = "message", duration = 3)
      updateTextInput(session, "ov_rating_val", value = "")
      updateSelectInput(session, "ov_stage_val", selected = "")
      updateSelectInput(session, "ov_restr_val", selected = "")
      updateTextAreaInput(session, "ov_reason", value = "")
    })

    output$overrides_pending <- renderUI({
      r <- overrides_rating(); s <- overrides_stage(); x <- overrides_restr()
      tagList(
        h5(sprintf("Pending overrides: rating=%d, stage=%d, restructuring=%d",
                    nrow(r), nrow(s), nrow(x))),
        if (nrow(r) > 0) tagList(h6("Rating overrides"), DT::DTOutput(ns("tbl_r"))),
        if (nrow(s) > 0) tagList(h6("Stage overrides"),  DT::DTOutput(ns("tbl_s"))),
        if (nrow(x) > 0) tagList(h6("Restructuring overrides"), DT::DTOutput(ns("tbl_x")))
      )
    })

    output$tbl_r <- DT::renderDT(.dt_compact(overrides_rating()))
    output$tbl_s <- DT::renderDT(.dt_compact(overrides_stage()))
    output$tbl_x <- DT::renderDT(.dt_compact(overrides_restr()))

    # ============== CONTINUE -> PHASE 2 =============================
    observeEvent(input$do_phase2, {
      st <- phase1_state(); if (is.null(st)) return()
      ov <- list(
        rating        = .convert_to_phase2(overrides_rating(), "override_rating"),
        stage         = .convert_to_phase2(overrides_stage(),  "override_stage"),
        restructuring = .convert_to_phase2(overrides_restr(),  "override_restructuring")
      )
      err <- NULL
      result <- NULL
      withProgress(message = "Phase 2: derived + write outputs", value = 0.1, {
        result <- tryCatch(
          run_etl_phase2(st, overrides = ov, reconcile = TRUE),
          error = function(e) { err <<- conditionMessage(e); NULL }
        )
        setProgress(1)
      })
      if (is.null(result)) {
        showNotification(paste("Phase 2 failed:", err),
                          type = "error", duration = 15)
        return()
      }
      phase2_result(result)
      phase_state("phase2_done")
      # Notify Runs page + Approval queue that a new run exists on
      # disk. Without this, those modules only refresh on manual
      # button-click and miss the just-completed run.
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed(
          session$userData$runs_changed() + 1)
      }
    })

    observeEvent(input$do_cancel, {
      phase_state("idle")
      phase1_state(NULL)
      phase2_result(NULL)
    })

    # ============== STEP 3: SUMMARY PAGE ============================
    output$run_summary <- renderUI({
      r <- phase2_result(); if (is.null(r)) return(NULL)
      tagList(
        h4(sprintf("Run %s complete — pending approval", r$run_id)),
        tags$ul(
          tags$li(sprintf("Duration: %.1fs", r$duration_seconds)),
          tags$li(sprintf("Outputs: %d files", length(r$output_paths %||% list()))),
          tags$li(sprintf("Path: %s", r$run_dir)),
          tags$li(sprintf("Overrides applied: rating=%d, stage=%d, restructuring=%d",
                            r$overrides_applied$rating %||% 0,
                            r$overrides_applied$stage %||% 0,
                            r$overrides_applied$restructuring %||% 0))
        ),
        p(class = "small-muted",
          "Visit the ", tags$strong("Approval queue"), " page to approve or ",
          "reject this run, or the ", tags$strong("Runs"), " page to view ",
          "manifest, validation, and outputs."),
        actionButton(ns("do_new_run"), "Start another run",
                      icon = icon("rotate"), class = "btn-secondary")
      )
    })

    observeEvent(input$do_new_run, {
      phase_state("idle")
      phase1_state(NULL)
      phase2_result(NULL)
      pre_run_results(NULL)
    })
  })
}


# ---- UI fragments per state -----------------------------------------

.ui_idle <- function(ns) {
  tagList(
    fluidRow(column(12,
      card(
        card_header("1. Input source"),
        p(class = "small-muted",
          "Pick where the 12 input files come from. Validate the choice ",
          "before running. The Pre-run check stays disabled until input ",
          "validation passes."),
        radioButtons(ns("input_source_kind"), label = NULL,
                      choices = c(
                        "Use the configured input directory"     = "configured",
                        "Pick a folder from the data drop"       = "drop_folder",
                        "Upload a zip from my computer"           = "upload"
                      ),
                      selected = "configured", inline = FALSE),
        uiOutput(ns("input_source_picker")),
        div(style = "margin-top: 0.75em;",
          actionButton(ns("do_validate_inputs"), "Validate inputs",
                        icon = icon("circle-check"),
                        class = "btn-outline-primary")
        ),
        uiOutput(ns("input_validation_result"))
      )
    )),
    fluidRow(
      column(6,
        card(
          card_header("2. Configuration"),
          # Snapshot dropdown is rendered as a reactive uiOutput rather
          # than a static selectInput + updateSelectInput. Reason: the
          # update-style approach has a race — when the app starts, the
          # observe fires before the client has registered the
          # selectInput, so the update message is dropped and the
          # dropdown stays at its default ("(live config)" only).
          # Rendering reactively guarantees the choices are correct
          # the first time the dropdown reaches the client.
          uiOutput(ns("snapshot_pick_ui")),
          uiOutput(ns("snapshot_meta")),
          # Run type — H18.
          # Official: requires an *approved* snapshot. Lands as
          #   pending_checker → goes through approval workflow → exportable
          #   as a sanctioned deliverable.
          # Unofficial: any snapshot OR live config. Skips approval, lands
          #   as `unofficial` (terminal). Exportable but clearly marked.
          radioButtons(ns("run_type"), "Run type",
                        choices = c(
                          "Unofficial — for testing / what-if analysis" = "unofficial",
                          "Official — sanctioned deliverable (needs approved snapshot)" = "official"
                        ),
                        selected = "unofficial"),
          uiOutput(ns("run_type_gate")),
          # Pre-run check is gated on input validation: the slot below
          # renders the button as enabled OR with a disabled-with-reason
          # message depending on the validation state.
          uiOutput(ns("pre_run_button_slot")),
          uiOutput(ns("start_run_slot"))
        )
      ),
      column(6,
        card(
          card_header("3. Pre-run findings"),
          uiOutput(ns("pre_run_status"))
        )
      )
    )
  )
}


.ui_pause <- function(ns) {
  tagList(
    fluidRow(column(12,
      card(
        card_header("Run paused — review and override"),
        uiOutput(ns("pause_summary")),
        p(class = "small-muted",
          "Below is the calculated customer-level view. Click a row to add ",
          "overrides for rating, stage, or restructuring. When done, click ",
          tags$strong("Continue"), " to finish the run."),
        DT::DTOutput(ns("cm_view_table"))
      )
    )),
    fluidRow(
      column(6,
        card(
          card_header("Add override"),
          uiOutput(ns("override_editor"))
        )
      ),
      column(6,
        card(
          card_header("Pending overrides for this run"),
          uiOutput(ns("overrides_pending"))
        )
      )
    ),
    fluidRow(column(12,
      hr(),
      div(style = "margin-bottom: 1em;",
        actionButton(ns("do_phase2"), "Continue (apply overrides + finish)",
                      icon = icon("forward"), class = "btn-success"),
        tags$span(style = "margin-left: 0.5em;",
          actionButton(ns("do_cancel"), "Cancel run",
                        icon = icon("xmark"), class = "btn-secondary"))
      )
    ))
  )
}


.ui_summary <- function(ns) {
  fluidRow(column(12,
    card(
      card_header("Run complete"),
      uiOutput(ns("run_summary"))
    )
  ))
}


# ---- Override-buffer helpers ---------------------------------------

.empty_override_buf <- function(kind) {
  data.frame(
    customer_id = character(),
    value       = character(),
    prior_value = character(),
    reason      = character(),
    stringsAsFactors = FALSE
  )
}

.add_override_row <- function(buf, customer_id, value, reason, prior = "") {
  rbind(buf, data.frame(
    customer_id = customer_id,
    value       = value,
    prior_value = prior,
    reason      = reason,
    stringsAsFactors = FALSE
  ))
}

.dt_compact <- function(df) {
  if (nrow(df) == 0) return(NULL)
  DT::datatable(
    df, rownames = FALSE, class = "narrow-table compact",
    options = list(pageLength = 5, dom = "tip")
  )
}

# Convert UI buffer (customer_id, value, prior_value, reason) into the
# shape expected by run_etl_phase2's overrides argument.
.convert_to_phase2 <- function(buf, value_col) {
  if (nrow(buf) == 0) return(NULL)
  out <- data.frame(
    customer_id = buf$customer_id,
    reason      = buf$reason,
    stringsAsFactors = FALSE
  )
  out[[value_col]] <- buf$value
  out
}
