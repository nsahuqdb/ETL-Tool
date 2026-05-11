# =============================================================================
# app/modules/mod_snapshot_manager.R
#
# Snapshot lifecycle management: create new snapshots, promote drafts
# through the status graph (draft -> pending -> approved -> archived).
#
# This is a write-capable counterpart to mod_snapshots.R (which is
# read-only browsing). Splitting them keeps the read-only view clean
# and lets reviewers see the manager separately if they want.
#
# Calls these pipeline helpers (from R/snapshots.R):
#   list_snapshots, read_snapshot_metadata, create_snapshot, promote_snapshot
# =============================================================================

mod_snapshot_manager_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Snapshot manager"),
      p(class = "small-muted",
        "Create new snapshots and move them through the lifecycle: ",
        tags$strong("draft"), " → ", tags$strong("tested"), " → ",
        tags$strong("pending_final"), " → ", tags$strong("approved"), ". "),
      p(class = "small-muted",
        tags$strong("Workflow:"), " (1) Create as ", tags$em("draft"),
        " — editable in Edit snapshot tab. (2) Use the snapshot in ",
        tags$strong("unofficial runs"), " to test its impact. ",
        "(3) Mark as ", tags$em("tested"),
        " when satisfied — locks it for review. (4) Submit to ",
        tags$em("pending_final"),
        " for the final approver (different user when separation is on). ",
        "(5) Final approver moves to ", tags$em("approved"),
        " — now usable for ", tags$strong("official runs"), ". ",
        "Each transition records an audit event and requires a reason.")
    )),
    fluidRow(
      column(5,
        card(
          card_header("Create snapshot"),
          textInput(ns("new_label"),       "Label",
                     placeholder = "e.g. 2026-Q1-draft-1"),
          textAreaInput(ns("new_description"), "Description",
                         rows = 3,
                         placeholder = "What's in this snapshot? Why was it created?"),
          selectInput(ns("new_parent"), "Parent snapshot (optional)",
                       choices = c("(none — first snapshot)" = "")),
          p(class = "small-muted",
            "Copies the current ", tags$code("config/"), " and ",
            tags$code("data-raw/static/"), " into ",
            tags$code("config_snapshots/<label>/"), "."),
          actionButton(ns("do_create"), "Create snapshot",
                        icon = icon("plus"),
                        class = "btn-primary"),
          uiOutput(ns("create_status"))
        )
      ),
      column(7,
        card(
          card_header("Promote snapshot"),
          selectInput(ns("promote_pick"), "Snapshot",
                       choices = c("(pick one)" = "")),
          uiOutput(ns("promote_state")),
          uiOutput(ns("promote_actions"))
        )
      )
    ),
    fluidRow(column(12,
      hr(),
      h4("All snapshots"),
      DT::DTOutput(ns("snapshots_overview"))
    ))
  )
}


mod_snapshot_manager_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    # Resolve the snapshots directory once. All R snapshot helpers accept
    # snapshots_root as a parameter; passing it explicitly defeats Shiny's
    # cwd unpredictability.
    snaps_root <- function() {
      proj_root <- getOption("ifrs9.project_root", getwd())
      getOption("ifrs9.snapshots_dir",
                file.path(proj_root, "config_snapshots"))
    }
    config_root <- function() {
      file.path(getOption("ifrs9.project_root", getwd()), "config")
    }
    static_root <- function() {
      file.path(getOption("ifrs9.project_root", getwd()),
                "data-raw", "static")
    }

    snaps <- reactive({
      refresh()
      tryCatch(list_snapshots(snaps_root()),
                error = function(e) list_snapshots.empty())
    })

    # ---- Update parent + promote dropdowns when snapshots change -----
    observe({
      s <- snaps()
      parent_choices <- c("(none — first snapshot)" = "")
      if (nrow(s) > 0) {
        labels <- sprintf("%s [%s]", s$label, s$status %||% "?")
        parent_choices <- c(parent_choices, setNames(s$label, labels))
      }
      updateSelectInput(session, "new_parent", choices = parent_choices,
                         selected = isolate(input$new_parent) %||% "")

      promote_choices <- c("(pick one)" = "")
      if (nrow(s) > 0) {
        # Order: drafts first (most actionable), then pending, etc
        rank <- c(draft = 0, pending = 1, approved = 2, archived = 3)
        ord  <- order(rank[s$status %||% rep("draft", nrow(s))], s$created_at)
        s2 <- s[ord, , drop = FALSE]
        labels <- sprintf("%s [%s]", s2$label, s2$status %||% "?")
        promote_choices <- c(promote_choices, setNames(s2$label, labels))
      }
      updateSelectInput(session, "promote_pick", choices = promote_choices,
                         selected = isolate(input$promote_pick) %||% "")
    })

    # ---- Create snapshot --------------------------------------------
    observeEvent(input$do_create, {
      label <- trimws(input$new_label %||% "")
      desc  <- trimws(input$new_description %||% "")
      parent <- input$new_parent
      if (!nzchar(label)) {
        showNotification("Label is required.", type = "warning")
        return()
      }
      if (!grepl("^[A-Za-z0-9._-]+$", label)) {
        showNotification("Label must contain only letters, digits, '.', '_', '-'.",
                          type = "warning")
        return()
      }
      if (!nzchar(desc)) {
        showNotification("Description is required (audit trail).",
                          type = "warning")
        return()
      }

      result <- tryCatch({
        create_snapshot(
          label = label,
          description = desc,
          created_by = Sys.info()[["user"]] %||% "unknown",
          parent = if (nzchar(parent)) parent else NULL,
          config_dir = config_root(),
          static_dir = static_root(),
          snapshots_root = snaps_root()
        )
      }, error = function(e) e)

      if (inherits(result, "error")) {
        showNotification(paste("Create failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(paste("Created snapshot:", label),
                          type = "message", duration = 5)
        updateTextInput(session, "new_label", value = "")
        updateTextAreaInput(session, "new_description", value = "")
        refresh(refresh() + 1)
        # Notify other modules (Run pipeline dropdown, approval queue) so
        # their snapshot lists pick up the change without a restart.
        if (!is.null(session$userData$snapshots_changed))
          session$userData$snapshots_changed(
            session$userData$snapshots_changed() + 1)
      }
    })

    # ---- Promote dropdown / actions --------------------------------
    selected_meta <- reactive({
      refresh()  # re-read after any successful create/promote action
      pick <- input$promote_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      tryCatch(read_snapshot_metadata(pick, snaps_root()),
                error = function(e) NULL)
    })

    output$promote_state <- renderUI({
      m <- selected_meta()
      if (is.null(m)) {
        return(p(class = "small-muted", "Pick a snapshot to see its state."))
      }
      tags$dl(class = "row",
        tags$dt(class = "col-sm-4", "label"),       tags$dd(class = "col-sm-8", m$label),
        tags$dt(class = "col-sm-4", "status"),      tags$dd(class = "col-sm-8",
          tags$span(class = sprintf("pill pill-%s", m$status %||% "draft"),
                    m$status %||% "?")),
        tags$dt(class = "col-sm-4", "description"), tags$dd(class = "col-sm-8", m$description %||% "—"),
        tags$dt(class = "col-sm-4", "created_by"),  tags$dd(class = "col-sm-8", m$created_by %||% "—"),
        tags$dt(class = "col-sm-4", "approved_by"), tags$dd(class = "col-sm-8", m$approved_by %||% "—"),
        tags$dt(class = "col-sm-4", "approved_at"), tags$dd(class = "col-sm-8", m$approved_at %||% "—"),
        tags$dt(class = "col-sm-4", "approval_reason"), tags$dd(class = "col-sm-8", m$approval_reason %||% "—")
      )
    })

    output$promote_actions <- renderUI({
      m <- selected_meta()
      if (is.null(m)) return(NULL)
      cur <- m$status %||% "draft"
      # Treat legacy "pending" the same as new "pending_final"
      if (cur == "pending") cur <- "pending_final"

      # Allowed transitions per snapshots.R::promote_snapshot
      allowed <- list(
        draft         = c("tested"),
        tested        = c("pending_final", "draft"),
        pending_final = c("approved", "rejected", "tested"),
        approved      = c("archived"),
        rejected      = c("draft"),
        archived      = character()
      )[[cur]] %||% character()

      if (length(allowed) == 0) {
        return(p(class = "small-muted",
                  sprintf("No transitions allowed from status '%s'.", cur)))
      }

      # Surface separation-of-duties context: who created this, who's acting.
      creator <- m$created_by %||% NA_character_
      current_user <- Sys.info()[["user"]] %||% "unknown"
      cfg <- tryCatch(.approval_cfg_for_snapshots(),
                       error = function(e) list(enforce_separation_of_duties = FALSE))
      enforce <- isTRUE(cfg$enforce_separation_of_duties)
      same_user <- !is.na(creator) && nzchar(creator) &&
                    identical(tolower(creator), tolower(current_user))
      cant_approve <- enforce && same_user

      sep_block <- tagList(
        tags$dl(class = "row",
          tags$dt(class = "col-sm-4", "Created by"),
            tags$dd(class = "col-sm-8", tags$code(creator %||% "?")),
          tags$dt(class = "col-sm-4", "Acting as"),
            tags$dd(class = "col-sm-8", tags$code(current_user)),
          tags$dt(class = "col-sm-4", "Separation enforced"),
            tags$dd(class = "col-sm-8",
                     if (enforce) tags$strong("Yes")
                     else tags$em("No (dev mode)"))
        )
      )

      tagList(
        sep_block,
        textAreaInput(ns("promote_reason"), "Reason / notes (required)",
                       rows = 2,
                       placeholder = "Will appear in the audit log."),
        do.call(tagList, lapply(allowed, function(target) {
          # Visual treatment per target
          btn_class <- switch(
            target,
            approved      = "btn-success",
            tested        = "btn-primary",
            pending_final = "btn-primary",
            rejected      = "btn-warning",
            archived      = "btn-secondary",
            draft         = "btn-warning",
            "btn-primary"
          )
          # Disable the Approve button when separation is enforced and
          # the current user created this snapshot. Other transitions
          # the creator can self-perform.
          disabled <- (target == "approved") && cant_approve
          btn_id <- ns(paste0("promote_to_", target))
          if (disabled) {
            tags$span(
              style = "margin-right: 0.5em;",
              tags$button(
                id = btn_id,
                class = paste("btn", btn_class),
                disabled = NA,
                style = "opacity: 0.5; cursor: not-allowed;",
                sprintf("→ %s", target)
              ),
              tags$span(class = "small-muted",
                        style = "margin-left: 0.25em;",
                        "(maker can't approve own work)")
            )
          } else {
            tags$span(
              style = "margin-right: 0.5em;",
              actionButton(btn_id,
                            label = sprintf("→ %s", target),
                            class = btn_class)
            )
          }
        }))
      )
    })

    # Wire promote observers — one per possible target state.
    .do_promote <- function(target) {
      m <- selected_meta()
      if (is.null(m)) return()
      reason <- trimws(input$promote_reason %||% "")
      if (!nzchar(reason)) {
        showNotification("Reason is required for any transition.",
                          type = "warning")
        return()
      }
      result <- tryCatch(
        promote_snapshot(
          label = m$label,
          status = target,
          approved_by = Sys.info()[["user"]] %||% "unknown",
          reason = reason,
          snapshots_root = snaps_root()
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Promote failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(sprintf("Snapshot %s -> %s", m$label, target),
                          type = "message")
        updateTextAreaInput(session, "promote_reason", value = "")
        refresh(refresh() + 1)
        # Notify other modules (Run pipeline dropdown, approval queue) so
        # their snapshot lists pick up the change without a restart.
        if (!is.null(session$userData$snapshots_changed))
          session$userData$snapshots_changed(
            session$userData$snapshots_changed() + 1)
      }
    }
    observeEvent(input$promote_to_tested,        .do_promote("tested"))
    observeEvent(input$promote_to_pending_final, .do_promote("pending_final"))
    observeEvent(input$promote_to_approved,      .do_promote("approved"))
    observeEvent(input$promote_to_rejected,      .do_promote("rejected"))
    observeEvent(input$promote_to_draft,         .do_promote("draft"))
    observeEvent(input$promote_to_archived,      .do_promote("archived"))

    # ---- Overview table ---------------------------------------------
    output$snapshots_overview <- DT::renderDT({
      s <- snaps()
      if (nrow(s) == 0) {
        return(DT::datatable(
          data.frame(message = "No snapshots yet. Create one above."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      status_pill <- function(st) {
        cls <- tolower(as.character(st))
        sprintf('<span class="pill pill-%s">%s</span>', cls, st)
      }
      display <- data.frame(
        label       = s$label,
        status      = vapply(s$status, status_pill, character(1)),
        created_at  = s$created_at,
        created_by  = s$created_by,
        parent      = s$parent,
        description = s$description,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display, rownames = FALSE, escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })
  })
}
