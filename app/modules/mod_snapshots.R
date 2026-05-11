# =============================================================================
# app/modules/mod_snapshots.R
#
# The Snapshots page: a list of all snapshots (any status), with a detail
# panel for the selected snapshot showing its metadata, file tree, and
# preview of any config/static file inside.
#
# Read-only. Reads:
#   list_snapshots(snapshots_root)           from R/snapshots.R
#   read_snapshot_metadata(label)
#   snapshot_paths(label)
#   diff_snapshots(a, b)
# =============================================================================

mod_snapshots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Configuration snapshots"),
      p(class = "small-muted",
        "A snapshot is a frozen, named version of the pipeline ",
        "configuration — `config/` and `data-raw/static/` together. ",
        "Snapshots are created explicitly (not automatically per-run) ",
        "and are independent of the run history shown on the Runs page. ",
        "They exist so a run can be reproduced exactly months later by ",
        "pinning a snapshot label."),
      p(class = "small-muted",
        textOutput(ns("snapshots_dir_label"), inline = TRUE)),
      actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                    class = "btn-sm btn-outline-secondary")
    )),
    fluidRow(column(12,
      DT::DTOutput(ns("snapshots_table"))
    )),
    uiOutput(ns("detail_block"))
  )
}


mod_snapshots_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    observeEvent(input$refresh, refresh(refresh() + 1))

    snapshots_root <- function() {
      getOption("ifrs9.snapshots_dir",
                default = Sys.getenv("IFRS9_SNAPSHOTS_DIR", "config_snapshots"))
    }

    snaps <- reactive({
      refresh()
      tryCatch(list_snapshots(snapshots_root()),
                error = function(e) {
                  showNotification(paste("Failed to list snapshots:",
                                          conditionMessage(e)), type = "error")
                  list_snapshots.empty()
                })
    })

    output$snapshots_dir_label <- renderText({
      d <- snapshots_root()
      d_norm <- normalizePath(d, mustWork = FALSE)
      exists <- dir.exists(d)
      sprintf("snapshots directory: %s   |   exists: %s   |   %d snapshots",
              d_norm,
              if (exists) "yes" else "NO — none created yet",
              nrow(snaps()))
    })

    output$snapshots_table <- DT::renderDT({
      s <- snaps()
      if (nrow(s) == 0) {
        return(DT::datatable(
          data.frame(
            information =
              paste("No snapshots found yet. Snapshots are different from runs:",
                    "a run is one execution of the pipeline; a snapshot is a",
                    "frozen named version of the configuration.",
                    "Create one from R with:",
                    "create_snapshot(label = '2026-Q1-draft', description = '...', created_by = 'me')",
                    sep = "\n")
          ),
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
        code_sha    = substr(s$code_sha_at_creation, 1, 8),
        description = s$description,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    selected_label <- reactive({
      idx <- input$snapshots_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      s <- snaps()
      if (idx > nrow(s)) return(NULL)
      s$label[idx]
    })

    output$detail_block <- renderUI({
      lbl <- selected_label()
      if (is.null(lbl)) {
        return(div(class = "small-muted", style = "margin-top: 2em;",
                    "Select a snapshot above to see its metadata + files."))
      }
      meta <- tryCatch(read_snapshot_metadata(lbl, snapshots_root()),
                        error = function(e) NULL)
      if (is.null(meta)) return(p("Could not read snapshot.yml"))

      tagList(
        hr(),
        h4(sprintf("Snapshot %s", lbl)),
        navset_card_tab(
          nav_panel("Metadata", uiOutput(ns("snap_meta"))),
          nav_panel("Files",    uiOutput(ns("snap_files")))
        )
      )
    })

    output$snap_meta <- renderUI({
      lbl <- selected_label(); if (is.null(lbl)) return(NULL)
      meta <- tryCatch(read_snapshot_metadata(lbl, snapshots_root()),
                        error = function(e) NULL)
      if (is.null(meta)) return(NULL)
      tags$dl(class = "row",
        tags$dt(class = "col-sm-3", "label"),       tags$dd(class = "col-sm-9", meta$label),
        tags$dt(class = "col-sm-3", "status"),      tags$dd(class = "col-sm-9",
          tags$span(class = sprintf("pill pill-%s", meta$status %||% "draft"),
                    meta$status %||% "?")),
        tags$dt(class = "col-sm-3", "description"), tags$dd(class = "col-sm-9", meta$description %||% "—"),
        tags$dt(class = "col-sm-3", "created_at"),  tags$dd(class = "col-sm-9", meta$created_at %||% "—"),
        tags$dt(class = "col-sm-3", "created_by"),  tags$dd(class = "col-sm-9", meta$created_by %||% "—"),
        tags$dt(class = "col-sm-3", "parent"),      tags$dd(class = "col-sm-9", meta$parent %||% "—"),
        tags$dt(class = "col-sm-3", "code_sha at creation"),
                                                     tags$dd(class = "col-sm-9", tags$code(meta$code_sha_at_creation %||% "—")),
        tags$dt(class = "col-sm-3", "approved_by"), tags$dd(class = "col-sm-9", meta$approved_by %||% "—"),
        tags$dt(class = "col-sm-3", "approved_at"), tags$dd(class = "col-sm-9", meta$approved_at %||% "—"),
        tags$dt(class = "col-sm-3", "approval_reason"), tags$dd(class = "col-sm-9", meta$approval_reason %||% "—")
      )
    })

    output$snap_files <- renderUI({
      lbl <- selected_label(); if (is.null(lbl)) return(NULL)
      sp <- tryCatch(snapshot_paths(lbl, snapshots_root()),
                      error = function(e) NULL)
      if (is.null(sp)) return(p("Could not resolve snapshot paths."))

      # Recursively list both subtrees so files in nested folders show.
      d <- normalizePath(sp$snapshot_dir, mustWork = FALSE, winslash = "/")
      cfg_files <- list.files(file.path(d, "config"),
                                full.names = TRUE, recursive = TRUE)
      st_files  <- list.files(file.path(d, "static"),
                                full.names = TRUE, recursive = TRUE)
      all_files <- c(cfg_files, st_files)
      if (length(all_files) == 0) return(p(class = "small-muted", "(no files)"))

      norm_paths <- normalizePath(all_files, mustWork = FALSE, winslash = "/")
      prefix <- paste0(d, "/")
      labels <- ifelse(startsWith(norm_paths, prefix),
                        substr(norm_paths, nchar(prefix) + 1, nchar(norm_paths)),
                        basename(norm_paths))

      tagList(
        div(style = "margin-bottom: 0.5em;",
          actionButton(ns("reload_file"), "Reload from disk",
                        icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary"),
          tags$span(class = "small-muted",
                    style = "margin-left: 0.5em;",
                    "Click after editing in ", tags$strong("Edit snapshot"),
                    " to see the latest content.")
        ),
        selectInput(ns("file_pick"), "File",
                     choices = setNames(norm_paths, labels),
                     width = "500px"),
        # Show the path being read so the user can verify
        textOutput(ns("file_path_label"), inline = FALSE),
        verbatimTextOutput(ns("file_content"))
      )
    })

    # Bump on Reload click; file_content depends on this counter so
    # it forcibly re-reads from disk.
    file_reload <- reactiveVal(0)
    observeEvent(input$reload_file, file_reload(file_reload() + 1))

    output$file_path_label <- renderText({
      pick <- input$file_pick
      if (is.null(pick) || !nzchar(pick)) return("")
      sprintf("path: %s", pick)
    })

    output$file_content <- renderText({
      file_reload()  # subscribe so Reload triggers a re-read
      pick <- req(input$file_pick)
      if (!file.exists(pick)) return("(file not found)")
      info <- file.info(pick)
      if (info$size > 1024 * 200) {
        return(sprintf("(file too large to preview: %d bytes)", info$size))
      }
      paste(readLines(pick, warn = FALSE), collapse = "\n")
    })
  })
}


list_snapshots.empty <- function() {
  tibble::tibble(
    label = character(), status = character(),
    created_at = character(), created_by = character(),
    description = character(), parent = character(),
    code_sha_at_creation = character()
  )
}
