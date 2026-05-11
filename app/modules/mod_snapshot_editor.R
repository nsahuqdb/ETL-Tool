# =============================================================================
# app/modules/mod_snapshot_editor.R
#
# H14: edit a draft snapshot's contents through the UI.
#
# UI shape:
#   1. Pick a snapshot from a dropdown.
#   2. If status != "draft": see an info card explaining that edits
#      are gated, with a Clone-to-draft form.
#   3. If status == "draft": pick an editable file from a whitelist
#      dropdown. YAML files render as a textarea; CSV files render
#      as a DT::editable table. Save button validates + writes
#      atomically.
#
# Calls these pipeline helpers (R/snapshots.R):
#   list_snapshots, read_snapshot_metadata, snapshot_paths
#   editable_snapshot_files, clone_snapshot,
#   save_snapshot_yaml, save_snapshot_csv
# =============================================================================

mod_snapshot_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h4("Snapshot editor"),
      div(class = "alert alert-secondary", style = "margin-bottom: 1em;",
        tags$strong("How to use this page:"),
        tags$ol(style = "margin-bottom: 0; margin-top: 0.5em;",
          tags$li("Pick a snapshot from the dropdown below. ",
                   "Only ", tags$strong("draft"), " snapshots are editable. ",
                   "Pending/approved/archived snapshots show a Clone-to-draft form."),
          tags$li("Pick a file to edit. YAML files open in a code editor; ",
                   "CSV files open as a table you can double-click to edit."),
          tags$li("Make your changes and click ", tags$strong("Save"), ". ",
                   "Saves are atomic — either the whole file is rewritten ",
                   "successfully, or nothing changes."),
          tags$li("To verify the saved content, switch to the ",
                   tags$strong("Snapshots"), " tab → pick the same snapshot → ",
                   "Files tab → click ", tags$strong("Reload from disk"),
                   " to see the freshly-saved content."),
          tags$li("When all edits are complete, go to ",
                   tags$strong("Manage snapshots"), " → select the snapshot → ",
                   tags$strong("→ pending"), " to lock it for review."),
          tags$li("On the ", tags$strong("Approval queue"),
                   " → Pending → Snapshots tab, a reviewer (typically not the ",
                   "author) approves or rejects with a required reason.")
        )
      )
    )),
    fluidRow(column(12,
      div(style = "margin-bottom: 0.5em;",
        actionButton(ns("refresh"), "Refresh",
                      icon = icon("rotate"),
                      class = "btn-sm btn-outline-secondary")
      ),
      selectInput(ns("snap_pick"), "Snapshot",
                   choices = c("(pick one)" = ""),
                   selected = "")
    )),
    uiOutput(ns("body"))
  )
}


mod_snapshot_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    snaps_root <- function() {
      proj_root <- getOption("ifrs9.project_root", getwd())
      getOption("ifrs9.snapshots_dir",
                file.path(proj_root, "config_snapshots"))
    }

    snaps <- reactive({
      refresh()
      tryCatch(list_snapshots(snaps_root()),
                error = function(e) NULL)
    })

    observeEvent(input$refresh, refresh(refresh() + 1))

    # Update snapshot dropdown whenever the list changes
    observe({
      s <- snaps()
      choices <- c("(pick one)" = "")
      if (!is.null(s) && nrow(s) > 0) {
        rank <- c(draft = 0, pending = 1, approved = 2, archived = 3)
        ord  <- order(rank[s$status %||% rep("draft", nrow(s))], s$created_at)
        s2 <- s[ord, , drop = FALSE]
        labels <- sprintf("%s [%s]", s2$label, s2$status %||% "?")
        choices <- c(choices, setNames(s2$label, labels))
      }
      updateSelectInput(session, "snap_pick", choices = choices,
                         selected = isolate(input$snap_pick) %||% "")
    })

    selected_meta <- reactive({
      refresh()
      pick <- input$snap_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      tryCatch(read_snapshot_metadata(pick, snaps_root()),
                error = function(e) NULL)
    })

    # =========== TOP-LEVEL BODY ROUTER ==============================
    output$body <- renderUI({
      m <- selected_meta()
      if (is.null(m)) {
        return(p(class = "small-muted",
                  "Pick a snapshot to start editing."))
      }
      if (isTRUE(m$status == "draft")) return(.editor_body(ns, m))
      .non_draft_body(ns, m)
    })


    # =========== NON-DRAFT: INFO + CLONE FORM =======================
    output$clone_form <- renderUI({
      m <- selected_meta()
      if (is.null(m)) return(NULL)
      tagList(
        h5("Clone to a new draft"),
        p(class = "small-muted",
          "Create a new ", tags$em("draft"), " snapshot starting from ",
          "this one's contents. The new draft inherits everything but is ",
          "free to edit. The lineage chain ", tags$code("parent"),
          " is preserved."),
        textInput(ns("clone_label"), "New label",
                   placeholder = "e.g. 2026-Q2-draft-1"),
        textAreaInput(ns("clone_desc"), "Description",
                       rows = 2,
                       placeholder = "What's the goal of this revision?"),
        actionButton(ns("do_clone"), "Clone to new draft",
                      icon = icon("code-branch"),
                      class = "btn-primary")
      )
    })

    observeEvent(input$do_clone, {
      m <- selected_meta(); if (is.null(m)) return()
      new_label <- trimws(input$clone_label %||% "")
      desc <- trimws(input$clone_desc %||% "")
      if (!nzchar(new_label)) {
        showNotification("New label is required.", type = "warning"); return()
      }
      if (!grepl("^[A-Za-z0-9._-]+$", new_label)) {
        showNotification("Label must be [A-Za-z0-9._-]+",
                          type = "warning"); return()
      }
      if (!nzchar(desc)) {
        showNotification("Description is required.", type = "warning"); return()
      }
      result <- tryCatch(
        clone_snapshot(
          source_label = m$label,
          new_label = new_label,
          description = desc,
          created_by = Sys.info()[["user"]] %||% "unknown",
          snapshots_root = snaps_root()
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Clone failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(sprintf("Cloned %s -> %s (draft)",
                                   m$label, new_label),
                          type = "message")
        updateTextInput(session, "clone_label", value = "")
        updateTextAreaInput(session, "clone_desc", value = "")
        refresh(refresh() + 1)
        # Auto-pick the new draft
        updateSelectInput(session, "snap_pick", selected = new_label)
      }
    })


    # =========== DRAFT: FILE EDITOR =================================
    files_choice <- reactive({
      m <- selected_meta(); if (is.null(m)) return(NULL)
      eligible <- editable_snapshot_files()
      sp <- snapshot_paths(m$label, snaps_root())
      eligible$exists <- vapply(eligible$relpath,
                                  function(rp) file.exists(file.path(sp$snapshot_dir, rp)),
                                  logical(1))
      eligible
    })

    observe({
      ec <- files_choice()
      if (is.null(ec)) return()
      labels <- ifelse(ec$exists,
                        sprintf("%s — %s", ec$relpath, ec$description),
                        sprintf("%s — %s (file not present in snapshot)",
                                ec$relpath, ec$description))
      choices <- setNames(ec$relpath, labels)
      updateSelectInput(session, "file_pick",
                         choices = c("(pick a file)" = "", choices),
                         selected = isolate(input$file_pick) %||% "")
    })

    file_meta <- reactive({
      m <- selected_meta(); if (is.null(m)) return(NULL)
      pick <- input$file_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      ec <- editable_snapshot_files()
      hit <- which(ec$relpath == pick)
      if (length(hit) != 1) return(NULL)
      sp <- snapshot_paths(m$label, snaps_root())
      list(
        snapshot = m$label,
        relpath  = pick,
        kind     = ec$kind[hit],
        full     = file.path(sp$snapshot_dir, pick)
      )
    })

    output$editor_panel <- renderUI({
      fm <- file_meta()
      if (is.null(fm)) {
        return(p(class = "small-muted",
                  "Pick a file to edit. YAML files open in a text editor; ",
                  "CSV files open as an editable table."))
      }
      if (!file.exists(fm$full)) {
        return(p(class = "small-muted",
                  sprintf("File does not exist in this snapshot: %s",
                          fm$relpath)))
      }
      if (fm$kind == "yaml") {
        text <- paste(readLines(fm$full, warn = FALSE), collapse = "\n")
        editor_widget <- if (requireNamespace("shinyAce", quietly = TRUE)) {
          # Real code editor: line numbers, syntax highlighting, jump-to-line.
          shinyAce::aceEditor(
            outputId  = ns("yaml_text"),
            value     = text,
            mode      = "yaml",
            theme     = "github",
            height    = "550px",
            fontSize  = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = TRUE,
            autoScrollEditorIntoView = TRUE,
            debounce  = 200
          )
        } else {
          # Fallback for installations without shinyAce. Functional but
          # no line numbers — recommend the install in the helper text.
          textAreaInput(ns("yaml_text"), label = NULL, value = text,
                         rows = 25, width = "100%")
        }
        tagList(
          tags$h6(tags$code(fm$relpath)),
          if (!requireNamespace("shinyAce", quietly = TRUE)) {
            p(class = "small-muted",
              tags$em("Install the "), tags$code("shinyAce"),
              tags$em(" package to get line numbers and YAML syntax highlighting in the editor."))
          },
          editor_widget,
          tags$br(),
          actionButton(ns("yaml_save"), "Save (validate + write)",
                        icon = icon("floppy-disk"),
                        class = "btn-primary"),
          tags$span(style = "margin-left: 0.5em;",
            actionButton(ns("yaml_revert"), "Discard changes",
                          icon = icon("arrow-rotate-left"),
                          class = "btn-outline-secondary"))
        )
      } else if (fm$kind == "csv") {
        tagList(
          tags$h6(tags$code(fm$relpath)),
          p(class = "small-muted",
            "Double-click a cell to edit. Click ", tags$strong("Save"),
            " to commit. Type changes are inferred from the existing ",
            "column type — keep numbers numeric, dates as ISO strings."),
          DT::DTOutput(ns("csv_table")),
          tags$br(),
          actionButton(ns("csv_save"), "Save",
                        icon = icon("floppy-disk"),
                        class = "btn-primary"),
          tags$span(style = "margin-left: 0.5em;",
            actionButton(ns("csv_revert"), "Discard changes",
                          icon = icon("arrow-rotate-left"),
                          class = "btn-outline-secondary"))
        )
      } else {
        p(class = "small-muted",
          sprintf("Unknown file kind '%s'", fm$kind))
      }
    })

    # ---- YAML save / revert ---------------------------------------
    observeEvent(input$yaml_save, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "yaml") return()
      text <- input$yaml_text %||% ""
      result <- tryCatch(
        save_snapshot_yaml(label = fm$snapshot, relpath = fm$relpath,
                            text = text, validate = TRUE,
                            snapshots_root = snaps_root()),
        error = function(e) list(ok = FALSE,
                                  message = conditionMessage(e))
      )
      if (isTRUE(result$ok)) {
        showNotification(
          sprintf("Saved: %s. To verify, open the Snapshots tab and click 'Reload from disk'.",
                   fm$relpath),
          type = "message", duration = 6)
        return()
      }
      # ---- Save failed: render an actionable error modal --------------
      err_msg <- result$message %||% "(no message)"
      ctx_block <- .yaml_error_context(text, err_msg)
      showModal(modalDialog(
        title = "YAML save failed",
        tags$p("The YAML did not parse. The file on disk was NOT changed."),
        tags$p(tags$strong("Error message:")),
        tags$pre(style = "white-space: pre-wrap; background: #f8d7da; padding: 0.6em;",
                  err_msg),
        if (!is.null(ctx_block)) {
          tagList(
            tags$p(tags$strong(sprintf("Context around line %d (▶ marks the line):",
                                          ctx_block$line))),
            tags$pre(style = "background: #fff3cd; padding: 0.6em; font-size: 0.85em; line-height: 1.4;",
                      ctx_block$snippet)
          )
        } else {
          tags$p(class = "small-muted",
                  "(Could not parse a line number from the error.)")
        },
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$yaml_revert, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "yaml") return()
      text <- paste(readLines(fm$full, warn = FALSE), collapse = "\n")
      if (requireNamespace("shinyAce", quietly = TRUE)) {
        shinyAce::updateAceEditor(session, "yaml_text", value = text)
      } else {
        updateTextAreaInput(session, "yaml_text", value = text)
      }
      showNotification("Reloaded from disk.", type = "message", duration = 3)
    })

    # ---- CSV editor ------------------------------------------------
    # Buffers for CSV state. csv_buffer holds the editable data; the
    # comment_header buffer holds any leading `#` provenance lines so
    # we can write them back on save (otherwise saving would silently
    # strip the file's metadata block).
    csv_buffer <- reactiveVal(NULL)
    csv_comment_header <- reactiveVal(character())

    # When the picked file changes, reload BOTH buffers from disk
    observeEvent(file_meta(), {
      fm <- file_meta()
      if (is.null(fm) || fm$kind != "csv") {
        csv_buffer(NULL); csv_comment_header(character()); return()
      }
      result <- tryCatch(
        read_static_csv_with_header(fm$full),
        error = function(e) NULL
      )
      if (is.null(result)) {
        csv_buffer(NULL); csv_comment_header(character())
        return()
      }
      csv_buffer(result$data)
      csv_comment_header(result$comment_header %||% character())
    }, ignoreNULL = FALSE)

    output$csv_table <- DT::renderDT({
      df <- csv_buffer()
      if (is.null(df)) return(NULL)
      DT::datatable(
        df,
        editable = list(target = "cell"),
        rownames = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    # Capture cell edits into the buffer
    observeEvent(input$csv_table_cell_edit, {
      info <- input$csv_table_cell_edit
      df <- csv_buffer()
      if (is.null(df)) return()
      r <- info$row; c <- info$col + 1
      old_val <- df[r, c]
      new_val <- info$value
      if (is.numeric(old_val)) {
        coerced <- suppressWarnings(as.numeric(new_val))
        if (is.na(coerced) && nzchar(new_val)) {
          showNotification(sprintf("Cell expects numeric; got '%s'", new_val),
                            type = "warning")
          return()
        }
        new_val <- coerced
      } else if (is.integer(old_val)) {
        coerced <- suppressWarnings(as.integer(new_val))
        if (is.na(coerced) && nzchar(new_val)) {
          showNotification(sprintf("Cell expects integer; got '%s'", new_val),
                            type = "warning")
          return()
        }
        new_val <- coerced
      }
      df[r, c] <- new_val
      csv_buffer(df)
    })

    observeEvent(input$csv_save, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "csv") return()
      df <- csv_buffer()
      if (is.null(df)) {
        showNotification("Nothing to save.", type = "warning"); return()
      }
      result <- tryCatch(
        save_snapshot_csv(label = fm$snapshot, relpath = fm$relpath,
                            df = df,
                            comment_header = csv_comment_header(),
                            snapshots_root = snaps_root()),
        error = function(e) list(ok = FALSE,
                                  message = conditionMessage(e))
      )
      if (isTRUE(result$ok)) {
        showNotification(
          sprintf("Saved: %s (%d rows). To verify, open the Snapshots tab and click 'Reload from disk'.",
                   fm$relpath, nrow(df)),
          type = "message", duration = 6)
      } else {
        showNotification(paste("Save failed:", result$message),
                          type = "error", duration = 12)
      }
    })

    observeEvent(input$csv_revert, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "csv") return()
      result <- tryCatch(
        read_static_csv_with_header(fm$full),
        error = function(e) NULL
      )
      if (is.null(result)) {
        csv_buffer(NULL); csv_comment_header(character())
      } else {
        csv_buffer(result$data)
        csv_comment_header(result$comment_header %||% character())
      }
      showNotification("Reloaded from disk.", type = "message", duration = 3)
    })
  })
}


# ============================================================================
# UI fragments per snapshot status. Inline functions (not exported).
# ============================================================================

.editor_body <- function(ns, m) {
  tagList(
    div(class = "alert alert-info", style = "margin-bottom: 1em;",
        sprintf("Editing draft snapshot: %s — ", m$label),
        tags$em(m$description %||% "")),
    selectInput(ns("file_pick"), "File to edit",
                 choices = c("(pick a file)" = ""),
                 width = "650px"),
    uiOutput(ns("editor_panel"))
  )
}

.non_draft_body <- function(ns, m) {
  tagList(
    div(class = "alert alert-warning", style = "margin-bottom: 1em;",
        sprintf("This snapshot is in status '%s'. ", m$status),
        "Edits are locked. Clone it to a new draft below to make a revision."),
    uiOutput(ns("clone_form"))
  )
}


#' Pull line/column from a yaml::yaml.load error message and build a
#' multi-line context snippet around it.
#'
#' libyaml emits errors of the form:
#'   "Parser error: while parsing a block collection at line N, column M
#'    did not find expected '-' indicator at line P, column Q"
#'
#' We pull the LAST line/column pair (the actual offending location)
#' and return a 5-line window with a marker on the offending line.
#'
#' @param text     the whole text the user is trying to save
#' @param err_msg  conditionMessage from yaml::yaml.load
#' @return list(line=int, snippet=character) or NULL if no line found
.yaml_error_context <- function(text, err_msg) {
  if (is.null(text) || !nzchar(text) || is.null(err_msg)) return(NULL)
  # libyaml messages can mention up to two locations; prefer the last
  # ("at line P, column Q") since that's where the actual mistake is.
  matches <- gregexpr("line\\s+(\\d+),\\s*column\\s+(\\d+)",
                      err_msg, perl = TRUE)[[1]]
  if (matches[1] == -1) return(NULL)
  starts  <- as.numeric(matches)
  lengths <- attr(matches, "match.length")
  # Take the last match
  i <- length(starts)
  m <- regmatches(err_msg,
                  regexpr("line\\s+(\\d+),\\s*column\\s+(\\d+)",
                          substr(err_msg, starts[i],
                                  starts[i] + lengths[i] - 1),
                          perl = TRUE))
  nums <- regmatches(m, regexec("line\\s+(\\d+),\\s*column\\s+(\\d+)", m))[[1]]
  if (length(nums) < 3) return(NULL)
  err_line <- as.integer(nums[2])
  if (is.na(err_line)) return(NULL)

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return(NULL)
  lo <- max(1L, err_line - 2L)
  hi <- min(length(lines), err_line + 2L)
  width <- nchar(as.character(hi))
  out <- vapply(lo:hi, function(n) {
    marker <- if (n == err_line) "▶" else " "
    sprintf("%s %*d  %s", marker, width, n, lines[n])
  }, character(1))
  list(line = err_line, snippet = paste(out, collapse = "\n"))
}
