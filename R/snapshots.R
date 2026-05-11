# =============================================================================
# snapshots.R
#
# Snapshot management: a snapshot is a FROZEN bundle of every config file
# and every static reference CSV at a point in time. One snapshot captures
# the full state needed to reproduce a run, INCLUDING the active model
# definitions inside models.yml.
#
# Code version is tracked SEPARATELY (see code_version.R + manifest.json):
#   (snapshot_id, code_sha) together form the full reproducibility key.
#
# Snapshot lifecycle:
#   draft     -> mutable; user edits configs/statics here
#   pending   -> frozen, awaiting approval
#   approved  -> frozen, in production. Cannot be modified.
#   archived  -> superseded by a newer approved snapshot
#
# Filesystem layout under config_snapshots/<label>/:
#   snapshot.yml      metadata (id, label, created_by, created_at,
#                     status, parent, description, code_sha_at_creation)
#   changelog.md      human-readable summary of changes vs parent
#   changes.json      machine-readable diff vs parent (file-level)
#   config/           frozen copy of project root config/
#   static/           frozen copy of project root data-raw/static/
#
# Functions:
#   list_snapshots()
#   create_snapshot(label, description, created_by, parent = "<latest_approved>")
#   read_snapshot_metadata(label_or_path)
#   snapshot_paths(label_or_path)         -> list of resolved paths for run_etl()
#   promote_snapshot(label, status, approved_by, reason)
#   diff_snapshots(a, b)                  -> structured diff
# =============================================================================


SNAPSHOT_STATUSES <- c(
  "draft",          # editable; creator iterating
  "tested",         # creator has self-locked for impact testing; not editable
  "pending_final",  # submitted to final approver (different user)
  "approved",       # signed off by final approver; usable for OFFICIAL runs
  "archived",       # superseded
  "rejected",       # final approver rejected; clone to draft to fix
  # Legacy alias kept for older snapshots:
  "pending"
)


# Resolve the snapshots root directory from (in priority order):
#  1. Explicit argument (if non-NULL and not the bare default literal)
#  2. getOption("ifrs9.snapshots_dir") — set by app.R at startup to an
#     absolute path so callers don't have to depend on getwd()
#  3. Literal "config_snapshots" relative to whatever getwd() is
#
# The middle option matters in Shiny: shiny::runApp("app") makes
# getwd() the app/ subdirectory, so a literal "config_snapshots"
# resolves to app/config_snapshots which doesn't exist. The option
# always wins when set.
.resolve_snapshots_root <- function(snapshots_root = "config_snapshots") {
  if (!identical(snapshots_root, "config_snapshots")) {
    return(snapshots_root)  # caller supplied an explicit path
  }
  getOption("ifrs9.snapshots_dir", snapshots_root)
}


#' Resolve a snapshot label (or full path) to its absolute directory.
#'
#' @param label_or_path  short label like "2026-Q1-final" OR a full path
#' @param snapshots_root default base directory for snapshots
.snapshot_dir <- function(label_or_path,
                          snapshots_root = "config_snapshots") {
  snapshots_root <- .resolve_snapshots_root(snapshots_root)
  if (dir.exists(label_or_path)) {
    return(normalizePath(label_or_path, mustWork = TRUE))
  }
  candidate <- file.path(snapshots_root, label_or_path)
  if (!dir.exists(candidate)) {
    stop(sprintf("Snapshot not found: %s (looked at '%s')",
                 label_or_path, candidate))
  }
  normalizePath(candidate, mustWork = TRUE)
}


#' Read snapshot.yml metadata from a snapshot directory.
read_snapshot_metadata <- function(label_or_path,
                                    snapshots_root = "config_snapshots") {
  d <- .snapshot_dir(label_or_path, snapshots_root)
  meta_path <- file.path(d, "snapshot.yml")
  if (!file.exists(meta_path)) {
    stop("snapshot.yml not found in: ", d)
  }
  meta <- yaml::read_yaml(meta_path)
  meta$path <- d
  meta
}


#' Resolve the runtime paths for a snapshot — what run_etl() needs.
#'
#' Returns a list with: config_yml, variable_dictionary, models,
#' model_inputs, static_dir. All paths are absolute and live inside the
#' snapshot directory.
snapshot_paths <- function(label_or_path,
                            snapshots_root = "config_snapshots") {
  d <- .snapshot_dir(label_or_path, snapshots_root)
  list(
    snapshot_dir         = d,
    config_yml           = file.path(d, "config", "config.yml"),
    variable_dictionary  = file.path(d, "config", "variable_dictionary.yml"),
    models               = file.path(d, "config", "models.yml"),
    model_inputs         = file.path(d, "config", "model_inputs.yml"),
    static_dir           = file.path(d, "static")
  )
}


#' List all snapshots with metadata.
#'
#' @return tibble with one row per snapshot.
list_snapshots <- function(snapshots_root = "config_snapshots") {
  snapshots_root <- .resolve_snapshots_root(snapshots_root)
  if (!dir.exists(snapshots_root)) {
    return(tibble::tibble(
      label = character(), status = character(),
      created_at = character(), created_by = character(),
      description = character(), parent = character(),
      code_sha_at_creation = character()
    ))
  }
  labels <- list.dirs(snapshots_root, recursive = FALSE, full.names = FALSE)
  rows <- lapply(labels, function(lbl) {
    meta <- tryCatch(read_snapshot_metadata(lbl, snapshots_root),
                     error = function(e) NULL)
    if (is.null(meta)) return(NULL)
    tibble::tibble(
      label                = meta$label %||% lbl,
      status               = meta$status %||% NA_character_,
      created_at           = meta$created_at %||% NA_character_,
      created_by           = meta$created_by %||% NA_character_,
      description          = meta$description %||% NA_character_,
      parent               = meta$parent %||% NA_character_,
      code_sha_at_creation = meta$code_sha_at_creation %||% NA_character_
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(tibble::tibble(
      label = character(), status = character(),
      created_at = character(), created_by = character(),
      description = character(), parent = character(),
      code_sha_at_creation = character()
    ))
  }
  do.call(rbind, rows)
}


#' Create a new snapshot by copying live config/ + data-raw/static/.
#'
#' @param label             unique identifier, e.g. "2026-Q1-draft-1"
#' @param description       human-readable description
#' @param created_by        username
#' @param parent            optional parent snapshot label (for lineage)
#' @param config_dir        source config directory (live)
#' @param static_dir        source static directory (live)
#' @param snapshots_root    where to write the snapshot
#' @param code_sha          current code SHA (NULL = auto-detect)
#' @return absolute path to the new snapshot directory
create_snapshot <- function(label,
                             description,
                             created_by,
                             parent = NULL,
                             config_dir = "config",
                             static_dir = "data-raw/static",
                             run_config_path = NULL,
                             snapshots_root = "config_snapshots",
                             code_sha = NULL) {
  if (!nzchar(label)) stop("label cannot be empty")
  if (!grepl("^[A-Za-z0-9._-]+$", label)) {
    stop("label must contain only [A-Za-z0-9._-] characters")
  }
  snapshots_root <- .resolve_snapshots_root(snapshots_root)
  out_dir <- file.path(snapshots_root, label)
  if (dir.exists(out_dir)) {
    stop(sprintf("Snapshot already exists: %s", out_dir))
  }
  if (!dir.exists(config_dir)) stop("config_dir not found: ", config_dir)
  if (!dir.exists(static_dir)) stop("static_dir not found: ", static_dir)

  # Resolve the top-level run config (config.yml). It lives at the
  # project root, not inside config/. If the caller doesn't tell us
  # where it is, look in the conventional places: ifrs9.config_path
  # option, then <project_root>/config.yml, then ./config.yml.
  if (is.null(run_config_path)) {
    proj_root <- getOption("ifrs9.project_root", getwd())
    candidates <- c(
      getOption("ifrs9.config_path", NA_character_),
      file.path(proj_root, "config.yml"),
      "config.yml"
    )
    candidates <- candidates[!is.na(candidates)]
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0) {
      stop("Could not locate the live run config (config.yml). Pass ",
           "run_config_path explicitly. Looked at: ",
           paste(candidates, collapse = ", "))
    }
    run_config_path <- hit[1]
  } else if (!file.exists(run_config_path)) {
    stop("run_config_path not found: ", run_config_path)
  }

  dir.create(out_dir, recursive = TRUE)
  dir.create(file.path(out_dir, "config"), recursive = TRUE)
  dir.create(file.path(out_dir, "static"), recursive = TRUE)

  # Copy contents (R has no built-in dir-copy that's portable; iterate)
  .copy_tree(config_dir, file.path(out_dir, "config"))
  .copy_tree(static_dir, file.path(out_dir, "static"))

  # Copy the top-level run config alongside the model/dictionary YAMLs.
  # CRITICAL: the live config.yml has paths like
  # `variable_dictionary: config/variable_dictionary.yml` — correct for
  # the LIVE layout where config.yml sits at the project root with the
  # config/ folder as a sibling. Inside the snapshot, config.yml is
  # itself INSIDE config/, so those relative paths would resolve to
  # `<snapshot>/config/config/variable_dictionary.yml` (double config/)
  # and break. We rewrite the relevant entries so they resolve as
  # siblings of the embedded config.yml.
  .rewrite_snapshot_config <- function(src, dst) {
    cfg <- yaml::read_yaml(src)
    rewrite_keys <- c("variable_dictionary", "models", "model_inputs")
    if (!is.null(cfg$paths) && is.list(cfg$paths)) {
      for (k in rewrite_keys) {
        v <- cfg$paths[[k]]
        if (!is.null(v) && is.character(v) && length(v) == 1 && nzchar(v)) {
          # Strip a leading "config/" since variable_dictionary.yml etc.
          # are now siblings of config.yml inside the snapshot's config/
          # folder.
          cfg$paths[[k]] <- sub("^config/", "", v)
        }
      }
    }
    writeLines(yaml::as.yaml(cfg), dst)
  }
  .rewrite_snapshot_config(run_config_path,
                            file.path(out_dir, "config", "config.yml"))

  if (is.null(code_sha)) {
    code_sha <- tryCatch(get_current_code_sha(),
                          error = function(e) NA_character_)
  }

  meta <- list(
    schema_version       = "1.0",
    label                = label,
    status               = "draft",
    description          = description,
    created_by           = created_by,
    created_at           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    parent               = parent,
    code_sha_at_creation = code_sha,
    approved_by          = NULL,
    approved_at          = NULL,
    approval_reason      = NULL
  )
  writeLines(yaml::as.yaml(meta), file.path(out_dir, "snapshot.yml"))

  # Audit log
  audit_event(list(
    event       = "snapshot_create",
    snapshot    = label,
    parent      = parent,
    user        = created_by,
    description = description
  ))

  invisible(normalizePath(out_dir))
}


#' Move a snapshot from one status to another.
#'
#' Allowed transitions:
#'   draft    -> pending
#'   pending  -> approved
#'   pending  -> draft     (reject — back to editing)
#'   approved -> archived  (superseded by a new approved snapshot)
#'
#' @param label          snapshot label
#' @param status         target status (one of SNAPSHOT_STATUSES)
#' @param approved_by    required when transitioning to approved
#' @param reason         human reason for the transition
#' Promote a snapshot through its lifecycle.
#'
#' New (H18) state machine:
#'   draft ──► tested ──► pending_final ──► approved
#'     │         │            │
#'     ▼         ▼            ▼
#'  (delete)  (back-to-draft)  rejected
#'                                │
#'                                ▼
#'                              (clone to new draft)
#'   approved ──► archived
#'
#' Separation of duties (config: approval.enforce_separation_of_duties):
#'   When TRUE, the user who created the snapshot CANNOT also be the
#'   final approver. The creator CAN mark their own snapshot as
#'   `tested` (self-lock) and CAN submit it to `pending_final` —
#'   only the final-approval step is gated.
#'
#' Each significant transition appends to a `transitions` list inside
#' snapshot.yml so the full audit trail is preserved (creator, tester,
#' submitter, approver — all with timestamp and reason).
#'
#' Backward compat: legacy "pending" status maps to "pending_final".
promote_snapshot <- function(label, status,
                              approved_by = NULL, reason = "",
                              snapshots_root = "config_snapshots") {
  # Map legacy alias to canonical status
  if (status == "pending") status <- "pending_final"
  if (!status %in% SNAPSHOT_STATUSES) {
    stop("Unknown status: ", status,
         ". Allowed: ", paste(SNAPSHOT_STATUSES, collapse = ", "))
  }
  meta <- read_snapshot_metadata(label, snapshots_root)
  current <- meta$status %||% "draft"
  # Treat legacy "pending" as if it's "pending_final" for transition logic
  if (current == "pending") current <- "pending_final"

  allowed <- list(
    draft         = c("tested"),
    tested        = c("pending_final", "draft"),    # back to draft if more edits needed
    pending_final = c("approved", "rejected", "tested"),  # approve, reject, or send back
    approved      = c("archived"),
    rejected      = c("draft"),
    archived      = c()
  )
  if (!status %in% (allowed[[current]] %||% character())) {
    stop(sprintf("Illegal status transition: %s -> %s (allowed from %s: %s)",
                 current, status, current,
                 paste(allowed[[current]] %||% "(none)", collapse = ", ")))
  }
  if (status == "approved" && (is.null(approved_by) || !nzchar(approved_by))) {
    stop("approved_by is required when promoting to approved")
  }
  if (status %in% c("tested", "pending_final", "rejected", "draft", "archived") &&
      (is.null(approved_by) || !nzchar(approved_by))) {
    stop("user (approved_by) is required when transitioning a snapshot")
  }
  if (is.null(reason) || !nzchar(reason)) {
    stop("reason is required when transitioning a snapshot")
  }

  # Separation of duties: the user who CREATED the snapshot cannot also
  # be the final approver. Other transitions (self-lock, submit) the
  # creator can do.
  if (status == "approved") {
    cfg <- tryCatch(.approval_cfg_for_snapshots(),
                    error = function(e) list(enforce_separation_of_duties = FALSE))
    if (isTRUE(cfg$enforce_separation_of_duties)) {
      creator <- meta$created_by %||% NA_character_
      if (!is.na(creator) && nzchar(creator) &&
          identical(tolower(creator), tolower(as.character(approved_by)))) {
        stop(sprintf(
          "Separation of duties is enforced: user '%s' created this snapshot ",
          approved_by),
          "and cannot also approve it. A different user must approve.")
      }
    }
  }

  meta$status <- status
  trans_record <- list(
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    by = approved_by, from = current, to = status, reason = reason
  )
  meta$transitions <- c(meta$transitions, list(trans_record))

  if (status == "approved") {
    meta$approved_by     <- approved_by
    meta$approved_at     <- trans_record$at
    meta$approval_reason <- reason
  }
  if (status == "tested") {
    meta$tested_by <- approved_by
    meta$tested_at <- trans_record$at
  }

  # Atomic write
  yaml_path <- file.path(meta$path, "snapshot.yml")
  tmp <- paste0(yaml_path, ".tmp")
  writeLines(yaml::as.yaml(meta), tmp)
  file.rename(tmp, yaml_path)

  audit_event(list(
    event       = "snapshot_promote",
    snapshot    = label,
    from_status = current,
    to_status   = status,
    user        = approved_by,
    reason      = reason
  ))

  invisible(meta)
}


# Reads approval config block for snapshot helpers. Mirrors the run-side
# `.approval_cfg()` so both flows respect the same flag.
.approval_cfg_for_snapshots <- function() {
  cfg_path <- getOption("ifrs9.config_path",
                          file.path(getOption("ifrs9.project_root", getwd()),
                                    "config.yml"))
  if (!file.exists(cfg_path)) {
    return(list(enforce_separation_of_duties = FALSE))
  }
  cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
  if (is.null(cfg) || is.null(cfg$approval)) {
    return(list(enforce_separation_of_duties = FALSE))
  }
  list(
    enforce_separation_of_duties =
      isTRUE(cfg$approval$enforce_separation_of_duties)
  )
}


#' Diff two snapshots — returns a list of changes per file.
#'
#' For YAML / CSV files, computes a row-level (CSV) or top-level-key (YAML)
#' diff. For larger files, only reports presence + size + sha256.
#'
#' @return list with $files_added, $files_removed, $files_modified
diff_snapshots <- function(a, b, snapshots_root = "config_snapshots") {
  da <- .snapshot_dir(a, snapshots_root)
  db <- .snapshot_dir(b, snapshots_root)
  fa <- .list_snapshot_files(da)
  fb <- .list_snapshot_files(db)

  added    <- setdiff(fb, fa)
  removed  <- setdiff(fa, fb)
  common   <- intersect(fa, fb)

  modified <- list()
  for (rel in common) {
    pa <- file.path(da, rel)
    pb <- file.path(db, rel)
    sa <- .file_sha256(pa)
    sb <- .file_sha256(pb)
    if (identical(sa, sb)) next
    modified[[length(modified) + 1]] <- list(
      file = rel,
      a_sha = sa, b_sha = sb,
      a_size = file.info(pa)$size, b_size = file.info(pb)$size
    )
  }

  list(
    a              = a,
    b              = b,
    files_added    = added,
    files_removed  = removed,
    files_modified = modified
  )
}


# ---------- internal helpers ----------

#' Recursive directory copy. Replaces files; does not remove extras at dest.
.copy_tree <- function(src, dst) {
  files <- list.files(src, recursive = TRUE, full.names = FALSE)
  for (f in files) {
    src_f <- file.path(src, f)
    dst_f <- file.path(dst, f)
    dir.create(dirname(dst_f), showWarnings = FALSE, recursive = TRUE)
    file.copy(src_f, dst_f, overwrite = TRUE)
  }
}

#' List all files in a snapshot's config/ and static/ subtrees.
.list_snapshot_files <- function(snapshot_dir) {
  out <- character()
  for (sub in c("config", "static")) {
    d <- file.path(snapshot_dir, sub)
    if (!dir.exists(d)) next
    f <- list.files(d, recursive = TRUE, full.names = FALSE)
    out <- c(out, file.path(sub, f))
  }
  out
}

#' SHA-256 of a file (uses digest package if installed, else size+mtime).
.file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = path, algo = "sha256")
  } else {
    info <- file.info(path)
    sprintf("size=%d;mtime=%s",
            info$size, format(info$mtime, "%Y-%m-%dT%H:%M:%S"))
  }
}


# =============================================================================
# H14: Snapshot editing
#
# A snapshot in `draft` status is editable through the Shiny app. Once
# promoted to `pending`, edits are locked — what reviewers see is what
# gets approved. To revise an approved snapshot, the user clones it
# back to a new draft (lineage preserved via `parent`).
#
# Editable file whitelist: the YAML config files plus a small set of
# tabular static-reference files that get tweaked per period (rating
# scale, TTC PD tables, EIR fallback, etc.). The other static files
# are treated as immutable — they encode the bank's portfolio taxonomy
# and shouldn't change between snapshots.
#
# Functions:
#   editable_snapshot_files()    - whitelist for the UI
#   clone_snapshot()             - fork approved/archived -> new draft
#   save_snapshot_yaml()         - validated atomic write of a YAML file
#   save_snapshot_csv()          - atomic write of a CSV file
# =============================================================================


#' Whitelist of files inside a snapshot that the UI can edit.
#'
#' Returned as a tibble with columns:
#'   relpath    : path relative to snapshot root (e.g. "config/config.yml")
#'   kind       : "yaml" or "csv"
#'   description: short label shown in the UI dropdown
editable_snapshot_files <- function() {
  tibble::tibble(
    relpath = c(
      # YAML configs
      "config/config.yml",
      "config/models.yml",
      "config/variable_dictionary.yml",
      "config/model_inputs.yml",
      "config/validation_suppressions.yml",
      # Tabular static-reference files commonly tuned per period
      "static/master_rating_scale.csv",
      "static/ttc_pd_table.csv",
      "static/ttc_pd_table_external.csv",
      "static/product_portfolio_mapping.csv",
      "static/eir_fallback.csv",
      "static/scenario_severity.csv"
    ),
    kind = c(
      "yaml", "yaml", "yaml", "yaml", "yaml",
      "csv",  "csv",  "csv",  "csv",  "csv", "csv"
    ),
    description = c(
      "Run config (paths, gating policy, extract date)",
      "Model registry (rating-type rules, model selection)",
      "MEV variable dictionary (registered macro variables)",
      "Model inputs (scenario weights, MEV forecasts)",
      "Validation suppressions",
      "Master rating scale (21 internal + 21 external)",
      "TTC PD table (internal)",
      "TTC PD table (external)",
      "Product → portfolio mapping",
      "EIR fallback rates",
      "Scenario severity (z-scores)"
    )
  )
}


#' Clone a snapshot into a new draft.
#'
#' Used to fork an approved/archived snapshot for a new revision. The
#' new draft starts as a bit-identical copy of the source's config/ and
#' static/ trees, with snapshot.yml rewritten (status=draft, parent set
#' to source label, fresh created_at/by, code_sha at creation = current
#' code SHA, NOT the source's).
#'
#' @param source_label   label of the snapshot to clone from
#' @param new_label      label for the new draft (must not collide)
#' @param description    description for the new draft
#' @param created_by     username
#' @param snapshots_root parent directory of all snapshots
#' @return new snapshot's metadata as written
clone_snapshot <- function(source_label, new_label, description,
                            created_by,
                            snapshots_root = "config_snapshots") {
  src_dir <- file.path(snapshots_root, source_label)
  if (!dir.exists(src_dir)) {
    stop("Source snapshot does not exist: ", source_label)
  }
  src_meta <- read_snapshot_metadata(source_label, snapshots_root)
  if (is.null(src_meta)) {
    stop("Source snapshot has no readable snapshot.yml: ", source_label)
  }

  dst_dir <- file.path(snapshots_root, new_label)
  if (dir.exists(dst_dir)) {
    stop("Destination snapshot already exists: ", new_label)
  }
  if (!grepl("^[A-Za-z0-9._-]+$", new_label)) {
    stop("Snapshot label must contain only [A-Za-z0-9._-]: ", new_label)
  }

  dir.create(dst_dir, recursive = TRUE)
  for (sub in c("config", "static")) {
    src_sub <- file.path(src_dir, sub)
    if (!dir.exists(src_sub)) next
    dst_sub <- file.path(dst_dir, sub)
    dir.create(dst_sub, recursive = TRUE)
    files <- list.files(src_sub, recursive = TRUE, full.names = TRUE)
    rels  <- list.files(src_sub, recursive = TRUE, full.names = FALSE)
    for (i in seq_along(files)) {
      target <- file.path(dst_sub, rels[i])
      dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
      file.copy(files[i], target, overwrite = FALSE)
    }
  }

  current_sha <- tryCatch(get_current_code_sha(),
                           error = function(e) NA_character_)
  meta <- list(
    schema_version       = "1.0",
    label                = new_label,
    status               = "draft",
    description          = description,
    created_by           = created_by,
    created_at           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    parent               = source_label,
    code_sha_at_creation = current_sha %||% NA_character_,
    cloned_from          = source_label,
    approved_by          = NULL,
    approved_at          = NULL,
    approval_reason      = NULL
  )
  meta_path <- file.path(dst_dir, "snapshot.yml")
  writeLines(yaml::as.yaml(meta), meta_path)

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event   = "snapshot_clone",
      snapshot      = new_label,
      cloned_from   = source_label,
      user          = created_by,
      description   = description
    ))
  }

  invisible(meta)
}


#' Atomic write of a YAML file inside a snapshot.
#'
#' By default the YAML is parsed before writing; an unparseable YAML
#' returns an error without touching the file. Set validate=FALSE only
#' if you really intend to write text that won't parse.
#'
#' Atomic via write-tmp-then-rename, so a partial write can never
#' corrupt the live file.
#'
#' @param label          snapshot label
#' @param relpath        path relative to snapshot root, e.g. "config/config.yml"
#' @param text           string content to write
#' @param validate       if TRUE (default), parse YAML before writing
#' @param snapshots_root parent directory
#' @param edited_by      username (for audit trail)
#' @return list(ok=TRUE/FALSE, message=character)
save_snapshot_yaml <- function(label, relpath, text,
                                validate = TRUE,
                                snapshots_root = "config_snapshots",
                                edited_by = Sys.info()[["user"]] %||% "unknown") {
  meta <- read_snapshot_metadata(label, snapshots_root)
  if (is.null(meta)) {
    return(list(ok = FALSE, message = sprintf("Snapshot not found: %s", label)))
  }
  if (!isTRUE(meta$status == "draft")) {
    return(list(ok = FALSE,
                message = sprintf("Cannot edit snapshot in status '%s'. Edits are only allowed on drafts.",
                                   meta$status %||% "unknown")))
  }

  # Whitelist check
  allowed <- editable_snapshot_files()
  if (!relpath %in% allowed$relpath) {
    return(list(ok = FALSE,
                message = sprintf("File '%s' is not in the editable whitelist.", relpath)))
  }

  if (isTRUE(validate)) {
    parsed <- tryCatch(yaml::yaml.load(text), error = function(e) e)
    if (inherits(parsed, "error")) {
      return(list(ok = FALSE,
                  message = sprintf("YAML parse error: %s",
                                    conditionMessage(parsed))))
    }
  }

  full_path <- file.path(snapshots_root, label, relpath)
  dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(full_path, ".tmp")
  writeLines(text, tmp)
  file.rename(tmp, full_path)

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event    = "snapshot_edit",
      snapshot = label,
      relpath  = relpath,
      user     = edited_by,
      kind     = "yaml"
    ))
  }
  list(ok = TRUE, message = "saved")
}


#' Atomic write of a CSV file inside a snapshot.
#'
#' If `comment_header` is non-empty, those lines are written verbatim
#' at the top of the file before the standard CSV body. This preserves
#' the provenance metadata that lives at the top of files like
#' `product_portfolio_mapping.csv` and `eir_fallback.csv`.
#'
#' @param label          snapshot label
#' @param relpath        path relative to snapshot root
#' @param df             data.frame to write (column names preserved)
#' @param comment_header character vector of `#`-prefixed header lines
#'                        to write verbatim before the CSV body.
#'                        Defaults to empty (no header).
#' @param snapshots_root parent directory
#' @param edited_by      username
#' @return list(ok=TRUE/FALSE, message=character)
save_snapshot_csv <- function(label, relpath, df,
                                comment_header = character(),
                                snapshots_root = "config_snapshots",
                                edited_by = Sys.info()[["user"]] %||% "unknown") {
  meta <- read_snapshot_metadata(label, snapshots_root)
  if (is.null(meta)) {
    return(list(ok = FALSE, message = sprintf("Snapshot not found: %s", label)))
  }
  if (!isTRUE(meta$status == "draft")) {
    return(list(ok = FALSE,
                message = sprintf("Cannot edit snapshot in status '%s'. Edits are only allowed on drafts.",
                                   meta$status %||% "unknown")))
  }
  allowed <- editable_snapshot_files()
  if (!relpath %in% allowed$relpath) {
    return(list(ok = FALSE,
                message = sprintf("File '%s' is not in the editable whitelist.", relpath)))
  }
  if (!is.data.frame(df)) {
    return(list(ok = FALSE, message = "save_snapshot_csv: df must be a data.frame"))
  }

  full_path <- file.path(snapshots_root, label, relpath)
  dir.create(dirname(full_path), showWarnings = FALSE, recursive = TRUE)
  write_static_csv_with_header(full_path, df,
                                 comment_header = comment_header)

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event    = "snapshot_edit",
      snapshot = label,
      relpath  = relpath,
      user     = edited_by,
      kind     = "csv",
      n_rows   = nrow(df)
    ))
  }
  list(ok = TRUE, message = "saved")
}


# =============================================================================
# Static CSV provenance-header preservation
#
# Several static reference files (e.g. product_portfolio_mapping.csv,
# eir_fallback.csv) carry a comment-block header at the top of the
# file documenting variable_id, source workbook cell range, last
# refresh date, and any team-added rows. The header lines start with
# `#` and are not part of the data.
#
# read.csv() does NOT skip `#` lines by default — comment.char is "".
# That causes two failure modes:
#   1. The header rows render as data rows in any UI that calls read.csv()
#   2. The first comment line typically has no commas, so read.csv
#      counts ONE column, and all subsequent data lines collapse into
#      one column.
#
# Helpers below read & write while preserving the comment header.
# =============================================================================


#' Read a static CSV, returning both the comment header and the data.
#'
#' @param path Full path to the CSV file
#' @return list with:
#'   comment_header : character vector of leading comment/blank lines
#'   data           : data.frame parsed by read.csv(comment.char="#")
read_static_csv_with_header <- function(path) {
  if (!file.exists(path)) {
    return(list(comment_header = character(),
                data = data.frame()))
  }
  lines <- readLines(path, warn = FALSE)
  # Walk leading lines: keep `#` comments and blank lines as the header
  # block. Stop at the first non-comment, non-blank line.
  i <- 1L
  while (i <= length(lines) &&
         (grepl("^\\s*#", lines[i]) || !nzchar(trimws(lines[i])))) {
    i <- i + 1L
  }
  header <- if (i > 1L) lines[1:(i-1L)] else character()
  data <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE,
                     check.names = FALSE,
                     comment.char = "#"),
    error = function(e) data.frame()
  )
  list(comment_header = header, data = data)
}


#' Write a CSV with a preserved comment header.
#'
#' Atomic via write-tmp-then-rename. Writes the header lines verbatim,
#' then appends standard CSV from `df`.
write_static_csv_with_header <- function(path, df,
                                          comment_header = character()) {
  tmp <- paste0(path, ".tmp")
  if (length(comment_header) > 0) {
    writeLines(comment_header, tmp)
    utils::write.table(df, file = tmp, sep = ",",
                        row.names = FALSE, col.names = TRUE,
                        append = TRUE, qmethod = "double", na = "")
  } else {
    utils::write.csv(df, tmp, row.names = FALSE, na = "")
  }
  file.rename(tmp, path)
  invisible(path)
}
