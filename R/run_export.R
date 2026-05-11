# =============================================================================
# run_export.R
#
# Build a single zip "run package" containing everything needed to
# reproduce, audit, or hand off a completed run:
#
#   - The 18 standard output CSVs
#   - The inputs actually used by the run (optional, default included)
#   - The frozen snapshot if one was used (config + static)
#   - All run reports (validation, reconciliation, input_source, run_status)
#   - Override files (if any were applied)
#   - The run's manifest.json
#   - A code_version.txt with the git SHA + dirty flag captured at run time
#   - A README.txt that names every section and points reviewers at the
#     audit trail
#
# Public API:
#   build_run_export(run_path, dest_zip, include_inputs=TRUE) -> list
#
# Returns a list with keys:
#   ok         logical
#   path       the dest_zip on success, or NA on failure
#   message    short status string
#   contents   character vector of relative paths inside the zip
#
# Designed to be called from a Shiny downloadHandler (which writes to
# a tempfile path) but works equally well from a script.
# =============================================================================


#' Build a run-export zip.
#'
#' @param run_path        path to a run directory (e.g. runs/<run_id>)
#' @param dest_zip        full path to write the zip to (will be overwritten)
#' @param include_inputs  if TRUE, copy the input files used by the run
#'                         into inputs/ in the bundle. Default TRUE.
#'                         Set FALSE to produce a smaller bundle that
#'                         still has outputs + reports + manifest.
#' @return list(ok, path, message, contents)
build_run_export <- function(run_path, dest_zip,
                              include_inputs = TRUE) {
  if (!dir.exists(run_path)) {
    return(list(ok = FALSE, path = NA_character_,
                message = sprintf("Run path does not exist: %s", run_path),
                contents = character()))
  }
  run_id <- basename(run_path)

  # Stage everything in a temp dir so we can build a clean tree before
  # zipping. The zip's top-level folder name = run_id, so unzipping
  # gives a self-contained `<run_id>/` folder.
  staging <- file.path(tempdir(),
                        paste0("ifrs9_export_",
                               format(Sys.time(), "%Y%m%d_%H%M%S_"),
                               sample.int(1e6, 1)))
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  bundle_root <- file.path(staging, run_id)
  dir.create(bundle_root)

  # ---- copy each section ---------------------------------------------
  contents <- character()

  # 1. manifest
  src_manifest <- file.path(run_path, "manifest.json")
  if (file.exists(src_manifest)) {
    file.copy(src_manifest, file.path(bundle_root, "manifest.json"))
    contents <- c(contents, "manifest.json")
  }

  # 2. outputs/ (the 18 CSVs)
  src_out <- file.path(run_path, "Output")
  if (dir.exists(src_out)) {
    dst_out <- file.path(bundle_root, "outputs")
    dir.create(dst_out)
    files <- list.files(src_out, full.names = TRUE)
    for (f in files) file.copy(f, file.path(dst_out, basename(f)))
    contents <- c(contents, paste0("outputs/", basename(files)))
  }

  # 3. reports/ (validation, recon, input_source, run_status)
  src_reports <- file.path(run_path, "reports")
  if (dir.exists(src_reports)) {
    dst_reports <- file.path(bundle_root, "reports")
    dir.create(dst_reports)
    files <- list.files(src_reports, full.names = TRUE, recursive = TRUE)
    rels  <- list.files(src_reports, full.names = FALSE, recursive = TRUE)
    for (i in seq_along(files)) {
      target <- file.path(dst_reports, rels[i])
      dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
      file.copy(files[i], target)
    }
    contents <- c(contents, paste0("reports/", rels))
  }

  # 4. overrides/ (per-run override CSVs, if any)
  src_ov <- file.path(run_path, "overrides")
  if (dir.exists(src_ov)) {
    dst_ov <- file.path(bundle_root, "overrides")
    dir.create(dst_ov)
    files <- list.files(src_ov, full.names = TRUE)
    for (f in files) file.copy(f, file.path(dst_ov, basename(f)))
    contents <- c(contents, paste0("overrides/", basename(files)))
  }

  # 5. inputs/ (12 files, if requested AND we can locate them)
  inputs_note <- character()
  if (isTRUE(include_inputs)) {
    src_input_dir <- .resolve_input_dir_for_run(run_path)
    if (!is.null(src_input_dir) && dir.exists(src_input_dir)) {
      dst_inputs <- file.path(bundle_root, "inputs")
      dir.create(dst_inputs)
      files <- list.files(src_input_dir, full.names = TRUE,
                           recursive = TRUE)
      rels  <- list.files(src_input_dir, full.names = FALSE,
                           recursive = TRUE)
      for (i in seq_along(files)) {
        target <- file.path(dst_inputs, rels[i])
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(files[i], target)
      }
      contents <- c(contents, paste0("inputs/", rels))
    } else {
      inputs_note <- c(inputs_note,
        sprintf("(inputs/ not included: source path '%s' is unavailable)",
                src_input_dir %||% "(unknown)"))
    }
  } else {
    inputs_note <- "(inputs/ omitted by user request)"
  }

  # 6. config_used/ — frozen at phase 1 time. Contains config/ and
  #    static/ subfolders plus a config_used.yml marker that says
  #    whether this came from a snapshot or live config.
  #
  #    For runs created BEFORE this freeze was added (any pre-H16b run),
  #    config_used/ won't exist on disk. We fall back to copying from
  #    the snapshot directory (if a snapshot was used) so the bundle
  #    still has SOMETHING. For very old live-config runs without a
  #    config_used freeze, the bundle has no config — the README
  #    explains this.
  cfg_note <- character()
  src_cfg_used <- file.path(run_path, "config_used")
  if (dir.exists(src_cfg_used)) {
    dst_cfg_used <- file.path(bundle_root, "config_used")
    dir.create(dst_cfg_used)
    files <- list.files(src_cfg_used, full.names = TRUE, recursive = TRUE)
    rels  <- list.files(src_cfg_used, full.names = FALSE, recursive = TRUE)
    for (i in seq_along(files)) {
      target <- file.path(dst_cfg_used, rels[i])
      dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
      file.copy(files[i], target)
    }
    contents <- c(contents, paste0("config_used/", rels))
  } else {
    # Fallback for older runs: try to copy the snapshot tree if one
    # was named in the manifest.
    src_snap <- .resolve_snapshot_dir_for_run(run_path)
    if (!is.null(src_snap) && dir.exists(src_snap)) {
      dst_snap <- file.path(bundle_root, "config_used")
      dir.create(dst_snap)
      files <- list.files(src_snap, full.names = TRUE, recursive = TRUE)
      rels  <- list.files(src_snap, full.names = FALSE, recursive = TRUE)
      for (i in seq_along(files)) {
        target <- file.path(dst_snap, rels[i])
        dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
        file.copy(files[i], target)
      }
      contents <- c(contents, paste0("config_used/", rels))
      cfg_note <- paste0("(config_used/ recovered from snapshot ",
                          basename(src_snap),
                          "; this run pre-dates the runtime config-freeze ",
                          "feature so config files are the snapshot's, ",
                          "not necessarily what was active at run time.)")
    } else {
      cfg_note <- paste0("(config_used/ not included: this run pre-dates ",
                          "the runtime config-freeze feature and used ",
                          "live config. The config files on disk now may ",
                          "differ from what was active at run time.)")
    }
  }

  # 7. code_version.txt
  cv_lines <- .build_code_version_block(run_path)
  writeLines(cv_lines, file.path(bundle_root, "code_version.txt"))
  contents <- c(contents, "code_version.txt")

  # 7b. approval_summary.txt — a single human-readable file summarizing
  #     the audit trail at a glance: who created the run, who approved
  #     it (with comment), what config kind was used, how many
  #     overrides were applied. The underlying data is in run_status.yml,
  #     config_used.yml, and overrides/, but a reviewer wants the
  #     answer immediately, not a treasure hunt.
  as_lines <- .build_approval_summary_block(run_path)
  writeLines(as_lines, file.path(bundle_root, "approval_summary.txt"))
  contents <- c(contents, "approval_summary.txt")

  # 8. README.txt
  readme <- .build_readme(run_id, contents,
                            inputs_note = inputs_note,
                            cfg_note = cfg_note,
                            run_path = run_path)
  writeLines(readme, file.path(bundle_root, "README.txt"))
  contents <- c(contents, "README.txt")

  # ---- zip the staged tree ----------------------------------------------
  # Use utils::zip with a relative cwd so paths inside the zip start at
  # <run_id>/ rather than the absolute staging path.
  if (file.exists(dest_zip)) file.remove(dest_zip)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging)
  zip_result <- tryCatch(
    utils::zip(zipfile = dest_zip, files = run_id, flags = "-r9X"),
    error = function(e) e
  )
  setwd(old_wd)
  if (inherits(zip_result, "error")) {
    return(list(ok = FALSE, path = NA_character_,
                message = sprintf("zip failed: %s",
                                   conditionMessage(zip_result)),
                contents = contents))
  }
  if (!file.exists(dest_zip) || file.size(dest_zip) == 0) {
    return(list(ok = FALSE, path = NA_character_,
                message = "zip produced no output (utils::zip may be unavailable)",
                contents = contents))
  }

  if (exists("audit_event", mode = "function")) {
    audit_event(list(
      event       = "run_export",
      run_id      = run_id,
      include_inputs = isTRUE(include_inputs),
      n_files     = length(contents),
      bytes       = file.size(dest_zip)
    ))
  }

  list(ok = TRUE,
       path = dest_zip,
       message = sprintf("exported %d files (%.1f MB)",
                          length(contents), file.size(dest_zip) / 1024 / 1024),
       contents = contents)
}


# =============================================================================
# Helpers — keep these private; they read run metadata to figure out
# where inputs and snapshot came from.
# =============================================================================


#' Find the input directory used by a run. Reads input_source.yml if
#' present (H16a runs), else falls back to manifest.json's input_dir
#' (pre-H16a runs that recorded it), else returns NULL.
.resolve_input_dir_for_run <- function(run_path) {
  src <- file.path(run_path, "reports", "input_source.yml")
  if (file.exists(src)) {
    meta <- tryCatch(yaml::read_yaml(src), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$details$path)) {
      return(meta$details$path)
    }
  }
  manifest <- file.path(run_path, "manifest.json")
  if (file.exists(manifest)) {
    m <- tryCatch(jsonlite::fromJSON(manifest), error = function(e) NULL)
    if (!is.null(m$input_dir)) return(m$input_dir)
  }
  NULL
}


#' Find the snapshot directory the run used, if any. Reads
#' manifest.json's snapshot_label and resolves it under the snapshots
#' root.
.resolve_snapshot_dir_for_run <- function(run_path) {
  manifest <- file.path(run_path, "manifest.json")
  if (!file.exists(manifest)) return(NULL)
  m <- tryCatch(jsonlite::fromJSON(manifest), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  label <- m$snapshot_label
  if (is.null(label) || is.na(label) || !nzchar(label)) return(NULL)
  snap_root <- getOption("ifrs9.snapshots_dir",
                          file.path(getOption("ifrs9.project_root", getwd()),
                                    "config_snapshots"))
  candidate <- file.path(snap_root, label)
  if (dir.exists(candidate)) candidate else NULL
}


#' Build the code_version.txt content for a run.
.build_code_version_block <- function(run_path) {
  manifest <- file.path(run_path, "manifest.json")
  m <- tryCatch(jsonlite::fromJSON(manifest), error = function(e) NULL)
  current_sha <- tryCatch(get_current_code_sha(),
                           error = function(e) NA_character_)
  c(
    "Code version captured at run time:",
    sprintf("  run_id          : %s", basename(run_path)),
    sprintf("  code_sha        : %s",
            m$code_sha %||% NA_character_ %||% "(unavailable)"),
    sprintf("  code_dirty      : %s",
            as.character(m$code_dirty %||% NA)),
    "",
    "For comparison, the code SHA in this exporter at export time:",
    sprintf("  exporter_sha    : %s",
            current_sha %||% "(unavailable)"),
    "",
    "If the two SHAs differ, the codebase has evolved since this run.",
    "Reproducing the run requires checking out the run's code_sha."
  )
}


#' Build approval_summary.txt — a single readable file pulling together
#' the audit trail spread across run_status.yml, config_used.yml, and
#' the overrides/ folder. Reviewer-friendly summary so the obvious
#' questions ("who approved this and when?", "did anyone override
#' anything?", "was this live config or a snapshot?") are answered in
#' one place.
.build_approval_summary_block <- function(run_path) {
  out <- character()
  out <- c(out,
    sprintf("Approval summary — run %s", basename(run_path)),
    paste(rep("=", 60), collapse = ""),
    "")

  # --- run_status.yml ---------------------------------------------------
  status_path <- file.path(run_path, "reports", "run_status.yml")
  if (file.exists(status_path)) {
    s <- tryCatch(yaml::read_yaml(status_path), error = function(e) NULL)
    if (!is.null(s)) {
      run_type <- s$run_type %||% "(not recorded)"
      # Surface run_type prominently — for unofficial runs this is the
      # most important fact in the whole summary.
      if (isTRUE(s$run_type == "unofficial") ||
          isTRUE(s$status == "unofficial")) {
        out <- c(out,
          paste(rep("!", 60), collapse = ""),
          "  RUN TYPE: UNOFFICIAL",
          "  This run was a what-if / impact test, not a sanctioned",
          "  deliverable. It did NOT go through the approval workflow.",
          paste(rep("!", 60), collapse = ""),
          "")
      }
      out <- c(out,
        "Run status",
        sprintf("  run_type       : %s", run_type),
        sprintf("  status         : %s", s$status %||% "(unknown)"),
        "")
      trans <- s$transitions %||% list()
      if (length(trans) > 0) {
        out <- c(out, "Transitions (chronological):")
        for (i in seq_along(trans)) {
          t <- trans[[i]]
          out <- c(out,
            sprintf("  [%d] %s", i, t$at %||% "(no timestamp)"),
            sprintf("        to     : %s", t$to %||% "?"),
            sprintf("        by     : %s", t$by %||% "?"),
            sprintf("        reason : %s",
                    if (is.null(t$reason) || !nzchar(t$reason)) "(none)" else t$reason)
          )
        }
        out <- c(out, "")
      }
    }
  } else {
    out <- c(out,
      "Run status",
      "  (run_status.yml not present — pre-H12 run or write failed)",
      "")
  }

  # --- config_used.yml --------------------------------------------------
  cu_path <- file.path(run_path, "config_used", "config_used.yml")
  out <- c(out, "Configuration used")
  if (file.exists(cu_path)) {
    cu <- tryCatch(yaml::read_yaml(cu_path), error = function(e) NULL)
    if (!is.null(cu)) {
      out <- c(out,
        sprintf("  kind           : %s", cu$kind %||% "?"),
        sprintf("  snapshot_label : %s",
                cu$snapshot_label %||% "(none — live config)"),
        sprintf("  source_config  : %s", cu$source_config %||% "?"),
        sprintf("  source_static  : %s", cu$source_static %||% "?"),
        sprintf("  frozen_at      : %s", cu$frozen_at %||% "?"),
        "")
    }
  } else {
    out <- c(out,
      "  (config_used/ not present — pre-H16b run or freeze failed.",
      "   The bundle's config_used/ directory may have been recovered",
      "   from the snapshot dir; see README.txt notes.)",
      "")
  }

  # --- input_source.yml -------------------------------------------------
  is_path <- file.path(run_path, "reports", "input_source.yml")
  out <- c(out, "Input source")
  if (file.exists(is_path)) {
    is <- tryCatch(yaml::read_yaml(is_path), error = function(e) NULL)
    if (!is.null(is)) {
      d <- is$details %||% list()
      out <- c(out,
        sprintf("  kind           : %s", is$kind %||% "?"),
        sprintf("  recorded_at    : %s", is$recorded_at %||% "?"))
      if (!is.null(d$path))         out <- c(out, sprintf("  path           : %s", d$path))
      if (!is.null(d$drop_name))    out <- c(out, sprintf("  drop_name      : %s", d$drop_name))
      if (!is.null(d$source_zip))   out <- c(out, sprintf("  source_zip     : %s", d$source_zip))
      if (!is.null(d$extracted_at)) out <- c(out, sprintf("  extracted_at   : %s", d$extracted_at))
      out <- c(out, "")
    }
  } else {
    out <- c(out,
      "  (input_source.yml not present — pre-H16a run.)",
      "")
  }

  # --- Overrides --------------------------------------------------------
  ov_dir <- file.path(run_path, "overrides")
  out <- c(out, "Overrides applied during this run")
  if (dir.exists(ov_dir)) {
    files <- list.files(ov_dir, "\\.csv$", full.names = TRUE)
    if (length(files) == 0) {
      out <- c(out, "  (none — no overrides applied)", "")
    } else {
      total <- 0L
      for (f in files) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                        error = function(e) NULL)
        n <- if (is.null(df)) NA_integer_ else nrow(df)
        if (!is.na(n)) total <- total + n
        out <- c(out, sprintf("  %-32s : %s rows",
                               basename(f),
                               if (is.na(n)) "(unreadable)" else as.character(n)))
        # Surface who made each override + reason
        if (!is.null(df) && nrow(df) > 0) {
          who_cols <- intersect(c("created_by", "user", "reviewer"),
                                  colnames(df))
          who <- if (length(who_cols) > 0) unique(df[[who_cols[1]]]) else "?"
          out <- c(out,
            sprintf("    created by   : %s",
                     paste(unique(who), collapse = ", ")))
        }
      }
      out <- c(out, sprintf("  TOTAL: %d override row(s) across %d file(s)",
                              total, length(files)),
              "")
    }
  } else {
    out <- c(out, "  (no overrides folder — none applied)", "")
  }

  # --- Tail with code SHA so it's all in one file ----------------------
  manifest <- file.path(run_path, "manifest.json")
  m <- tryCatch(jsonlite::fromJSON(manifest), error = function(e) NULL)
  out <- c(out,
    "Code version",
    sprintf("  code_sha       : %s",
            m$code_sha %||% NA_character_ %||% "(unavailable)"),
    sprintf("  code_dirty     : %s",
            as.character(m$code_dirty %||% NA)),
    "",
    sprintf("Bundle generated: %s",
            format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")))
  out
}


#' Build the README.txt content listed alongside the bundle.
#' If the run is UNOFFICIAL (run_type=unofficial in run_status.yml), the
#' README starts with a prominent banner so a reviewer / downstream
#' consumer cannot mistake the bundle for a sanctioned deliverable.
.build_readme <- function(run_id, contents,
                            inputs_note = character(),
                            cfg_note = character(),
                            run_path = NULL) {
  # Detect run type from run_status.yml if available
  run_type <- NA_character_
  status   <- NA_character_
  if (!is.null(run_path)) {
    rs_path <- file.path(run_path, "reports", "run_status.yml")
    if (file.exists(rs_path)) {
      rs <- tryCatch(yaml::read_yaml(rs_path), error = function(e) NULL)
      if (!is.null(rs)) {
        run_type <- rs$run_type %||% NA_character_
        status   <- rs$status   %||% NA_character_
      }
    }
  }
  is_unofficial <- isTRUE(run_type == "unofficial") ||
                    isTRUE(status == "unofficial")

  banner <- if (is_unofficial) {
    c(
      paste(rep("!", 60), collapse = ""),
      "  THIS IS AN UNOFFICIAL RUN",
      paste(rep("!", 60), collapse = ""),
      "",
      "This run was executed as UNOFFICIAL. It is intended for testing,",
      "what-if analysis, or impact assessment ONLY. It has NOT gone",
      "through the approval workflow and the contents must NOT be",
      "treated as a sanctioned deliverable.",
      "",
      "Do not load these outputs into downstream production systems.",
      "Do not cite these numbers in regulatory or audit reports.",
      "If you need an official version of this calculation, re-run the",
      "pipeline with run_type = 'official' against an approved snapshot",
      "and have it pass through the approval queue.",
      "",
      paste(rep("!", 60), collapse = ""),
      ""
    )
  } else {
    character()
  }

  header <- sprintf("IFRS9 ETL — Run package: %s%s", run_id,
                    if (is_unofficial) "  [UNOFFICIAL]" else "")

  c(
    banner,
    header,
    paste(rep("=", 60), collapse = ""),
    "",
    sprintf("Run type: %s",
            if (is.na(run_type)) "(not recorded — pre-H18 run, treated as official)"
            else toupper(run_type)),
    sprintf("Final status: %s",
            if (is.na(status)) "(unknown)" else status),
    "",
    "This zip contains a complete record of one IFRS9 ETL run. Anyone",
    "with this bundle should be able to:",
    "  1. See the 18 standard output CSVs (folder: outputs/)",
    "  2. See the input files that produced them (folder: inputs/)",
    "  3. See the configuration used at run time (folder: config_used/)",
    "  4. See every validation result (reports/validation.csv|md)",
    "  5. See where the inputs came from (reports/input_source.yml)",
    "  6. See the run's approval status (reports/run_status.yml)",
    "  7. See override decisions made during the run (overrides/)",
    "  8. See the code version that produced the run (code_version.txt)",
    "",
    "Folder layout:",
    "",
    "  manifest.json       run metadata: id, durations, snapshot label,",
    "                      code SHA, totals, file hashes",
    "  outputs/            the 18 standard output CSVs",
    "  inputs/             the 12 input files actually used (xlsx + xls)",
    "  config_used/        config and static reference files frozen at",
    "                      run time. config_used.yml inside this folder",
    "                      identifies whether the source was a named",
    "                      snapshot or live config.",
    "  reports/            validation, reconciliation, input_source,",
    "                      run_status",
    "  overrides/          per-run override CSVs (rating, stage,",
    "                      restructuring), only present if overrides",
    "                      were applied during the run",
    "  code_version.txt    code SHA at run time + comparison to current",
    "  approval_summary.txt  one-page audit summary: who approved,",
    "                      config kind, override counts, code SHA",
    "",
    "Audit trail:",
    "  - reports/run_status.yml lists every transition (created,",
    "    pending_checker, approved/rejected, OR unofficial-terminal)",
    "    with timestamp, user, and reason. Includes run_type field.",
    "  - reports/input_source.yml records whether the inputs came from",
    "    the configured directory, a data drop folder, or a user upload.",
    "  - reports/validation.csv has one row per validator with",
    "    pass/fail, severity, message, and stage.",
    "  - The full app audit log (etl_audit.jsonl) is NOT included here;",
    "    it is project-wide. Filter that log by this run_id for a full",
    "    chronological view.",
    "",
    if (length(inputs_note) > 0) c("Notes on inputs:", paste0("  ", inputs_note), "") else character(),
    if (length(cfg_note) > 0)    c("Notes on config:", paste0("  ", cfg_note), "") else character(),
    "Reproducing this run:",
    "  1. Check out the code at the SHA listed in code_version.txt",
    "  2. Restore the config_used/config files into the project's",
    "     config/ folder, and config_used/static into data-raw/static/.",
    "  3. Place the inputs/ contents in the project's input directory.",
    "  4. Run the pipeline.",
    "  5. The 18 produced CSVs should match outputs/ byte-for-byte",
    "     (modulo timestamp fields in the manifest).",
    "",
    sprintf("Bundle generated: %s",
            format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    sprintf("File count: %d", length(contents))
  )
}
