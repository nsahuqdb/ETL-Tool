# ============================================================================
# R/reconciliation.R
#
# Reconciliation reporting for Phase G output CSVs.
#
# Two modes:
#   1. summarize_output_dir(dir)
#        → list of per-file summary tibbles (row count, schema, per-column
#          stats: sum / mean / min / max / n_NA / n_distinct).
#        Used for monitoring a single run.
#
#   2. reconcile_output_dirs(actual_dir, reference_dir)
#        → list of per-file comparison tibbles + an overall verdict tibble.
#        Header match, row count delta, per-column sum / mean / max-abs-diff
#        / max-rel-diff. For numeric columns only.
#        Used for regression-checking against a previous run or a
#        bundled reference.
#
# Outputs are tibbles, ready to display in Shiny or write to disk.
# A `write_reconciliation_markdown()` helper renders the human-readable
# version to a single .md file.
# ============================================================================


.is_numeric_col <- function(x) {
  is.numeric(x) && !inherits(x, c("Date", "POSIXct", "POSIXt"))
}


# Heuristic: are these strings "date-like" in MM/DD/YYYY (or M/D/YYYY) form?
# Excel CSV dates are commonly stored as strings, so when read.csv brings
# them in as character we still need to compare them as dates so cosmetic
# differences (08/05/2025 vs 8/5/2025) don't count as value mismatches.
.looks_like_date_col <- function(x) {
  if (!is.character(x)) return(FALSE)
  s <- x[!is.na(x) & nzchar(x)]
  if (length(s) == 0) return(FALSE)
  # Sample up to 50 non-empty values
  s <- head(s, 50)
  ok <- grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s*$", s)
  mean(ok) >= 0.9   # 90%+ look like a date
}

# Normalise a date-like string to a canonical form (YYYY-MM-DD).
# NA / empty / unparseable returns NA.
.normalise_date_string <- function(x) {
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x) & nzchar(x) &
         grepl("^\\s*(\\d{1,2})/(\\d{1,2})/(\\d{4})\\s*$", x)
  if (any(ok)) {
    parts <- regmatches(x[ok],
                regexec("^\\s*(\\d{1,2})/(\\d{1,2})/(\\d{4})\\s*$", x[ok]))
    mat <- do.call(rbind, lapply(parts, function(p) p[2:4]))
    out[ok] <- sprintf("%s-%02d-%02d",
                       mat[, 3],
                       as.integer(mat[, 1]),
                       as.integer(mat[, 2]))
  }
  out
}


#' Per-column summary stats for a single tibble.
#'
#' For numeric: n, n_na, n_zero, sum, mean, min, max, sd
#' For chr/factor: n, n_na, n_distinct, top_value
#' For Date / POSIXct: n, n_na, min, max
#'
#' @return tibble with one row per column.
summarize_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(column = character(), kind = character(),
                          n = integer(), n_na = integer(),
                          stat1 = character(), stat2 = character(),
                          stat3 = character(), stat4 = character()))
  }
  out <- vector("list", ncol(df))
  for (i in seq_along(df)) {
    nm  <- names(df)[i]
    col <- df[[i]]
    n_total <- length(col)
    n_na    <- sum(is.na(col))
    if (.is_numeric_col(col)) {
      vals <- col[!is.na(col)]
      out[[i]] <- tibble::tibble(
        column   = nm, kind = "numeric",
        n        = n_total, n_na = n_na,
        stat1    = sprintf("sum=%.6g",  sum(vals)),
        stat2    = sprintf("mean=%.6g", mean(vals)),
        stat3    = sprintf("min=%.6g",  if (length(vals)) min(vals) else NA_real_),
        stat4    = sprintf("max=%.6g",  if (length(vals)) max(vals) else NA_real_)
      )
    } else if (inherits(col, c("Date","POSIXct","POSIXt"))) {
      vals <- col[!is.na(col)]
      out[[i]] <- tibble::tibble(
        column = nm, kind = "date",
        n      = n_total, n_na = n_na,
        stat1  = sprintf("min=%s", if (length(vals)) as.character(min(vals)) else NA),
        stat2  = sprintf("max=%s", if (length(vals)) as.character(max(vals)) else NA),
        stat3  = "", stat4 = ""
      )
    } else {
      vals <- as.character(col[!is.na(col)])
      n_dist <- length(unique(vals))
      top_t  <- if (length(vals)) {
        tab <- sort(table(vals), decreasing = TRUE)
        sprintf("top='%s' (%d)", names(tab)[1], as.integer(tab[1]))
      } else ""
      out[[i]] <- tibble::tibble(
        column = nm, kind = "chr",
        n      = n_total, n_na = n_na,
        stat1  = sprintf("n_distinct=%d", n_dist),
        stat2  = top_t, stat3 = "", stat4 = ""
      )
    }
  }
  do.call(rbind, out)
}


#' Summarize a single CSV file.
#'
#' @return list(file=, rows=, cols=, summary=tibble)
summarize_csv <- function(path) {
  if (!file.exists(path)) {
    return(list(file = basename(path), rows = NA, cols = NA,
                summary = NULL, error = "file not found"))
  }
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
             colClasses = NA),
    error = function(e) NULL
  )
  if (is.null(df)) {
    return(list(file = basename(path), rows = NA, cols = NA,
                summary = NULL, error = "could not parse"))
  }
  list(file    = basename(path),
       rows    = nrow(df),
       cols    = ncol(df),
       summary = summarize_columns(df),
       error   = NA_character_)
}


#' Summarize every CSV in a directory.
#'
#' @return named list (one entry per file)
summarize_output_dir <- function(dir) {
  if (!dir.exists(dir)) stop("directory does not exist: ", dir)
  csvs <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) {
    warning("no .csv files found in ", dir)
    return(list())
  }
  out <- lapply(csvs, summarize_csv)
  names(out) <- vapply(out, function(x) x$file, character(1))
  out
}


# ============================================================================
# RECONCILIATION — comparison against a reference dir
# ============================================================================


#' Default natural-key spec for the 18 IFRS9 output files.
#'
#' Used by reconcile_csv() / reconcile_output_dirs() to align rows by key
#' (rather than by position) before column-by-column comparison. This is
#' the right behavior whenever two CSVs may be sorted differently.
#'
#' Files not in this list fall back to position-based comparison.
default_key_spec <- function() {
  list(
    AccountMaster_1               = "ContractId",
    AccountMaster_2               = "ContractId",
    CustomerMaster_1              = "CustomerId",
    CustomerMaster_2              = "CustomerId",
    CustomerStagingFlag_1         = "CustomerId",
    CustomerStagingFlag_2         = "CustomerId",
    Origination_1                 = "ContractId",
    Origination_2                 = "ContractId",
    Collateral                    = "CollateralId",
    AccountCollateralAllocation   = c("CollateralId", "ContractId"),
    LifeTimeParameterOther        = c("ContractId", "MonthLifetime"),
    StPD                          = c("PortfolioCode", "PDBucketDim1",
                                       "MonthLifetime"),
    FxRate                        = "CurrencyCode",
    Portfolios                    = "PortfolioCode",
    PortfolioRatingType           = c("PortfolioCode", "RatingType"),
    Ratings                       = "Rating",
    RatingTypes                   = "RatingType",
    CollateralType                = "CollateralTypeId"
  )
}


#' Compare two tibbles by joining on `key` columns (key-based diff).
#'
#' For each non-key column present in both:
#'   * numeric    -> sum_act, sum_ref, sum_diff, max_abs_diff, max_rel_diff
#'                   computed over rows matched on key
#'   * other      -> set-difference count
#'
#' Returns NULL if `key` is empty or any key column is missing — caller
#' should fall back to compare_columns_positional().
#'
#' Returns a list:
#'   $per_column    tibble (column-by-column comparison)
#'   $matched_idx_a integer vector — row indices in `act` that matched
#'   $matched_idx_r integer vector — corresponding row indices in `ref`
#'   $unmatched_act tibble — rows of `act` whose key wasn't in `ref`
#'   $unmatched_ref tibble — rows of `ref` whose key wasn't in `act`
compare_columns_keyed <- function(act, ref, key) {
  if (length(key) == 0) return(NULL)
  miss_a <- setdiff(key, names(act))
  miss_r <- setdiff(key, names(ref))
  if (length(miss_a) > 0 || length(miss_r) > 0) return(NULL)

  keystr <- function(df) {
    cols <- lapply(key, function(k) as.character(df[[k]]))
    do.call(paste, c(cols, sep = "\u001f"))
  }
  ka <- keystr(act); kr <- keystr(ref)
  m_a <- match(ka, kr)
  matched_idx_a <- which(!is.na(m_a))
  matched_idx_r <- m_a[matched_idx_a]
  unmatched_a_rows <- which(is.na(m_a))
  unmatched_r_rows <- setdiff(seq_len(nrow(ref)), matched_idx_r)
  unmatched_a <- length(unmatched_a_rows)
  unmatched_r <- length(unmatched_r_rows)

  cols_a <- names(act); cols_r <- names(ref)
  all_cols <- union(cols_a, cols_r)
  out <- vector("list", length(all_cols))

  safe_max <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    max(x, na.rm = TRUE)
  }

  for (i in seq_along(all_cols)) {
    nm <- all_cols[i]
    in_a <- nm %in% cols_a
    in_r <- nm %in% cols_r
    if (!in_a || !in_r) {
      out[[i]] <- tibble::tibble(
        column = nm, kind = if (nm %in% key) "key-absent" else "absent",
        in_actual = in_a, in_reference = in_r,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = "column not in both")
      next
    }
    if (nm %in% key) {
      out[[i]] <- tibble::tibble(
        column = nm, kind = "key",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = sprintf("matched=%d  unmatched_act=%d  unmatched_ref=%d",
                        length(matched_idx_a), unmatched_a, unmatched_r))
      next
    }
    a <- act[[nm]][matched_idx_a]
    r <- ref[[nm]][matched_idx_r]
    if (.is_numeric_col(a) && .is_numeric_col(r)) {
      a_n <- as.numeric(a); r_n <- as.numeric(r)
      d <- a_n - r_n
      m_abs <- safe_max(abs(d))
      denom <- pmax(abs(r_n), 1e-12)
      m_rel <- safe_max(abs(d) / denom)
      out[[i]] <- tibble::tibble(
        column = nm, kind = "numeric",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = sum(a_n, na.rm = TRUE),
        sum_ref = sum(r_n, na.rm = TRUE),
        sum_diff = sum(a_n, na.rm=TRUE) - sum(r_n, na.rm=TRUE),
        max_abs_diff = m_abs, max_rel_diff = m_rel,
        notes = sprintf("compared %d matched rows", length(matched_idx_a)))
    } else if (.looks_like_date_col(a) || .looks_like_date_col(r)) {
      # Normalise both sides to YYYY-MM-DD before comparing so 08/05/2025
      # vs 8/5/2025 doesn't count as a value mismatch.
      a_d <- .normalise_date_string(as.character(a))
      r_d <- .normalise_date_string(as.character(r))
      a_d[is.na(a_d)] <- ""; r_d[is.na(r_d)] <- ""
      n_diff <- sum(a_d != r_d)
      out[[i]] <- tibble::tibble(
        column = nm, kind = "date",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = sprintf("date mismatches=%d (of %d matched rows)",
                        n_diff, length(matched_idx_a)))
    } else {
      a_c <- as.character(a); r_c <- as.character(r)
      a_c[is.na(a_c)] <- ""; r_c[is.na(r_c)] <- ""
      n_diff <- sum(a_c != r_c)
      out[[i]] <- tibble::tibble(
        column = nm, kind = "non-numeric",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = sprintf("string mismatches=%d (of %d matched rows)",
                        n_diff, length(matched_idx_a)))
    }
  }
  list(per_column      = do.call(rbind, out),
       matched_idx_a   = matched_idx_a,
       matched_idx_r   = matched_idx_r,
       unmatched_a_rows = unmatched_a_rows,
       unmatched_r_rows = unmatched_r_rows,
       n_matched       = length(matched_idx_a),
       n_unmatched_act = unmatched_a,
       n_unmatched_ref = unmatched_r)
}


#' Position-based comparison (legacy fallback).
compare_columns_positional <- function(act, ref) {
  cols_a <- names(act); cols_r <- names(ref)
  all_cols <- union(cols_a, cols_r)
  out <- vector("list", length(all_cols))
  for (i in seq_along(all_cols)) {
    nm <- all_cols[i]
    in_a <- nm %in% cols_a; in_r <- nm %in% cols_r
    if (!in_a || !in_r) {
      out[[i]] <- tibble::tibble(
        column = nm, kind = "absent",
        in_actual = in_a, in_reference = in_r,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = "column not in both")
      next
    }
    a <- act[[nm]]; r <- ref[[nm]]
    if (.is_numeric_col(a) && .is_numeric_col(r)) {
      n <- min(length(a), length(r))
      a_n <- as.numeric(a)[seq_len(n)]
      r_n <- as.numeric(r)[seq_len(n)]
      d <- a_n - r_n
      m_abs <- max(abs(d), na.rm = TRUE)
      denom <- pmax(abs(r_n), 1e-12)
      m_rel <- max(abs(d) / denom, na.rm = TRUE)
      out[[i]] <- tibble::tibble(
        column = nm, kind = "numeric",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = sum(a_n, na.rm = TRUE),
        sum_ref = sum(r_n, na.rm = TRUE),
        sum_diff = sum(a_n, na.rm=TRUE) - sum(r_n, na.rm=TRUE),
        max_abs_diff = m_abs, max_rel_diff = m_rel,
        notes = if (length(a) != length(r)) {
          sprintf("compared first %d of %d/%d rows", n, length(a), length(r))
        } else {
          ""
        })
    } else {
      a_set <- unique(as.character(a[!is.na(a)]))
      r_set <- unique(as.character(r[!is.na(r)]))
      diff_count <- length(setdiff(a_set, r_set)) + length(setdiff(r_set, a_set))
      out[[i]] <- tibble::tibble(
        column = nm, kind = "non-numeric",
        in_actual = TRUE, in_reference = TRUE,
        sum_act = NA_real_, sum_ref = NA_real_, sum_diff = NA_real_,
        max_abs_diff = NA_real_, max_rel_diff = NA_real_,
        notes = sprintf("n_distinct_act=%d, n_distinct_ref=%d, set_symdiff=%d",
                        length(a_set), length(r_set), diff_count))
    }
  }
  do.call(rbind, out)
}


#' Compare two tibbles. Tries key-based comparison if `key` provided;
#' falls back to positional otherwise.
#'
#' @return list with $per_column tibble, plus (if keyed) matched/unmatched
#'   row indices for downstream dumping.
compare_columns <- function(act, ref, key = NULL) {
  if (!is.null(key) && length(key) > 0) {
    keyed <- compare_columns_keyed(act, ref, key)
    if (!is.null(keyed)) return(keyed)
  }
  list(per_column = compare_columns_positional(act, ref),
       matched_idx_a = NULL, matched_idx_r = NULL,
       unmatched_a_rows = NULL, unmatched_r_rows = NULL)
}


#' Dump per-file mismatch artifacts.
#'
#' For a reconciled file with a key, writes (under `dir`):
#'   <basename>_unmatched_actual.csv   - rows in actual whose key wasn't in ref
#'   <basename>_unmatched_reference.csv - rows in ref whose key wasn't in actual
#'   <basename>_value_diffs.csv        - rows where keys matched but at
#'                                        least one value differs (side-by-side)
#'
#' Returns a named list of paths actually written (empty for files that match).
dump_mismatches <- function(reconcile_obj, actual_dir, reference_dir, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  written <- list()

  for (f in names(reconcile_obj$per_file)) {
    rec <- reconcile_obj$per_file[[f]]
    if (is.null(rec) || !is.na(rec$error %||% NA)) next
    if (is.null(rec$comparison$matched_idx_a)) next  # no key

    base_no_ext <- sub("\\.csv$", "", f)
    act <- read.csv(file.path(actual_dir, f), stringsAsFactors = FALSE,
                    check.names = FALSE, colClasses = NA)
    ref <- read.csv(file.path(reference_dir, f), stringsAsFactors = FALSE,
                    check.names = FALSE, colClasses = NA)
    cmp <- rec$comparison

    # 1. Unmatched actual rows
    if (length(cmp$unmatched_a_rows) > 0) {
      p <- file.path(dir, paste0(base_no_ext, "_unmatched_actual.csv"))
      write.csv(act[cmp$unmatched_a_rows, , drop = FALSE], p,
                row.names = FALSE, na = "")
      written[[paste0(f, ":unmatched_actual")]] <-
        list(path = p, rows = length(cmp$unmatched_a_rows))
    }

    # 2. Unmatched reference rows
    if (length(cmp$unmatched_r_rows) > 0) {
      p <- file.path(dir, paste0(base_no_ext, "_unmatched_reference.csv"))
      write.csv(ref[cmp$unmatched_r_rows, , drop = FALSE], p,
                row.names = FALSE, na = "")
      written[[paste0(f, ":unmatched_reference")]] <-
        list(path = p, rows = length(cmp$unmatched_r_rows))
    }

    # 3. Value diffs on matched rows
    if (length(cmp$matched_idx_a) > 0) {
      a_m <- act[cmp$matched_idx_a, , drop = FALSE]
      r_m <- ref[cmp$matched_idx_r, , drop = FALSE]
      common_cols <- intersect(names(a_m), names(r_m))
      key <- cmp$per_column$column[cmp$per_column$kind == "key"]
      compare_cols <- setdiff(common_cols, key)

      # Compute differing rows: any non-key common column differs
      diff_mask <- rep(FALSE, nrow(a_m))
      for (col in compare_cols) {
        a_c <- a_m[[col]]; r_c <- r_m[[col]]
        if (.is_numeric_col(a_c) && .is_numeric_col(r_c)) {
          # Numeric tolerance: 1e-6 absolute or 1e-6 relative
          d <- abs(as.numeric(a_c) - as.numeric(r_c))
          tol <- pmax(1e-6, abs(as.numeric(r_c)) * 1e-6)
          diff_mask <- diff_mask | (!is.na(d) & d > tol)
        } else if (.looks_like_date_col(a_c) || .looks_like_date_col(r_c)) {
          a_d <- .normalise_date_string(as.character(a_c))
          r_d <- .normalise_date_string(as.character(r_c))
          a_d[is.na(a_d)] <- ""; r_d[is.na(r_d)] <- ""
          diff_mask <- diff_mask | (a_d != r_d)
        } else {
          a_s <- as.character(a_c); a_s[is.na(a_s)] <- ""
          r_s <- as.character(r_c); r_s[is.na(r_s)] <- ""
          diff_mask <- diff_mask | (a_s != r_s)
        }
      }

      if (any(diff_mask)) {
        sub_a <- a_m[diff_mask, , drop = FALSE]
        sub_r <- r_m[diff_mask, , drop = FALSE]
        # Side-by-side: key cols once; then for each non-key common col,
        # both _actual and _reference variants.
        side <- sub_a[, key, drop = FALSE]
        for (col in compare_cols) {
          side[[paste0(col, "_actual")]]    <- sub_a[[col]]
          side[[paste0(col, "_reference")]] <- sub_r[[col]]
        }
        p <- file.path(dir, paste0(base_no_ext, "_value_diffs.csv"))
        write.csv(side, p, row.names = FALSE, na = "")
        written[[paste0(f, ":value_diffs")]] <-
          list(path = p, rows = sum(diff_mask))
      }
    }
  }

  written
}


#' Reconcile two CSV files.
#'
#' @param actual_path,reference_path file paths
#' @param key character vector of key columns for keyed comparison; pass
#'        NULL to fall back to positional comparison.
#' @return list(file=, rows_act=, rows_ref=, headers_match=, comparison=)
reconcile_csv <- function(actual_path, reference_path, key = NULL) {
  if (!file.exists(actual_path)) {
    return(list(file = basename(actual_path), error = "actual not found"))
  }
  if (!file.exists(reference_path)) {
    return(list(file = basename(actual_path), error = "reference not found"))
  }
  act <- tryCatch(read.csv(actual_path, stringsAsFactors = FALSE,
                           check.names = FALSE, colClasses = NA),
                  error = function(e) NULL)
  ref <- tryCatch(read.csv(reference_path, stringsAsFactors = FALSE,
                           check.names = FALSE, colClasses = NA),
                  error = function(e) NULL)
  if (is.null(act) || is.null(ref)) {
    return(list(file = basename(actual_path), error = "parse failure"))
  }

  # Header equality, ignoring trailing empty-named columns (Excel CSV
  # writer adds them in some files — they're not real columns).
  strip_empty_tail <- function(nm) {
    while (length(nm) > 0 && (is.na(tail(nm, 1)) || tail(nm, 1) == "")) {
      nm <- nm[-length(nm)]
    }
    nm
  }
  hdr_match <- identical(strip_empty_tail(names(act)),
                          strip_empty_tail(names(ref)))

  list(file          = basename(actual_path),
       rows_act      = nrow(act),
       rows_ref      = nrow(ref),
       cols_act      = ncol(act),
       cols_ref      = ncol(ref),
       headers_match = hdr_match,
       comparison    = compare_columns(act, ref, key = key),
       error         = NA_character_)
}


#' Reconcile every CSV across two directories.
#'
#' @param actual_dir,reference_dir directories
#' @param key_spec named list mapping file basename (without .csv) to key
#'        column vector. Defaults to default_key_spec().
#' @return list(per_file=list(reconcile_csv outputs), verdict=tibble)
reconcile_output_dirs <- function(actual_dir, reference_dir,
                                    key_spec = default_key_spec()) {
  if (!dir.exists(actual_dir))    stop("actual_dir not found: ",    actual_dir)
  if (!dir.exists(reference_dir)) stop("reference_dir not found: ", reference_dir)

  act_files <- list.files(actual_dir, pattern = "\\.csv$")
  ref_files <- list.files(reference_dir, pattern = "\\.csv$")
  all_files <- union(act_files, ref_files)

  per_file <- list()
  verdict_rows <- list()
  for (f in all_files) {
    base_no_ext <- sub("\\.csv$", "", f)
    key <- key_spec[[base_no_ext]]
    act_p <- file.path(actual_dir, f)
    ref_p <- file.path(reference_dir, f)
    if (!f %in% act_files) {
      verdict_rows[[length(verdict_rows) + 1]] <- tibble::tibble(
        file = f, status = "missing_actual",
        rows_act = NA_integer_, rows_ref = length(readLines(ref_p, warn = FALSE)) - 1L,
        rows_diff = NA_integer_, headers_match = NA, max_abs_diff = NA_real_)
      next
    }
    if (!f %in% ref_files) {
      verdict_rows[[length(verdict_rows) + 1]] <- tibble::tibble(
        file = f, status = "missing_reference",
        rows_act = length(readLines(act_p, warn = FALSE)) - 1L,
        rows_ref = NA_integer_, rows_diff = NA_integer_,
        headers_match = NA, max_abs_diff = NA_real_)
      next
    }

    rec <- reconcile_csv(act_p, ref_p, key = key)
    per_file[[f]] <- rec
    if (!is.na(rec$error %||% NA)) {
      verdict_rows[[length(verdict_rows) + 1]] <- tibble::tibble(
        file = f, status = paste0("error:", rec$error),
        rows_act = NA_integer_, rows_ref = NA_integer_,
        rows_diff = NA_integer_, headers_match = NA, max_abs_diff = NA_real_)
      next
    }
    cmp <- rec$comparison$per_column
    nm_diff_max <- if (!is.null(cmp)) {
      v <- cmp$max_abs_diff[cmp$kind == "numeric"]
      v <- v[!is.na(v) & is.finite(v)]
      if (length(v)) max(v) else NA_real_
    } else NA_real_

    status <- if (!rec$headers_match) {
      "header_mismatch"
    } else if (rec$rows_act != rec$rows_ref) {
      "row_count_mismatch"
    } else if (is.finite(nm_diff_max) && nm_diff_max > 1e-6) {
      "value_drift"
    } else {
      "match"
    }
    verdict_rows[[length(verdict_rows) + 1]] <- tibble::tibble(
      file          = f,
      status        = status,
      rows_act      = rec$rows_act,
      rows_ref      = rec$rows_ref,
      rows_diff     = rec$rows_act - rec$rows_ref,
      headers_match = rec$headers_match,
      max_abs_diff  = nm_diff_max)
  }

  list(per_file = per_file,
       verdict  = do.call(rbind, verdict_rows))
}


# ============================================================================
# REPORT WRITERS
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a


#' Write a Markdown summary report for a single run (no comparison).
write_summary_markdown <- function(summary_list, out_path) {
  lines <- c(
    sprintf("# IFRS9 ETL Output Summary"),
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Files: %d", length(summary_list)),
    "",
    "## Files",
    "",
    "| File | Rows | Cols |",
    "|------|------|------|"
  )
  for (s in summary_list) {
    lines <- c(lines, sprintf("| %s | %s | %s |",
                              s$file,
                              if (is.na(s$rows)) "?" else format(s$rows, big.mark=","),
                              if (is.na(s$cols)) "?" else s$cols))
  }
  for (s in summary_list) {
    if (is.null(s$summary)) next
    lines <- c(lines, "", sprintf("## %s", s$file), "")
    lines <- c(lines, "| Column | Kind | n | n_NA | Stat1 | Stat2 | Stat3 | Stat4 |",
                       "|--------|------|---|------|-------|-------|-------|-------|")
    for (i in seq_len(nrow(s$summary))) {
      r <- s$summary[i, ]
      lines <- c(lines, sprintf("| %s | %s | %d | %d | %s | %s | %s | %s |",
                                r$column, r$kind, r$n, r$n_na,
                                r$stat1, r$stat2, r$stat3, r$stat4))
    }
  }
  writeLines(lines, out_path)
  invisible(out_path)
}


#' Write a Markdown reconciliation report (comparison against reference).
write_reconciliation_markdown <- function(reconcile_obj, out_path) {
  v <- reconcile_obj$verdict
  lines <- c(
    sprintf("# IFRS9 ETL Reconciliation Report"),
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Verdict",
    "",
    sprintf("- Files compared: %d", nrow(v)),
    sprintf("- match:              %d", sum(v$status == "match")),
    sprintf("- row_count_mismatch: %d", sum(v$status == "row_count_mismatch")),
    sprintf("- header_mismatch:    %d", sum(v$status == "header_mismatch")),
    sprintf("- value_drift:        %d", sum(v$status == "value_drift")),
    sprintf("- missing_actual:     %d", sum(v$status == "missing_actual")),
    sprintf("- missing_reference:  %d", sum(v$status == "missing_reference")),
    sprintf("- errors:             %d", sum(grepl("^error:", v$status))),
    "",
    "## Per-file verdict",
    "",
    "| File | Status | Rows (actual / ref) | Δ rows | Headers match | Max abs diff |",
    "|------|--------|---------------------|--------|---------------|--------------|"
  )
  for (i in seq_len(nrow(v))) {
    r <- v[i, ]
    lines <- c(lines, sprintf("| %s | %s | %s / %s | %s | %s | %s |",
      r$file, r$status,
      if (is.na(r$rows_act)) "?" else format(r$rows_act, big.mark=","),
      if (is.na(r$rows_ref)) "?" else format(r$rows_ref, big.mark=","),
      if (is.na(r$rows_diff)) "?" else format(r$rows_diff, big.mark=","),
      if (is.na(r$headers_match)) "?" else as.character(r$headers_match),
      if (is.na(r$max_abs_diff)) "?" else format(r$max_abs_diff, scientific = TRUE, digits = 3)))
  }

  # Per-file column-level detail (only files with drift or mismatch)
  drift_files <- v$file[v$status %in% c("value_drift","row_count_mismatch","header_mismatch")]
  if (length(drift_files) > 0) {
    lines <- c(lines, "", "## Column-level detail (files with drift / mismatch)")
    for (f in drift_files) {
      rec <- reconcile_obj$per_file[[f]]
      if (is.null(rec) || is.null(rec$comparison) ||
           is.null(rec$comparison$per_column)) next
      lines <- c(lines, "", sprintf("### %s", f), "")
      lines <- c(lines,
        "| Column | Kind | sum (act) | sum (ref) | sum diff | max abs diff | max rel diff | Notes |",
        "|--------|------|-----------|-----------|----------|--------------|--------------|-------|")
      cmp <- rec$comparison$per_column
      for (i in seq_len(nrow(cmp))) {
        r <- cmp[i, ]
        lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
          r$column, r$kind,
          if (is.na(r$sum_act)) "" else format(r$sum_act, big.mark=",", digits = 6),
          if (is.na(r$sum_ref)) "" else format(r$sum_ref, big.mark=",", digits = 6),
          if (is.na(r$sum_diff)) "" else format(r$sum_diff, scientific = TRUE, digits = 3),
          if (is.na(r$max_abs_diff)) "" else format(r$max_abs_diff, scientific = TRUE, digits = 3),
          if (is.na(r$max_rel_diff)) "" else format(r$max_rel_diff, scientific = TRUE, digits = 3),
          r$notes %||% ""))
      }
    }
  }

  writeLines(lines, out_path)
  invisible(out_path)
}
