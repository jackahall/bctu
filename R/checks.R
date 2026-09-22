# ---------------------------------------------------------------------------
# Checks: DVP (Data Validation) and CDI (Critical Data Items)
# ---------------------------------------------------------------------------
# A DVP is a single function of a snapshot's data that returns a NAMED list of
# findings. Each named element is one check; the name becomes the worksheet
# name. Findings are free-form data frames: a check returns whatever columns it
# needs (one field, several fields, or cross-record combinations). Nothing about
# the shape of a finding is fixed, so a check can report whatever combination of
# variables and records raised it.
#
# To see what has changed, the SAME DVP function is run on two datasets (a
# `before` and an `after` snapshot) and the findings are compared whole-row per
# check: a finding is `new` if its entire row appears in `after` but not
# `before`, `resolved` if it appears in `before` but not `after`, `unchanged` if
# in both. There is no baseline ledger and no fixed finding key; the two
# datasets you pass ARE the comparison.

# --- running a DVP ----------------------------------------------------------
#' Run a DVP function on one dataset and validate its output
#'
#' A DVP is `function(data)` returning a named list, one element per check. Each
#' element is a data frame of findings (or `NULL` / an empty data frame when the
#' check finds nothing). The function is rejected with a clear error if it does
#' not return a named list, so a malformed DVP fails immediately rather than
#' producing an unusable report.
#' @param dvp A function of the data returning a named list of findings.
#' @param data The data the DVP reads: usually a snapshot (a named list of data
#'   frames), but any object the DVP function accepts.
#' @return A named list of findings data frames (empty frames kept in place).
#' @examples
#' data <- data.frame(id = 1:3, weight_kg = c(65, 999, 70))
#' dvp <- function(data) {
#'   list(weight_range = data[data$weight_kg > 300, ])
#' }
#' run_dvp(dvp, data)
#' @export
run_dvp <- function(dvp, data) {
  if (!is.function(dvp))
    cli::cli_abort(c(
      "A DVP must be a function of the data.",
      "i" = "Write it as {.code function(data) list(check_one = ..., check_two = ...)}."))

  findings <- dvp(data)

  # An explicitly named EMPTY list (names character(0), e.g. a placeholder DVP
  # with no checks defined yet) is valid and yields a zero-check report; a bare
  # list() with NULL names is still rejected as a likely mistake.
  ok_named_list <- is.list(findings) && !is.data.frame(findings) &&
    !is.null(names(findings)) &&
    all(nzchar(names(findings))) && !anyDuplicated(names(findings))
  if (!ok_named_list)
    cli::cli_abort(c(
      "A DVP function must return a named list of findings, one element per check.",
      "x" = "Each element needs a non-empty, unique name (used as the worksheet name).",
      "i" = "For example: {.code function(data) list(weight_range = ..., visit_order = ...)}.",
      "i" = "A DVP with no checks yet returns {.code stats::setNames(list(), character(0))}."))

  for (nm in names(findings)) {
    el <- findings[[nm]]
    if (is.null(el)) {
      findings[[nm]] <- data.frame()
    } else if (!is.data.frame(el)) {
      cli::cli_abort(c(
        "Check {.val {nm}} must return a data frame of findings.",
        "i" = "Return an empty data frame (or {.code NULL}) when the check finds nothing."))
    }
  }
  findings
}

# --- before / after comparison ---------------------------------------------
#' Whole-row identity keys for a findings data frame
#'
#' Joins every column of each row into one string, so two findings are the same
#' only when their entire rows match. Findings are free-form, so identity is the
#' whole row and nothing is assumed about column names. A genuine `NA` is
#' encoded with a reserved control character, distinct from the literal string
#' `"NA"`, so the two can never collide in the key.
#' @param df A findings data frame.
#' @return A character vector of one key per row (empty for a zero-row frame).
#' @export
finding_row_keys <- function(df) {
  if (nrow(df) == 0L) return(character(0))
  cols <- lapply(df, function(col) {
    ch <- as.character(col)
    ch[is.na(col)] <- "\x02"
    ch
  })
  do.call(paste, c(cols, sep = "\x1f"))
}

#' Length-`n` vector of `NA`s of the same type as a template column
#'
#' Typed so that a filled-in column binds onto a column of the same name
#' without coercion; a bare `NA` is logical and will not bind onto a labelled
#' or date column.
#' @param template A vector to take the type from.
#' @param n Length of the result.
#' @return A length-`n` vector of `NA`s of `template`'s type.
#' @keywords internal
na_like <- function(template, n) template[rep(NA_integer_, n)]

#' Give two findings frames the same columns, with the same types
#'
#' A column missing from one frame is added to it as typed `NA`s taken from the
#' other. A column present in both but read as different types (a field that
#' was empty in one extract and populated in the next) is widened: an
#' all-missing column takes the other frame's type, and any other mismatch is
#' written as text, with a warning naming the columns.
#' @param x,y Findings data frames.
#' @param check Optional check name, named in the warning.
#' @return A list of the two frames, carrying the same columns in the same
#'   order.
#' @export
align_findings <- function(x, y, check = NULL) {
  cols <- union(names(x), names(y))
  for (col in setdiff(cols, names(x))) x[[col]] <- na_like(y[[col]], nrow(x))
  for (col in setdiff(cols, names(y))) y[[col]] <- na_like(x[[col]], nrow(y))

  widened <- character(0)
  for (col in cols) {
    a <- x[[col]]; b <- y[[col]]
    if (identical(class(a), class(b))) next
    if (is.logical(a) && all(is.na(a)))      x[[col]] <- na_like(b, length(a))
    else if (is.logical(b) && all(is.na(b))) y[[col]] <- na_like(a, length(b))
    else {
      x[[col]] <- as.character(a)
      y[[col]] <- as.character(b)
      widened <- c(widened, col)
    }
  }
  if (length(widened)) {
    in_check <- if (is.null(check)) "" else paste0(" in check '", check, "'")
    cli::cli_warn(c(
      "{cli::qty(length(widened))}Column{?s} {.val {widened}}{in_check} changed type between the two snapshots.",
      "i" = "Written as text so the before/after comparison can proceed."))
  }
  list(x[cols], y[cols])
}

#' Row-bind two findings frames, tolerating differing columns
#'
#' The `before` and `after` findings for one check share the same columns and
#' types in normal use, so this is a plain `rbind`; [align_findings()] handles
#' a check that appears in one run but not the other, and a column whose type
#' changed between the two extracts.
#' @param x,y Findings data frames.
#' @param check Optional check name, named in any alignment warning.
#' @return One data frame with the union of columns.
#' @export
bind_findings <- function(x, y, check = NULL) {
  if (nrow(x) == 0L && nrow(y) == 0L) {
    cols <- union(names(x), names(y))
    return(as.data.frame(
      stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE))
  }
  if (nrow(x) == 0L) return(y)
  if (nrow(y) == 0L) return(x)
  aligned <- align_findings(x, y, check = check)
  out <- rbind(aligned[[1L]], aligned[[2L]])
  rownames(out) <- NULL
  out
}

#' Compare a DVP's findings between two datasets (before vs after)
#'
#' Runs `dvp` on `before` and on `after` and, for every check, labels each
#' finding whole-row: `new` (row present in `after` but not `before`),
#' `resolved` (row present in `before` but not `after`) or `unchanged` (row in
#' both). Rows are keyed positionally over the columns each frame returns, so a
#' check must return the same columns, in the same order, on both runs; a check
#' that returns different or reordered columns between `before` and `after` is
#' not reconciled and every row will read as changed.
#' @param dvp A DVP function (see [run_dvp()]).
#' @param before,after The two datasets (snapshots) to compare.
#' @return A named list; each element is that check's findings with an added
#'   `status` column (`new` / `unchanged` / `resolved`).
#' @examples
#' before <- data.frame(id = 1:3, weight_kg = c(65, 999, 70))
#' after  <- data.frame(id = 1:3, weight_kg = c(65, 72, 70))
#' dvp <- function(data) {
#'   list(weight_range = data[data$weight_kg > 300, ])
#' }
#' compare_dvp(dvp, before, after)
#' @export
compare_dvp <- function(dvp, before, after) {
  b <- run_dvp(dvp, before)
  a <- run_dvp(dvp, after)

  has_status <- function(d) is.data.frame(d) && "status" %in% names(d)
  offenders <- unique(c(names(a)[vapply(a, has_status, logical(1))],
                        names(b)[vapply(b, has_status, logical(1))]))
  if (length(offenders))
    cli::cli_abort(c(
      "A check returned a reserved column named {.field status}: {.val {offenders}}.",
      "i" = "{.fn compare_dvp} adds its own {.field status} column (new / unchanged / resolved).",
      "x" = "Rename that column in your DVP (for example {.field query_status})."))

  checks <- union(names(a), names(b))
  out <- stats::setNames(vector("list", length(checks)), checks)
  for (nm in checks) {
    bf <- if (is.null(b[[nm]])) data.frame() else b[[nm]]
    af <- if (is.null(a[[nm]])) data.frame() else a[[nm]]
    bk <- finding_row_keys(bf)
    ak <- finding_row_keys(af)

    current <- af
    current$status <- if (nrow(af)) ifelse(ak %in% bk, "unchanged", "new") else character(0)

    resolved <- bf[!(bk %in% ak), , drop = FALSE]
    resolved$status <- if (nrow(resolved)) "resolved" else character(0)

    out[[nm]] <- bind_findings(current, resolved, check = nm)
  }
  attr(out, "check_info") <- attr(a, "check_info") %||% attr(b, "check_info")
  out
}

# --- per-check query text (check info) --------------------------------------
#' Validate a check-info table (per-check query text and labels)
#'
#' A check-info table describes the checks in DM-facing language: one row per
#' check with the query text a data manager acts on, plus optional grouping and
#' criticality labels. Required columns: `check` (unique, matching the names the
#' DVP function returns) and `query` (non-empty text). Optional columns:
#' `section` (free text) and `critical` (logical), settable independently.
#' Checks with findings but no info row, and info rows matching no check, are
#' warned about (not errors), so a partially annotated DVP still reports.
#' @param info A data frame as described above.
#' @param check_names The check names the DVP produced this run.
#' @return `info`, with columns ordered `check`, `section`, `critical`, `query`
#'   (those present), row order preserved.
#' @keywords internal
validate_check_info <- function(info, check_names) {
  if (!is.data.frame(info) || !all(c("check", "query") %in% names(info)))
    cli::cli_abort(c(
      "{.arg check_info} must be a data frame with columns {.field check} and {.field query}.",
      "i" = "Optional columns: {.field section} (text) and {.field critical} (logical)."))
  chk <- as.character(info$check); qry <- as.character(info$query)
  if (anyNA(chk) || !all(nzchar(trimws(chk))))
    cli::cli_abort("Every {.field check} in {.arg check_info} must be a non-empty name.")
  if (anyDuplicated(chk))
    cli::cli_abort("Duplicate {.field check} name{?s} in {.arg check_info}: {.val {unique(chk[duplicated(chk)])}}.")
  if (anyNA(qry) || !all(nzchar(trimws(qry))))
    cli::cli_abort("Every {.field query} in {.arg check_info} must be non-empty text.")
  if ("critical" %in% names(info) && !is.logical(info$critical))
    cli::cli_abort("{.field critical} in {.arg check_info} must be logical (TRUE/FALSE).")

  missing_info <- setdiff(check_names, chk)
  if (length(missing_info))
    cli::cli_warn(c("Check{?s} with no {.arg check_info} row: {.val {missing_info}}.",
                    "i" = "Their findings are written without query text."))
  unknown <- setdiff(chk, check_names)
  if (length(unknown))
    cli::cli_warn(c("{.arg check_info} row{?s} matching no check this run: {.val {unknown}}.",
                    "i" = "Kept in the index (an all-clear check still belongs in the catalogue)."))

  ord <- intersect(c("check", "section", "critical", "query"), names(info))
  info[c(ord, setdiff(names(info), ord))]
}

#' Prepend the query text as the first column of each finding frame
#'
#' The query travels with the row, so a data manager filtering or copying
#' findings (especially from a per-site workbook) keeps the DM-facing text next
#' to the record. Added AFTER any before/after comparison, so editing a query's
#' wording never makes findings read as new/resolved.
#' @param sheets Named list of findings frames.
#' @param info A validated check-info table.
#' @return `sheets` with a `query` first column on every non-empty frame whose
#'   check has an info row.
#' @keywords internal
add_query_column <- function(sheets, info) {
  clash <- names(sheets)[vapply(sheets, function(d) "query" %in% names(d), logical(1))]
  if (length(clash))
    cli::cli_abort(c(
      "Check{?s} {.val {clash}} returned a reserved column named {.field query}.",
      "i" = "With {.arg check_info}, the engine adds its own {.field query} column.",
      "x" = "Rename that column in your DVP (for example {.field query_status})."))
  for (nm in names(sheets)) {
    d <- sheets[[nm]]
    row <- match(nm, info$check)
    if (nrow(d) == 0L || is.na(row)) next
    sheets[[nm]] <- cbind(query = as.character(info$query[row]), d)
  }
  sheets
}

# --- default before-snapshot resolution -------------------------------------
#' Resolve the `before` argument of a report into a snapshot, or NULL
#'
#' A `bctu_snapshot` passes through; `NULL` means no comparison. A character
#' selector is resolved from the store the `after` snapshot lives in (falling
#' back to the project store). The default selector `"penultimate"` resolves
#' to the snapshot immediately BEFORE `after` in that store, so a rerun on an
#' older snapshot compares against its own predecessor, never against newer
#' data. Every resolution failure (no store, no earlier snapshot, or an
#' earlier directory that cannot be loaded, e.g. a pre-rebuild snapshot with
#' no manifest) falls back to `NULL` with a message, never an error: an
#' uncompared report is always preferable to no report.
#' @param before A `bctu_snapshot`, a character selector, or `NULL`.
#' @param after The current snapshot the report is about.
#' @param verbose Verbosity.
#' @return A `bctu_snapshot`, or `NULL` for no comparison.
#' @keywords internal
resolve_before_snapshot <- function(before, after, verbose = 1L) {
  # anything that is not a selector string (a snapshot, or any dataset the
  # DVP accepts) passes through untouched; NULL stays "no comparison"
  if (!is.character(before)) return(before)
  if (!is_string(before))
    cli::cli_abort("{.arg before} must be a snapshot, a selector string, or NULL.")

  fall_back <- function(reason) {
    if (verbose >= 1L)
      cli::cli_alert_info("No comparison: {reason}. Issuing the report uncompared.")
    NULL
  }

  after_dir <- attr(after, "dir")
  store <- if (!is.null(after_dir)) dirname(after_dir)
           else tryCatch(snapshot_store(verbose = 0L), error = function(e) NULL)
  if (is.null(store) || !dir.exists(store))
    return(fall_back("no snapshot store could be resolved"))

  which <- before
  if (identical(before, "penultimate")) {
    after_id <- attr(after, "id")
    ids <- tryCatch(list_snapshots(store), error = function(e) character(0))
    earlier <- if (!is.null(after_id)) ids[ids < after_id] else ids[-length(ids)]
    if (!length(earlier))
      return(fall_back(paste0("no earlier snapshot in ", store)))
    which <- max(earlier)
  }

  snap <- tryCatch(suppressWarnings(load_snapshot(which = which, store = store, verbose = 0L)),
                   error = function(e) NULL)
  if (is.null(snap))
    return(fall_back(paste0("the earlier snapshot ", which,
                            " could not be loaded (it may predate the rebuilt package)")))
  if (verbose >= 1L)
    cli::cli_alert_info("comparing against snapshot {.val {attr(snap, 'id')}}")
  snap
}

# --- snapshot fingerprint ---------------------------------------------------
#' A single integrity fingerprint for a saved snapshot
#'
#' Derived from the per-table SHA-256 values in the snapshot's own manifest, so
#' it does not re-hash the payload and matches what the snapshot recorded.
#' @param snapshot A saved `bctu_snapshot` (must carry its on-disk directory).
#' @return A hex string, or `NA` if the snapshot is not on disk.
#' @export
snapshot_fingerprint <- function(snapshot) {
  dir <- attr(snapshot, "dir")
  if (is.null(dir) || !dir.exists(dir)) return(NA_character_)
  man <- yaml::read_yaml(file.path(dir, manifest_filename))
  shas <- unlist(lapply(man$tables, function(t)
    vapply(t$files, function(f) f$sha256 %||% "", character(1))))
  digest::digest(sort(unname(shas)), algo = "sha256")
}

# --- trial name and per-site resolution ------------------------------------
#' The study/trial name carried on a snapshot
#'
#' Read from the snapshot's own metadata (which the datasource sets), so a
#' report never needs the trial name passed in by hand. Falls back to
#' `"snapshot"` when a snapshot carries no name.
#' @param snapshot A `bctu_snapshot`.
#' @return A filesystem-safe study/trial token.
#' @export
report_trial_name <- function(snapshot) {
  meta <- attr(snapshot, "bctu_meta")
  sanitise_study_name(if (is.list(meta)) meta$name else NULL)
}

#' Group label for each finding row, from the findings themselves or a snapshot
#'
#' A finding that carries `group_col` as one of its own columns is sited from
#' that column directly, row by row (a trial whose dataset has no single id
#' column can still split per site by selecting the site into each finding).
#' Rows without their own site are resolved from the snapshot(s): any table
#' holding both `id_col` and `group_col` maps a record to a site. Pass both the
#' `before` and `after` snapshots when comparing, so a `resolved` finding whose
#' record was removed from `after` is still sited from `before`. Findings with
#' no site by either route are labelled `NO_SITE` so they are never silently
#' dropped from the split.
#' @param findings A findings data frame.
#' @param snapshots A list of snapshots to union (earlier snapshots take
#'   priority when a record's site conflicts across snapshots).
#' @param id_col Name of the record-id column shared by findings and data.
#' @param group_col Name of the grouping column (a site, a country, ...) in the
#'   findings and/or the data.
#' @return A character vector of sites, one per finding row.
#' @export
resolve_finding_sites <- function(findings, snapshots, id_col, group_col) {
  own <- if (group_col %in% names(findings)) as.character(findings[[group_col]])
         else rep(NA_character_, nrow(findings))
  map <- character(0)
  for (snap in snapshots) {
    for (nm in names(snap)) {
      tab <- snap[[nm]]
      if (is.data.frame(tab) && all(c(id_col, group_col) %in% names(tab))) {
        add <- stats::setNames(as.character(tab[[group_col]]), as.character(tab[[id_col]]))
        map <- c(map, add[!names(add) %in% names(map)])
      }
    }
  }
  ids <- if (id_col %in% names(findings)) as.character(findings[[id_col]])
         else rep(NA_character_, nrow(findings))
  looked_up <- unname(map[ids])
  site <- ifelse(!is.na(own) & nzchar(own), own, looked_up)
  site[is.na(site) | !nzchar(site)] <- "NO_SITE"
  site
}

#' Group labels for each finding row, one column per grouping variable
#'
#' Calls [resolve_finding_sites()] once per grouping column, so every level is
#' resolved the same way: from the finding's own column when it has one,
#' otherwise from the snapshot(s) by record id.
#' @param findings A findings data frame.
#' @param snapshots A list of snapshots to union.
#' @param id_col Name of the record-id column shared by findings and data.
#' @param split_by Grouping columns, outermost first.
#' @return A character matrix, one row per finding and one column per entry in
#'   `split_by`.
#' @export
finding_group_labels <- function(findings, snapshots, id_col, split_by) {
  labels <- lapply(split_by, function(col)
    resolve_finding_sites(findings, snapshots, id_col, col))
  matrix(unlist(labels), nrow = nrow(findings), ncol = length(split_by),
         dimnames = list(NULL, split_by))
}

#' Validate the grouping columns for the report split
#'
#' @param split_by Grouping columns, outermost first, or `NULL` for no split.
#' @return A character vector of column names, or `NULL`.
#' @keywords internal
resolve_split_by <- function(split_by) {
  if (is.null(split_by)) return(NULL)
  split_by <- as.character(split_by)
  if (!length(split_by) || anyNA(split_by) || !all(nzchar(split_by)))
    cli::cli_abort(c(
      "{.arg split_by} must name at least one column.",
      "i" = "Give the grouping columns outermost first, e.g. {.code c(\"country\", \"site\")}."))
  split_by
}

#' Check that the grouping levels nest
#'
#' Every value of an inner grouping column must sit under one value of the
#' column outside it: a site that appears under two countries would have its
#' queries split across two workbooks. Unresolved (`NO_SITE`) inner values are
#' exempt, since a finding can carry a country but no site.
#' @param labels A list of group-label matrices (see [finding_group_labels()]).
#' @return Invisibly `TRUE`; errors when a level does not nest.
#' @keywords internal
check_split_nesting <- function(labels) {
  all_labels <- do.call(rbind, unname(labels))
  if (is.null(all_labels) || ncol(all_labels) < 2L) return(invisible(TRUE))
  for (k in 2:ncol(all_labels)) {
    inner <- all_labels[, k]; outer <- all_labels[, k - 1L]
    pairs <- unique(cbind(outer, inner)[inner != "NO_SITE", , drop = FALSE])
    dup <- unique(pairs[duplicated(pairs[, 2L]), 2L])
    if (length(dup))
      cli::cli_abort(c(
        "{.field {colnames(all_labels)[k]}} does not nest within {.field {colnames(all_labels)[k - 1L]}}.",
        "x" = "{.val {dup}} appear{?s/} under more than one {.field {colnames(all_labels)[k - 1L]}}.",
        "i" = "Each inner group must belong to exactly one outer group; check the grouping columns in the data."))
  }
  invisible(TRUE)
}

#' Write the nested per-group report sets
#'
#' One folder per value of this level's grouping column, holding that group's
#' workbook (and readable copies when asked for), then the same again for the
#' next level down. With one grouping column this is a folder per site.
#' @param sheets A named list of findings data frames (empty checks omitted).
#' @param labels A named list of group-label matrices, one per entry of
#'   `sheets` (see [finding_group_labels()]).
#' @param dir Directory to write this level into.
#' @param base_name File stem, extended with each group's name as levels nest.
#' @param level Index of the grouping column this call writes.
#' @param write_readable Also write per-check CSV/TXT copies?
#' @param check_info Optional validated check-info table.
#' @return Invisibly, the directory.
#' @keywords internal
write_split_sets <- function(sheets, labels, dir, base_name, level,
                             write_readable = FALSE, check_info = NULL) {
  n_levels <- ncol(labels[[1L]])
  values <- sort(unique(unlist(lapply(names(sheets), function(nm) labels[[nm]][, level]))))

  for (value in values) {
    per_check <- list(); per_labels <- list()
    for (nm in names(sheets)) {
      keep <- labels[[nm]][, level] == value
      if (any(keep)) {
        per_check[[nm]]  <- sheets[[nm]][keep, , drop = FALSE]
        per_labels[[nm]] <- labels[[nm]][keep, , drop = FALSE]
      }
    }
    if (!length(per_check)) next

    safe_value <- gsub("[^A-Za-z0-9_-]+", "_", value)
    group_dir  <- file.path(dir, safe_value)
    group_name <- paste0(base_name, "_", safe_value)
    dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(write_readable)) {
      write_findings_readable(per_check, group_dir)
      if (!is.null(check_info)) write_checks_index(check_info, group_dir)
    }
    write_findings_workbook(per_check,
      file.path(group_dir, paste0(group_name, ".xlsx")), index = check_info)

    if (level < n_levels)
      write_split_sets(per_check, per_labels, group_dir, group_name, level + 1L,
                       write_readable = write_readable, check_info = check_info)
  }
  invisible(dir)
}

# --- writers ----------------------------------------------------------------
#' Write each check's findings as a readable CSV and plain-text copy
#'
#' One pair of files per check, named by the check. Gives a fully testable,
#' Excel-free record of every finding. Names that sanitise to the same file
#' stem (e.g. `"a/b"` and `"a b"` both sanitise to `"a_b"`) are de-duplicated
#' with a trailing underscore, the same way the workbook sheet names are.
#' @param sheets A named list of findings data frames.
#' @param dir Directory to write into (created if missing).
#' @return Invisibly, the directory.
#' @export
write_findings_readable <- function(sheets, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  used <- character(0)
  for (nm in names(sheets)) {
    safe <- gsub("[^A-Za-z0-9_-]+", "_", nm)
    while (safe %in% used) safe <- paste0(safe, "_")
    used <- c(used, safe)
    df <- as.data.frame(sheets[[nm]])
    utils::write.csv(df, file.path(dir, paste0(safe, ".csv")), row.names = FALSE, na = "")
    txt <- if (nrow(df)) utils::capture.output(print(df, row.names = FALSE)) else "(no findings)"
    writeLines(txt, file.path(dir, paste0(safe, ".txt")))
  }
  invisible(dir)
}

#' Write a set of checks to one Excel workbook, one worksheet per check
#'
#' A convenience only: the CSV/TXT copies carry the same content, so the DVP
#' logic is fully testable without Excel. Uses `openxlsx` if installed; if not,
#' warns and does nothing. Worksheet names are the check names, trimmed to
#' Excel's 31-character limit (de-duplicated if trimming collides).
#' @param sheets A named list of findings data frames.
#' @param path Workbook path (`.xlsx`).
#' @param index Optional check-info table written as a leading `checks_index`
#'   worksheet (query text catalogue; no counts, so it is identical between a
#'   full and an update set).
#' @return Invisibly, the path (or `NULL` if `openxlsx` is unavailable or there
#'   is nothing to write).
#' @export
write_findings_workbook <- function(sheets, path, index = NULL) {
  if (!length(sheets)) return(invisible(NULL))
  if (!requireNamespace("openxlsx", quietly = TRUE))
    cli::cli_abort(c(
      "The {.pkg openxlsx} package is required to write the findings workbook.",
      "i" = "Install it with {.code install.packages(\"openxlsx\")}, then rerun."))
  wb <- openxlsx::createWorkbook()
  used <- character(0)
  if (!is.null(index)) {
    openxlsx::addWorksheet(wb, "checks_index")
    openxlsx::writeData(wb, "checks_index", as.data.frame(index))
    used <- "checks_index"
  }
  for (nm in names(sheets)) {
    sheet <- unique_sheet_name(nm, used)
    used <- c(used, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, as.data.frame(sheets[[nm]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = FALSE)
  invisible(path)
}

#' Write the checks index (per-check query text) as CSV and TXT
#'
#' One catalogue of the DVP's checks in DM-facing language, written alongside
#' the findings in every report set and per-site directory, so query text is
#' never stranded away from the findings it explains. Lists every check in the
#' info table, including all-clear checks with no findings this run.
#' @param info A validated check-info table (see [validate_check_info()]).
#' @param dir Directory to write into.
#' @return Invisibly, the directory.
#' @keywords internal
write_checks_index <- function(info, dir) {
  df <- as.data.frame(info)
  utils::write.csv(df, file.path(dir, "checks_index.csv"), row.names = FALSE, na = "")
  writeLines(utils::capture.output(print(df, row.names = FALSE)),
             file.path(dir, "checks_index.txt"))
  invisible(dir)
}

#' Make an Excel-safe, unique worksheet name (<= 31 chars)
#'
#' Excel worksheet names are capped at 31 characters and cannot repeat. This
#' sanitises the name and, on a clash, appends a numeric suffix that SHORTENS the
#' stem so the result always changes and always stays within 31 characters (the
#' naive "append and re-truncate" never terminates once the name is already 31).
#' @keywords internal
unique_sheet_name <- function(nm, used) {
  base <- substr(gsub("[^A-Za-z0-9_ -]", "_", nm), 1L, 31L)
  if (!nzchar(base)) base <- "sheet"
  cand <- base
  i <- 1L
  while (cand %in% used) {
    sfx  <- paste0("_", i)
    cand <- paste0(substr(base, 1L, 31L - nchar(sfx)), sfx)
    i <- i + 1L
  }
  cand
}

#' Drop checks with no findings
#'
#' Empty checks get no worksheet, matching the delivered DVR (an all-clear check
#' is not a blank tab). The names are preserved for the checks that remain.
#' @param sheets A named list of findings data frames.
#' @return The subset of `sheets` with at least one row.
#' @export
nonempty_checks <- function(sheets) {
  sheets[vapply(sheets, function(d) nrow(d) > 0L, logical(1))]
}

#' Write one report set: an overall workbook plus (optionally) a group split
#'
#' The delivered record is one Excel workbook per set: an overall workbook of
#' all findings (one worksheet per non-empty check), plus a workbook per group
#' under `sites/` when `split_by` is given, so a centre receives only its own
#' queries alongside the overall set. With more than one grouping column the
#' folders nest, outermost column first, and every level gets its own workbook:
#' `split_by = c("country", "site")` writes a folder per country holding that
#' country's workbook, and inside it a folder and workbook per site.
#' Per-check CSV/TXT copies are written only when `write_readable` is `TRUE`.
#' Groups are resolved from `snapshot`, and from `before_snapshot`
#' too when given, so a `resolved` finding whose record was removed from
#' `snapshot` is still grouped correctly. A `cli_warn` is raised per check that
#' has findings with no resolvable group.
#' @param sheets A named list of findings data frames (empty checks omitted).
#' @param snapshot The snapshot the findings came from (for group lookup).
#' @param dir Directory to write this set into.
#' @param base_name File stem for the workbooks.
#' @param id_col Record-id column used to map findings to groups.
#' @param split_by Grouping columns, outermost first; `NULL` (default) writes
#'   the overall workbook only.
#' @param write_readable Also write per-check CSV/TXT copies of the findings?
#'   Default `FALSE`: the workbook is the delivered record.
#' @param before_snapshot Optional earlier snapshot, unioned with `snapshot`
#'   for group lookup (see [resolve_finding_sites()]).
#' @param check_info Optional validated check-info table; when given, the
#'   checks index (sheet, CSV and TXT) is written with this set and every
#'   per-group output, so query text always accompanies the findings.
#' @return Invisibly, the directory.
#' @export
write_report_set <- function(sheets, snapshot, dir, base_name,
                             id_col = "record_id", split_by = NULL,
                             write_readable = FALSE, before_snapshot = NULL,
                             check_info = NULL) {
  split_by <- resolve_split_by(split_by)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  filled <- nonempty_checks(sheets)

  if (isTRUE(write_readable)) {
    write_findings_readable(sheets, dir)
    if (!is.null(check_info)) write_checks_index(check_info, dir)
  }

  if (length(filled))
    write_findings_workbook(filled, file.path(dir, paste0(base_name, ".xlsx")),
                            index = check_info)

  if (!is.null(split_by) && length(filled)) {
    snapshots <- if (is.null(before_snapshot)) list(snapshot) else list(before_snapshot, snapshot)
    labels <- lapply(filled, function(d)
      finding_group_labels(d, snapshots, id_col, split_by))
    check_split_nesting(labels)

    for (nm in names(filled)) for (col in split_by) {
      no_group_n <- sum(labels[[nm]][, col] == "NO_SITE")
      if (no_group_n > 0L)
        cli::cli_warn(c(
          "Check {.val {nm}}: {no_group_n} finding{?s} could not be mapped to a {.field {col}}.",
          "i" = "Written under {.val NO_SITE} in the split."))
    }

    write_split_sets(filled, labels, file.path(dir, "sites"), base_name, level = 1L,
                     write_readable = write_readable, check_info = check_info)
  }
  invisible(dir)
}

# --- the readable summary ---------------------------------------------------
summary_filename <- "summary.md"

#' Tally finding rows by change status
#'
#' @param status The `status` values of a set of finding rows, or any vector of
#'   the same length when the report is uncompared.
#' @param compared Was the report compared to a before snapshot?
#' @return A named integer vector: `Findings` when uncompared; otherwise
#'   `Current` (new plus unchanged), `New`, `Unchanged` and `Resolved`.
#' @keywords internal
status_tally <- function(status, compared) {
  if (!compared) return(c(Findings = length(status)))
  c(Current   = sum(status != "resolved"),
    New       = sum(status == "new"),
    Unchanged = sum(status == "unchanged"),
    Resolved  = sum(status == "resolved"))
}

#' Format a markdown table
#'
#' @param header Column names. The first column is left-aligned, the rest
#'   right-aligned.
#' @param rows A list of character vectors, one per row.
#' @return A character vector of markdown lines.
#' @keywords internal
markdown_table <- function(header, rows) {
  line <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  c(line(header),
    line(c("---", rep("---:", length(header) - 1L))),
    vapply(rows, line, character(1)))
}

#' Write the readable summary of a DVR/CDI run
#'
#' A markdown companion to the YAML manifest, laid out for reading in a
#' meeting: the run's identity, a table of findings per check, and, when the
#' report is split, a table of findings per group at every level (each country,
#' then each site within it). Counts are of finding rows; a compared report
#' shows current (new plus unchanged), new, unchanged and resolved.
#' @param manifest The run's manifest list (see [run_data_report()]).
#' @param sheets The named list of findings frames, carrying `status` when
#'   compared.
#' @param labels Group-label matrices for the non-empty sheets (see
#'   [finding_group_labels()]), or `NULL` when the report is not split.
#' @param path File to write.
#' @return Invisibly, the lines written.
#' @export
write_report_summary <- function(manifest, sheets, labels = NULL, path) {
  compared <- isTRUE(manifest$compared)
  filled <- nonempty_checks(sheets)
  status_of <- function(df) if (compared) df$status else rep("", nrow(df))
  fmt <- function(tally) format(tally, big.mark = ",", trim = TRUE)
  header <- c("", names(status_tally(character(0), compared)))

  lines <- c(
    paste0("# ", manifest$dvr_id),
    "",
    markdown_table(c("", ""), list(
      c("Trial", manifest$trial),
      c("Report", toupper(manifest$kind)),
      c("Version", if (is.na(manifest$version)) "none" else manifest$version),
      c("Snapshot", manifest$after_snapshot$id),
      c("Compared to", if (compared) manifest$before_snapshot$id else "not compared"),
      c("Issued", paste(manifest$created_utc, "by", manifest$operator)))),
    "",
    "## Findings by check",
    "")

  header[1L] <- "Check"
  by_check <- lapply(names(filled), function(nm)
    c(nm, fmt(status_tally(status_of(filled[[nm]]), compared))))
  all_status <- unlist(lapply(filled, status_of), use.names = FALSE)
  by_check <- c(by_check, list(c("All checks", fmt(status_tally(all_status, compared)))))
  lines <- c(lines, markdown_table(header, by_check))

  if (!is.null(labels) && length(filled)) {
    split_by <- colnames(labels[[1L]])
    header[1L] <- "Group"
    rows <- list(c("Overall", fmt(status_tally(all_status, compared))))
    add_level <- function(keep_of, level, prefix) {
      values <- sort(unique(unlist(lapply(names(filled), function(nm)
        labels[[nm]][keep_of[[nm]], level]))))
      for (value in values) {
        keep_here <- lapply(names(filled), function(nm)
          keep_of[[nm]] & labels[[nm]][, level] == value)
        names(keep_here) <- names(filled)
        status_here <- unlist(lapply(names(filled), function(nm)
          status_of(filled[[nm]])[keep_here[[nm]]]), use.names = FALSE)
        label <- paste(c(prefix, value), collapse = " / ")
        rows[[length(rows) + 1L]] <<- c(label, fmt(status_tally(status_here, compared)))
        if (level < length(split_by)) add_level(keep_here, level + 1L, c(prefix, value))
      }
    }
    everything <- lapply(filled, function(d) rep(TRUE, nrow(d)))
    add_level(everything, 1L, character(0))
    lines <- c(lines, "",
               paste0("## Findings by ", paste(split_by, collapse = " and ")),
               "", markdown_table(header, rows))
  }

  writeLines(lines, path, useBytes = TRUE)
  invisible(lines)
}

# --- the report engine ------------------------------------------------------
#' Build a Data Validation Report (DVR) from a DVP function and a snapshot
#'
#' Runs `dvp` on the `after` snapshot and writes, under every directory in
#' `paths`, the house layout `<path>/<after snapshot id>/v<version>/` (a folder
#' per data state, then a folder per controlled document version; without a
#' `version`, `<path>/<after snapshot id>/` directly): an overall workbook (one
#' worksheet per non-empty check) plus per-group workbooks when `split_by` is
#' given, the readable CSV/TXT copies, an auditable YAML manifest and a
#' markdown `summary.md` of the counts per check and per group. A rerun
#' of the same snapshot and version suffixes the leaf folder `_N`, never
#' silently overwriting. When a `before` snapshot is supplied, each finding
#' is labelled `new` / `unchanged` / `resolved` by a whole-row comparison of the
#' two runs (see [compare_dvp()]) and the change labelling is written per
#' `status_output`: a separate `new/` folder alongside `full/` (the default),
#' or a `status` column plus an `update/` set. Resolved findings show the
#' `before` snapshot's data values, so they are only written when
#' `include_resolved = TRUE`. The trial name is
#' taken from the snapshot itself (see [report_trial_name()]); nothing about the
#' trial has to be passed in.
#' @param dvp A DVP function: `function(data)` returning a named list of
#'   findings (see [run_dvp()]).
#' @param after The current snapshot the report is about.
#' @param before The earlier snapshot to compare against. Default
#'   `"penultimate"`: the snapshot immediately before `after` in its store is
#'   loaded automatically, so findings carry the change labelling on every
#'   routine run; when that resolution fails (a store with one snapshot, or an
#'   earlier directory the package cannot read, e.g. pre-rebuild snapshots),
#'   the report is issued uncompared with a message, never an error. Pass a
#'   `bctu_snapshot` to compare against explicitly, another selector string to
#'   resolve it from the store, or `NULL` for no comparison.
#' @param paths One or more directories to receive the report (the report is
#'   written to each). Defaults to the working directory.
#' @param id_col Record-id column used to map findings to groups. Default
#'   `"record_id"`.
#' @param split_by Columns in the data to split the report by, outermost
#'   first; when `NULL` (default) no split is written. One column writes a
#'   folder and workbook per site; `c("country", "site")` writes a folder and
#'   workbook per country, and a folder and workbook per site inside it.
#' @param version Optional DVP version string, recorded and added to file names.
#' @param operator Person issuing the report (recorded); default the OS user.
#' @param check_info Optional data frame describing the checks in DM-facing
#'   language: columns `check` (matching the names the DVP returns) and `query`
#'   (the text a data manager acts on), plus optional `section` (free text) and
#'   `critical` (logical). When given, a `checks_index` sheet leads every
#'   workbook (overall and per-site), `checks_index.csv`/`.txt` are written
#'   alongside the findings in every output directory, and each check's query,
#'   section and criticality are recorded in the manifest. The index lists
#'   every check in the table, including all-clear checks with no findings.
#'   When `NULL`, the DVP function may supply the same table itself via
#'   `attr(findings, "check_info")` on the list it returns; an explicit
#'   `check_info` argument wins over the attribute.
#' @param query_column Prepend each check's query text as the first column of
#'   its finding rows (so the text travels with a row into a data manager's
#'   query log)? Default `TRUE` when check info is available. The column is
#'   added after any before/after comparison, so rewording a query never makes
#'   findings read as new or resolved. A check returning its own `query` column
#'   is an error while check info is in use.
#' @param status_output How a compared report presents the change labelling.
#'   `"folders"` (the default) writes sets side by side: `full/` (every current
#'   finding, new and unchanged) and `new/` (findings not in `before`), plus
#'   `resolved/` when `include_resolved = TRUE`; the written files carry no
#'   `status` column because the folder says which set a row is in. Findings
#'   in `full/` but not in `new/` are unchanged from `before`. `"column"`
#'   writes `full/` with a `status` column plus an `update/` set of the
#'   changed rows. Ignored when there is no `before` snapshot. The returned
#'   `sheets` keep the `status` column either way, and the manifest records the
#'   per-check tallies (including resolved) either way.
#' @param include_resolved Also write the findings resolved since `before`
#'   (rows present in `before` but gone from `after`)? Default `TRUE`:
#'   `"folders"` adds a `resolved/` set and `"column"` keeps resolved rows
#'   (labelled `resolved`) in `full/` and the `update/` set. A resolved row
#'   shows the BEFORE snapshot's data values, stale against the current
#'   extract; set `FALSE` to leave resolved rows out of the written files.
#' @param write_readable Also write per-check CSV/TXT copies of the findings?
#'   Default `FALSE`: the delivered record is the workbook (one worksheet per
#'   check), and `openxlsx` is required up front. The checks index and the YAML
#'   manifest are always written.
#' @param verbose Verbosity.
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @examples
#' \dontrun{
#' dvp <- function(data) list(weight_range = data$records[data$records$weight_kg > 300, ])
#' save_dvr(dvp, after = load_snapshot("latest"))
#' }
#' @export
save_dvr <- function(dvp, after, before = "penultimate", paths = getwd(),
                     id_col = "record_id", split_by = NULL, version = NULL,
                     operator = NULL, check_info = NULL, query_column = TRUE,
                     status_output = c("folders", "column"),
                     include_resolved = TRUE,
                     write_readable = FALSE, verbose = 2L) {
  run_data_report(dvp, after, before, paths, kind = "dvr", id_col = id_col,
                  split_by = split_by, version = version, operator = operator,
                  check_info = check_info, query_column = query_column,
                  status_output = status_output,
                  include_resolved = include_resolved,
                  write_readable = write_readable, verbose = verbose)
}

#' Build a Critical Data Items (CDI) report from a DVP function and a snapshot
#'
#' Identical machinery to [save_dvr()] with its own label, including the
#' default comparison against the previous snapshot.
#' @inheritParams save_dvr
#' @return Invisibly, a list with the report id, directories written, sheets,
#'   and per-check counts.
#' @examples
#' \dontrun{
#' dvp <- function(data) list(weight_range = data$records[data$records$weight_kg > 300, ])
#' save_cdi(dvp, after = load_snapshot("latest"))
#' }
#' @export
save_cdi <- function(dvp, after, before = "penultimate", paths = getwd(),
                     id_col = "record_id", split_by = NULL,
                     version = NULL, operator = NULL,
                     check_info = NULL, query_column = TRUE,
                     status_output = c("folders", "column"),
                     include_resolved = TRUE,
                     write_readable = FALSE, verbose = 2L) {
  run_data_report(dvp, after, before, paths, kind = "cdi", id_col = id_col,
                  split_by = split_by, version = version, operator = operator,
                  check_info = check_info, query_column = query_column,
                  status_output = status_output,
                  include_resolved = include_resolved,
                  write_readable = write_readable, verbose = verbose)
}

#' Shared engine behind [save_dvr()] and [save_cdi()]
#'
#' Explicitly named (no hidden helper): the DVR and CDI wrappers differ only
#' in their `kind` label.
#' @inheritParams save_dvr
#' @param kind `"dvr"` or `"cdi"`.
#' @return Invisibly, a list describing the written report.
#' @export
run_data_report <- function(dvp, after, before = "penultimate", paths = getwd(),
                            kind = c("dvr", "cdi"), id_col = "record_id",
                            split_by = NULL, version = NULL, operator = NULL,
                            check_info = NULL, query_column = TRUE,
                            status_output = c("folders", "column"),
                            include_resolved = TRUE,
                            write_readable = FALSE, verbose = 2L) {
  kind <- match.arg(kind)
  status_output <- match.arg(status_output)
  split_by <- resolve_split_by(split_by)
  if (!requireNamespace("openxlsx", quietly = TRUE))
    cli::cli_abort(c(
      "The {.pkg openxlsx} package is required: the {toupper(kind)} is delivered as one Excel workbook.",
      "i" = "Install it with {.code install.packages(\"openxlsx\")}, then rerun."))
  paths <- as.character(paths)
  if (!length(paths))
    cli::cli_abort("{.arg paths} must name at least one output directory.")
  operator <- operator %||% unname(Sys.info()[["user"]]) %||% "unknown"
  trial <- report_trial_name(after)
  before <- resolve_before_snapshot(before, after, verbose = verbose)
  compared <- !is.null(before)

  sheets <- if (compared) compare_dvp(dvp, before, after) else run_dvp(dvp, after)

  info <- check_info %||% attr(sheets, "check_info")
  if (!is.null(info)) {
    info <- validate_check_info(info, names(sheets))
    if (isTRUE(query_column)) sheets <- add_query_column(sheets, info)
  }

  # The written sets, per status_output. "folders" drops the status column from
  # every written file (the folder a row sits in carries its status); "column"
  # keeps the column and writes the changed rows as one update/ set. Resolved
  # rows carry the BEFORE snapshot's data values, so they are written only when
  # include_resolved is TRUE. The returned `sheets` keep every row and the
  # status column in both modes.
  with_status <- function(d, keep, drop_col) {
    if (!("status" %in% names(d)) || nrow(d) == 0L) d <- d[0, , drop = FALSE]
    else d <- d[d$status %in% keep, , drop = FALSE]
    if (drop_col) d$status <- NULL
    d
  }
  full_sheets <- sheets
  update_sheets <- new_sheets <- resolved_sheets <- NULL
  if (compared && status_output == "folders") {
    full_sheets <- lapply(sheets, with_status, c("new", "unchanged"), drop_col = TRUE)
    new_sheets  <- lapply(sheets, with_status, "new", drop_col = TRUE)
    if (isTRUE(include_resolved))
      resolved_sheets <- lapply(sheets, with_status, "resolved", drop_col = TRUE)
  } else if (compared) {
    if (!isTRUE(include_resolved))
      full_sheets <- lapply(sheets, with_status, c("new", "unchanged"), drop_col = FALSE)
    changed <- if (isTRUE(include_resolved)) c("new", "resolved") else "new"
    update_sheets <- lapply(sheets, with_status, changed, drop_col = FALSE)
  }

  # ---- per-check counts (with status tallies when compared) ----
  check_summaries <- lapply(names(sheets), function(nm) {
    df <- sheets[[nm]]
    rec <- list(name = nm, rows = nrow(df))
    if (compared && "status" %in% names(df)) {
      rec$new       <- sum(df$status == "new")
      rec$unchanged <- sum(df$status == "unchanged")
      rec$resolved  <- sum(df$status == "resolved")
    }
    if (!is.null(info)) {
      row <- match(nm, info$check)
      if (!is.na(row)) {
        rec$query <- as.character(info$query[row])
        if ("section" %in% names(info) && !is.na(info$section[row]))
          rec$section <- as.character(info$section[row])
        if ("critical" %in% names(info) && !is.na(info$critical[row]))
          rec$critical <- info$critical[row]
      }
    }
    rec
  })
  total_rows <- sum(vapply(sheets, nrow, integer(1)))

  # ---- one report id shared across every destination ----
  now <- utc_now()
  # The report is keyed by the DATA it validated: the id and directory carry
  # the after-snapshot's id (and the document version when given). A second
  # run on the same snapshot and version gets an explicit _N-suffixed
  # directory (the while-loop below), never a silent overwrite. The run moment
  # is recorded as created_utc in the manifest. An unsaved after snapshot has
  # no id, so the run time stands in for it.
  id <- attr(after, "id") %||% snapshot_id(now)
  ver <- if (!is.null(version)) paste0("v", sub("^[vV]", "", as.character(version))) else NULL
  dvr_id <- paste(Filter(Negate(is.null), list(toupper(kind), ver, id)), collapse = "-")
  base <- paste(Filter(nzchar, c(trial, toupper(kind), id, ver %||% "")), collapse = "_")

  versions <- list(
    bctu = tryCatch(as.character(utils::packageVersion("bctu")), error = function(e) NA_character_),
    r    = R.version.string)
  snapshot_ref <- function(s) {
    if (is.null(s)) return(NULL)
    list(id = attr(s, "id") %||% NA_character_,
         sha256 = tryCatch(snapshot_fingerprint(s), error = function(e) NA_character_))
  }
  manifest <- list(
    schema = "bctu-dvr/1",
    kind = kind,
    dvr_id = dvr_id,
    trial = trial,
    tag = (attr(after, "bctu_tag") %||% attr(after, "bctu_meta")$tag) %||% NA_character_,
    version = version %||% NA_character_,
    created_utc = iso8601(now),
    operator = operator,
    compared = compared,
    status_output = if (compared) status_output else NA_character_,
    include_resolved = if (compared) isTRUE(include_resolved) else NA,
    after_snapshot = snapshot_ref(after),
    before_snapshot = snapshot_ref(before),
    id_col = id_col,
    split_by = if (length(split_by)) as.list(split_by) else NA_character_,
    checks = check_summaries,
    total_findings = total_rows,
    versions = versions
  )

  summary_labels <- NULL
  if (length(split_by) && length(nonempty_checks(sheets))) {
    snapshots <- if (is.null(before)) list(after) else list(before, after)
    summary_labels <- lapply(nonempty_checks(sheets), function(d)
      finding_group_labels(d, snapshots, id_col, split_by))
    check_split_nesting(summary_labels)
  }

  written_dirs <- character(0)
  for (p in paths) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
    # House layout: <path>/<after-snapshot-id>/v<version>/ (a folder per data
    # state, then a folder per controlled document version); an unversioned
    # report writes <path>/<after-snapshot-id>/ directly. A rerun of the same
    # snapshot and version gets an explicit _N suffix on the leaf folder,
    # never a silent overwrite.
    base_dir <- if (!is.null(ver)) file.path(p, id) else p
    leaf     <- ver %||% id
    report_dir <- file.path(base_dir, leaf)
    suffix <- 0L
    while (dir.exists(report_dir)) {
      suffix <- suffix + 1L
      report_dir <- file.path(base_dir, paste0(leaf, "_", suffix))
    }
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

    write_report_set(full_sheets, after, file.path(report_dir, "full"), base,
                     id_col = id_col, split_by = split_by,
                     write_readable = write_readable,
                     before_snapshot = before, check_info = info)
    if (compared && status_output == "folders") {
      write_report_set(new_sheets, after, file.path(report_dir, "new"),
                       paste0(base, "_New"), id_col = id_col,
                       split_by = split_by, write_readable = write_readable,
                       before_snapshot = before, check_info = info)
      if (!is.null(resolved_sheets))
        write_report_set(resolved_sheets, after, file.path(report_dir, "resolved"),
                         paste0(base, "_Resolved"), id_col = id_col,
                         split_by = split_by, write_readable = write_readable,
                         before_snapshot = before, check_info = info)
    } else if (compared) {
      write_report_set(update_sheets, after, file.path(report_dir, "update"),
                       paste0(base, "_Update"), id_col = id_col,
                       split_by = split_by, write_readable = write_readable,
                       before_snapshot = before, check_info = info)
    }
    yaml::write_yaml(manifest, file.path(report_dir, manifest_filename))
    write_report_summary(manifest, sheets, summary_labels,
                         file.path(report_dir, summary_filename))
    written_dirs <- c(written_dirs, report_dir)
  }

  if (verbose >= 1L) {
    cli::cli_alert_success("{toupper(kind)} {.val {dvr_id}} issued -> {length(written_dirs)} location{?s}")
    if (verbose >= 2L)
      cli::cli_alert_info("{length(nonempty_checks(sheets))} check{?s} with findings, {total_rows} finding{?s}{if (compared) ' (compared to a before snapshot)' else ''}.")
  }

  invisible(list(
    dvr_id = dvr_id, kind = kind, trial = trial, dirs = written_dirs,
    compared = compared, status_output = if (compared) status_output else NA_character_,
    include_resolved = if (compared) isTRUE(include_resolved) else NA,
    sheets = sheets, update = update_sheets,
    new = new_sheets, resolved = resolved_sheets,
    checks = check_summaries, total_findings = total_rows, manifest = manifest))
}
