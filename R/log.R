# ---------------------------------------------------------------------------
# Run logs: a recorded transcript + structured record of an analysis session
# ---------------------------------------------------------------------------
# A run log is one directory per run holding a plain-text transcript
# (`run.log`), a structured YAML record (`run.yml`, schema bctu-runlog/1), and
# a `run.msg` sidecar that exists only when the run was not clean. An
# append-only `index.csv` in the log location lists every run. The record
# header is written BEFORE any work runs, so even a session killed outright
# leaves the header, the transcript up to death, and an index entry that never
# reached a closed status.
#
# The console experience does not change: stdout is duplicated (split sink),
# conditions are observed without muffling, and the command echo is appended
# by a task callback. The only visible additions are one announced line at
# start and one at close.

# The one open run log for this session (at most one; starting a second is an
# error). Runtime state, like the sink stack itself, not configuration.
run_log_state <- new.env(parent = emptyenv())

#' The provenance stamp used in run logs and snapshot manifests
#'
#' The pure computation behind [checkpoint()]: the stamp alone, with no run-log
#' interaction, used internally wherever a stamp must never have side effects
#' (snapshot manifests, the run-log header itself).
#' @param time The stamp's UTC time; default now. Passing it lets a caller use
#'   one clock read for the stamp and for anything derived from it.
#' @return A list: `created_utc`, `r_version`, `bctu_version`, `user`, `host`.
#' @keywords internal
checkpoint_stamp <- function(time = utc_now()) {
  ver <- tryCatch(as.character(utils::packageVersion("bctu")), error = function(e) NA_character_)
  list(created_utc = iso8601(time), r_version = R.version.string,
       bctu_version = ver,
       user = unname(Sys.info()[["user"]]) %||% "unknown",
       host = unname(Sys.info()[["nodename"]]) %||% "unknown")
}

#' Resolve the log location (explicit, or `<project dir>/log-output`)
#' @keywords internal
resolve_log_location <- function(location) {
  if (!is.null(location)) {
    if (!is_string(location) || !nzchar(location))
      cli::cli_abort("{.arg location} must be a single directory path.")
    return(location)
  }
  proj <- tryCatch(bctu_project(), error = function(e) NULL)
  if (is.null(proj))
    cli::cli_abort(c(
      "No bctu project marker found, so there is no default log location.",
      "i" = "Pass {.arg location} explicitly, for example {.code start_log(location = \"log-output\")}."))
  file.path(proj$root, "log-output")
}

#' Default run name: the running script's file name, else `interactive-session`
#' @keywords internal
default_run_name <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) >= 1L && nzchar(file_arg[[1L]]))
    return(sub("\\.[rR]$", "", basename(file_arg[[1L]])))
  "interactive-session"
}

#' Is a calling-handler-establishing frame on the call stack?
#'
#' `globalCallingHandlers()` refuses to run while calling handlers are on the
#' stack, and every catching wrapper (`tryCatch`, `try`,
#' `withCallingHandlers`, `suppressWarnings`, `suppressMessages`) establishes
#' one, so the refusal cannot itself be caught. This inspects the call stack
#' for those wrappers instead, so registration can be skipped gracefully
#' (e.g. under testthat) rather than erroring.
#' @return `TRUE` if registration would be refused.
#' @keywords internal
handler_frames_on_stack <- function() {
  blockers <- list(base::tryCatch, base::withCallingHandlers, base::try,
                   base::suppressWarnings, base::suppressMessages)
  for (i in seq_len(sys.nframe())) {
    fn <- sys.function(i)
    if (any(vapply(blockers, identical, logical(1), y = fn))) return(TRUE)
  }
  FALSE
}

#' Snapshot identity block for the run record
#' @keywords internal
run_log_snapshot_ref <- function(snapshot) {
  if (is.null(snapshot)) return(NULL)
  list(id = attr(snapshot, "id") %||% "unsaved",
       tag = (attr(snapshot, "bctu_tag") %||% attr(snapshot, "bctu_meta")$tag) %||% NA_character_,
       sha256 = tryCatch(snapshot_fingerprint(snapshot), error = function(e) NA_character_))
}

#' File inventory (path, size, SHA-256) for inputs and outputs
#' @keywords internal
run_log_file_inventory <- function(paths) {
  if (is.null(paths)) return(NULL)
  paths <- as.character(paths)
  files <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else p
  }))
  files <- files[file.exists(files) & !dir.exists(files)]
  lapply(files, function(f) list(
    path = f, size_bytes = as.integer(file.info(f)$size), sha256 = sha256_file(f)))
}

#' Atomic write of the run record YAML
#' @keywords internal
write_run_record <- function(record, dir) {
  tmp <- file.path(dir, "run.yml.tmp")
  yaml::write_yaml(record, tmp)
  file.rename(tmp, file.path(dir, "run.yml"))
  invisible(record)
}

#' Start a run log
#'
#' Opens a recorded session: from this call until [stop_log()], everything the
#' session prints is copied to a transcript on disc, warnings and errors are
#' tallied, each top-level command is appended, and a structured YAML record
#' accompanies the transcript. Nothing about the console experience changes:
#' output, messages, warnings and errors still appear exactly as before; the
#' transcript is a copy, not a diversion.
#'
#' The record header is written to disc BEFORE this function returns, in
#' status `started`, so a session that later dies still leaves the header and
#' the transcript up to the point of death; a record that never reached
#' `closed` is itself evidence of an interrupted run.
#'
#' Command echo is appended after each top-level expression completes, so a
#' command appears in the transcript after its own output (an R limitation:
#' there is no pre-execution hook). Warning/error/message capture depends on
#' the context, recorded as `condition_capture` in the record: at top level
#' (console, `Rscript`) R's global condition handlers are used (`"global"`);
#' inside a knitr/R Markdown render the handlers are installed through
#' knitr's own `calling.handlers` chunk option (`"knitr"`; effective from the
#' chunk after the one calling `start_log()`); and when the call is wrapped
#' in `tryCatch()` or similar, where R refuses global handlers, `start_log()`
#' says so and records the transcript without condition tallies (`"none"`).
#' Input that is not a top-level expression (e.g. `readline()` answers) and
#' graphics are not captured.
#'
#' @param location Directory to hold run logs. Default: `log-output/` under
#'   the bctu project root when a project marker is found; outside a project
#'   it must be given explicitly.
#' @param name Run name; becomes the record directory's stem. Default: the
#'   running script's file name under `Rscript`, else `interactive-session`.
#' @param snapshot Optional `bctu_snapshot` this run works on; its id, tag and
#'   SHA-256 fingerprint are recorded.
#' @param params Optional named list of run parameters, recorded verbatim.
#' @param inputs Optional character vector of input files or directories; each
#'   file's SHA-256 is recorded.
#' @param verbose Verbosity; `0` suppresses the announcement line.
#' @return Invisibly, the run id. The run directory is
#'   `<location>/<name>-<UTC time>` (suffixed `-N` on a same-second collision),
#'   where the time equals the header checkpoint's `created_utc`.
#' @examples
#' \dontrun{
#' start_log()
#' # ... analysis ...
#' checkpoint()
#' # ... more analysis ...
#' stop_log()
#' }
#' @export
start_log <- function(location = NULL, name = NULL, snapshot = NULL,
                      params = NULL, inputs = NULL, verbose = 1L) {
  if (!is.null(run_log_state$log))
    cli::cli_abort(c(
      "A run log is already open: {.val {run_log_state$log$id}}.",
      "i" = "Close it with {.fn stop_log} before starting another."))

  now   <- utc_now()
  stamp <- checkpoint_stamp(now)

  location <- resolve_log_location(location)
  if (!dir.exists(location)) dir.create(location, recursive = TRUE, showWarnings = FALSE)

  name <- name %||% default_run_name()
  if (!is_string(name) || !nzchar(name))
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  safe_name <- gsub("[^A-Za-z0-9._-]+", "-", name)

  id  <- paste0(safe_name, "-", snapshot_id(now))
  dir <- file.path(location, id)
  suffix <- 0L
  while (dir.exists(dir)) {
    suffix <- suffix + 1L
    dir <- file.path(location, paste0(id, "-", suffix))
  }
  if (suffix > 0L) id <- basename(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  loaded <- sort(loadedNamespaces())
  packages <- stats::setNames(as.list(vapply(loaded, function(p)
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_),
    character(1))), loaded)

  record <- list(
    schema = "bctu-runlog/1",
    id = id,
    name = name,
    status = "started",
    checkpoint = stamp,
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    platform = R.version$platform,
    packages = packages,
    snapshot = run_log_snapshot_ref(snapshot),
    params = params,
    inputs = run_log_file_inventory(inputs),
    checkpoints = list()
  )
  record <- Filter(Negate(is.null), record)
  write_run_record(record, dir)

  con <- file(file.path(dir, "run.log"), open = "at")
  header <- c(
    paste0("=== bctu run log ", id, " ==="),
    paste0("started_utc: ", stamp$created_utc),
    paste0("user: ", stamp$user, "@", stamp$host),
    paste0(stamp$r_version, "; bctu ", stamp$bctu_version),
    "")
  writeLines(header, con); flush(con)

  sink_depth_before <- sink.number()
  sink(con, split = TRUE)

  log <- new.env(parent = emptyenv())
  log$closed <- FALSE

  counts <- new.env(parent = emptyenv())
  counts$warnings <- 0L; counts$errors <- 0L
  # Handlers check the log's closed flag first: when deregistration is not
  # possible at close time (handler frames on the stack), the handlers are
  # left registered but inert.
  condition_line <- function(kind) {
    force(kind)
    function(cnd) {
      if (isTRUE(log$closed)) return(invisible(NULL))
      if (kind == "warning") counts$warnings <- counts$warnings + 1L
      if (kind == "error")   counts$errors   <- counts$errors + 1L
      msg <- tryCatch(conditionMessage(cnd), error = function(e) "<unreadable condition>")
      cat(paste0("## [", kind, " ", iso8601(), "] ", trimws(msg), "\n"), file = con)
      flush(con)
      invisible(NULL)
    }
  }
  handlers <- list(message = condition_line("message"),
                   warning = condition_line("warning"),
                   error   = condition_line("error"))
  # Three condition-capture routes, recorded as `condition_capture` in the
  # record. Inside knitr the handlers are installed through knitr's own
  # calling.handlers chunk option (knitr owns its evaluation loop; effective
  # from the next chunk). Otherwise globalCallingHandlers() is called bare: it
  # cannot be guarded by any catching wrapper (the wrapper is exactly what it
  # refuses to run under), so the stack is inspected first and capture is
  # skipped, announced, where registration would be refused (the transcript
  # then records output and commands only).
  capture <- "none"
  if (isTRUE(getOption("knitr.in.progress")) &&
      requireNamespace("knitr", quietly = TRUE)) {
    log$prior_chunk_handlers <- knitr::opts_chunk$get("calling.handlers")
    knitr::opts_chunk$set(calling.handlers = c(handlers, log$prior_chunk_handlers))
    capture <- "knitr"
  } else if (!handler_frames_on_stack()) {
    do.call(globalCallingHandlers, handlers)
    capture <- "global"
  } else if (verbose >= 1L) {
    cli::cli_alert_warning("Warnings/errors cannot be tallied here (called under tryCatch or similar); the transcript still records output and commands.")
  }

  echo_callback <- function(expr, value, ok, visible) {
    lines <- tryCatch(deparse(expr), error = function(e) "<undeparseable expression>")
    cat(paste0("> ", lines, collapse = "\n"), "\n", sep = "", file = con)
    flush(con)
    TRUE
  }
  callback_name <- addTaskCallback(echo_callback, name = paste0("bctu-run-log-", id))

  log$id <- id; log$dir <- dir; log$con <- con
  log$record <- record; log$counts <- counts
  log$handlers <- handlers; log$capture <- capture
  log$callback_name <- names(callback_name) %||% paste0("bctu-run-log-", id)
  log$sink_depth_before <- sink_depth_before
  log$location <- location
  run_log_state$log <- log

  # An unclean R exit (quit(), session end) marks the record interrupted where
  # R still runs finalizers; a hard kill leaves the header in status started.
  reg.finalizer(log, function(e) {
    if (isTRUE(e$closed)) return(invisible(NULL))
    e$record$status <- "interrupted"
    e$record$ended_utc <- iso8601()
    tryCatch(write_run_record(e$record, e$dir), error = function(err) NULL)
    tryCatch(append_run_index(e$location, e$record, e$counts), error = function(err) NULL)
    invisible(NULL)
  }, onexit = TRUE)

  if (verbose >= 1L)
    cli::cli_alert_info("run log started -> {.file {dir}}")
  invisible(id)
}

#' Stop the open run log
#'
#' Closes the recorded session: removes the capture hooks (the console was
#' never altered, so nothing visibly changes back), finalises the YAML record
#' with end time, status and tallies, records an inventory of `outputs` files
#' with SHA-256, appends the run to the location's `index.csv`, and writes the
#' `run.msg` sidecar if the run was not clean.
#' @param outputs Optional character vector of output files or directories to
#'   inventory (path, size, SHA-256) in the record.
#' @param verbose Verbosity; `0` suppresses the announcement line.
#' @return Invisibly, the path of the run's record directory.
#' @export
stop_log <- function(outputs = NULL, verbose = 1L) {
  log <- run_log_state$log
  if (is.null(log))
    cli::cli_abort(c("No run log is open.",
                     "i" = "Start one with {.fn start_log}."))

  removeTaskCallback(log$callback_name)
  if (identical(log$capture, "knitr")) {
    knitr::opts_chunk$set(calling.handlers = log$prior_chunk_handlers)
  } else if (identical(log$capture, "global") && !handler_frames_on_stack()) {
    # deregister exactly the handlers this log added, leaving any others; when
    # the stack blocks deregistration the handlers stay registered but inert
    # (closed flag), which is harmless
    current <- globalCallingHandlers()
    globalCallingHandlers(NULL)
    keep <- current[!vapply(current, function(h)
      any(vapply(log$handlers, identical, logical(1), y = h)), logical(1))]
    if (length(keep)) do.call(globalCallingHandlers, keep)
  }

  # pop this log's sink (and any sinks left open above it, with a warning)
  extra <- 0L
  while (sink.number() > log$sink_depth_before) {
    sink(NULL)
    extra <- extra + 1L
  }
  if (extra > 1L)
    cli::cli_warn("{extra - 1L} sink{?s} left open inside the run were closed.")
  close(log$con)

  status <- if (log$counts$errors > 0L) "errors"
            else if (log$counts$warnings > 0L) "warnings"
            else "clean"
  log$record$status <- status
  log$record$ended_utc <- iso8601()
  log$record$warnings <- log$counts$warnings
  log$record$errors <- log$counts$errors
  log$record$condition_capture <- log$capture
  out_inv <- run_log_file_inventory(outputs)
  if (!is.null(out_inv)) log$record$outputs <- out_inv
  log$record$transcript_sha256 <- sha256_file(file.path(log$dir, "run.log"))
  write_run_record(log$record, log$dir)

  append_run_index(log$location, log$record, log$counts)

  if (status != "clean")
    writeLines(c(paste0("run ", log$record$id, " finished with status: ", status),
                 paste0("warnings: ", log$counts$warnings, "; errors: ", log$counts$errors)),
               file.path(log$dir, "run.msg"))

  log$closed <- TRUE
  run_log_state$log <- NULL
  if (verbose >= 1L)
    cli::cli_alert_success("run log {status}: {.file {log$dir}}")
  invisible(log$dir)
}

#' Append one run's line to the location's append-only index
#' @keywords internal
append_run_index <- function(location, record, counts) {
  index <- file.path(location, "index.csv")
  row <- data.frame(
    id = record$id, name = record$name,
    user = record$checkpoint$user, host = record$checkpoint$host,
    started_utc = record$checkpoint$created_utc,
    ended_utc = record$ended_utc %||% NA_character_,
    status = record$status,
    warnings = counts$warnings, errors = counts$errors,
    stringsAsFactors = FALSE)
  utils::write.table(row, index, sep = ",", row.names = FALSE,
                     col.names = !file.exists(index), append = file.exists(index),
                     qmethod = "double")
  invisible(index)
}

#' List the runs recorded at a log location
#' @param location The log location; default resolved as in [start_log()].
#' @return A data frame, one row per recorded run (empty if none yet).
#' @export
list_logs <- function(location = NULL) {
  location <- resolve_log_location(location)
  index <- file.path(location, "index.csv")
  if (!file.exists(index))
    return(data.frame(id = character(0), name = character(0), user = character(0),
                      host = character(0), started_utc = character(0),
                      ended_utc = character(0), status = character(0),
                      warnings = integer(0), errors = integer(0),
                      stringsAsFactors = FALSE))
  utils::read.csv(index, stringsAsFactors = FALSE)
}

#' Read one run's structured record
#' @param id The run id (as listed by [list_logs()]).
#' @param location The log location; default resolved as in [start_log()].
#' @return The run record as a list (the parsed `run.yml`).
#' @export
read_log <- function(id, location = NULL) {
  location <- resolve_log_location(location)
  path <- file.path(location, id, "run.yml")
  if (!file.exists(path))
    cli::cli_abort(c("No run record found for {.val {id}} at {.file {location}}.",
                     "i" = "List the recorded runs with {.fn list_logs}."))
  yaml::read_yaml(path)
}

#' Write a checkpoint stamp into the open run log
#' @keywords internal
record_checkpoint <- function(log, stamp) {
  writeLines(c("",
               paste0("=== checkpoint ", stamp$created_utc, " ==="),
               paste0("user: ", stamp$user, "@", stamp$host),
               paste0(stamp$r_version, "; bctu ", stamp$bctu_version),
               ""), log$con)
  flush(log$con)
  log$record$checkpoints <- c(log$record$checkpoints, list(stamp))
  write_run_record(log$record, log$dir)
  invisible(stamp)
}
