# In-process tests cover the record structure and lifecycle; the condition
# capture and crash behaviour need a top-level R session (globalCallingHandlers
# cannot register under testthat's own handlers), so those run in child
# Rscript processes.

# Child processes load the INSTALLED bctu; when the library holds a version
# without run logs (e.g. devtools::test against source), the child tests skip.
child_bctu_has_run_logs <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    out <- tryCatch(system2(file.path(R.home("bin"), "Rscript"),
      c("-e", shQuote("cat(exists('start_log', asNamespace('bctu')))")),
      stdout = TRUE, stderr = TRUE), error = function(e) "FALSE")
    cached <<- any(grepl("TRUE", out))
    cached
  }
})

test_that("start_log outside a project with no location errors in plain English", {
  withr::local_dir(withr::local_tempdir())
  expect_error(start_log(verbose = 0L), "location")
})

test_that("start_log writes the header record before any work, and stop_log closes it", {
  loc <- withr::local_tempdir()
  id <- start_log(location = loc, name = "demo", verbose = 0L)

  dir <- file.path(loc, id)
  rec <- yaml::read_yaml(file.path(dir, "run.yml"))
  expect_equal(rec$schema, "bctu-runlog/1")
  expect_equal(rec$status, "started")
  expect_match(id, "^demo-")
  # the run id's timestamp equals the header checkpoint's stamp
  expect_equal(sub("^demo-", "", id),
               snapshot_id(as.POSIXct(rec$checkpoint$created_utc,
                                      format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  expect_true("bctu" %in% names(rec$packages))

  print("captured-output-marker")
  dir_out <- stop_log(verbose = 0L)

  expect_equal(dir_out, dir)
  transcript <- readLines(file.path(dir, "run.log"))
  expect_true(any(grepl("captured-output-marker", transcript, fixed = TRUE)))
  rec <- yaml::read_yaml(file.path(dir, "run.yml"))
  expect_equal(rec$status, "clean")
  expect_false(is.null(rec$ended_utc))
  expect_equal(rec$transcript_sha256, sha256_file(file.path(dir, "run.log")))
  expect_false(file.exists(file.path(dir, "run.msg")))

  idx <- list_logs(loc)
  expect_equal(nrow(idx), 1L)
  expect_equal(idx$id, id)
  expect_equal(idx$status, "clean")
})

test_that("console sink depth is restored after stop_log", {
  loc <- withr::local_tempdir()
  before <- sink.number()
  start_log(location = loc, name = "sinks", verbose = 0L)
  stop_log(verbose = 0L)
  expect_equal(sink.number(), before)
})

test_that("a second run in the same second gets a suffixed directory and both index lines", {
  loc <- withr::local_tempdir()
  id1 <- start_log(location = loc, name = "twin", verbose = 0L)
  stop_log(verbose = 0L)
  # force the collision regardless of clock: pre-create every un-suffixed
  # candidate directory for the next few seconds
  for (off in 0:3) dir.create(file.path(loc, paste0("twin-", snapshot_id(utc_now() + off))),
                              showWarnings = FALSE)
  id2 <- start_log(location = loc, name = "twin", verbose = 0L)
  stop_log(verbose = 0L)
  expect_match(id2, "-[0-9]+$")
  expect_equal(nrow(list_logs(loc)), 2L)
  expect_true(all(c(id1, id2) %in% list_logs(loc)$id))
})

test_that("checkpoint() writes into an open log and stays pure with none open", {
  loc <- withr::local_tempdir()
  id <- start_log(location = loc, name = "cp", verbose = 0L)
  st <- checkpoint()
  stop_log(verbose = 0L)

  rec <- yaml::read_yaml(file.path(loc, id, "run.yml"))
  expect_length(rec$checkpoints, 1L)
  expect_equal(rec$checkpoints[[1]]$created_utc, st$created_utc)
  transcript <- readLines(file.path(loc, id, "run.log"))
  expect_true(any(grepl("=== checkpoint ", transcript, fixed = TRUE)))

  # non-interactive, no open log: pure stamp, nothing created
  empty <- withr::local_tempdir()
  withr::local_dir(empty)
  st2 <- checkpoint()
  expect_true(is.list(st2) && !is.null(st2$created_utc))
  expect_length(list.files(empty, recursive = TRUE), 0L)
})

test_that("stop_log with no open log, and a second start_log, both error clearly", {
  expect_error(stop_log(verbose = 0L), "No run log is open")
  loc <- withr::local_tempdir()
  start_log(location = loc, name = "solo", verbose = 0L)
  expect_error(start_log(location = loc, name = "second", verbose = 0L),
               "already open")
  stop_log(verbose = 0L)
})

test_that("read_log returns the record and names unknown ids", {
  loc <- withr::local_tempdir()
  id <- start_log(location = loc, name = "reader", verbose = 0L)
  stop_log(verbose = 0L)
  rec <- read_log(id, location = loc)
  expect_equal(rec$id, id)
  expect_error(read_log("no-such-run", location = loc), "No run record")
})

test_that("outputs are inventoried with hashes", {
  loc  <- withr::local_tempdir()
  outd <- withr::local_tempdir()
  writeLines("result", file.path(outd, "result.txt"))
  id <- start_log(location = loc, name = "outs", verbose = 0L)
  stop_log(outputs = outd, verbose = 0L)
  rec <- read_log(id, location = loc)
  expect_length(rec$outputs, 1L)
  expect_equal(rec$outputs[[1]]$sha256, sha256_file(file.path(outd, "result.txt")))
})

test_that("a top-level session records conditions, command echo and the msg sidecar", {
  skip_on_cran()
  skip_if_not(child_bctu_has_run_logs(), "installed bctu lacks run logs")
  loc <- withr::local_tempdir()
  script <- file.path(withr::local_tempdir(), "child.R")
  writeLines(c(
    sprintf("bctu::start_log(location = '%s', name = 'child', verbose = 0L)", loc),
    "print('child-output')",
    "warning('child-warning')",
    "bctu::stop_log(verbose = 0L)"), script)
  res <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                 stdout = TRUE, stderr = TRUE)

  idx <- list_logs(loc)
  expect_equal(nrow(idx), 1L)
  expect_equal(idx$status, "warnings")
  expect_equal(idx$warnings, 1L)
  dir <- file.path(loc, idx$id)
  transcript <- readLines(file.path(dir, "run.log"))
  expect_true(any(grepl("child-output", transcript, fixed = TRUE)))
  expect_true(any(grepl("## [warning", transcript, fixed = TRUE)))
  expect_true(any(grepl("> print", transcript, fixed = TRUE)))
  expect_true(file.exists(file.path(dir, "run.msg")))
})

test_that("quit() without stop_log leaves an interrupted record; a hard kill leaves the started header", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(child_bctu_has_run_logs(), "installed bctu lacks run logs")
  loc <- withr::local_tempdir()

  quit_script <- file.path(withr::local_tempdir(), "quitter.R")
  writeLines(c(
    sprintf("bctu::start_log(location = '%s', name = 'quitter', verbose = 0L)", loc),
    "quit(save = 'no')"), quit_script)
  system2(file.path(R.home("bin"), "Rscript"), shQuote(quit_script),
          stdout = TRUE, stderr = TRUE)
  idx <- list_logs(loc)
  qrow <- idx[grepl("^quitter-", idx$id), , drop = FALSE]
  expect_equal(nrow(qrow), 1L)
  expect_equal(qrow$status, "interrupted")

  kill_script <- file.path(withr::local_tempdir(), "sleeper.R")
  pid_file <- file.path(dirname(kill_script), "pid")
  writeLines(c(
    sprintf("bctu::start_log(location = '%s', name = 'sleeper', verbose = 0L)", loc),
    sprintf("writeLines(as.character(Sys.getpid()), '%s')", pid_file),
    "Sys.sleep(60)"), kill_script)
  system(sprintf("'%s' %s > /dev/null 2>&1 &",
                 file.path(R.home("bin"), "Rscript"), shQuote(kill_script)))
  for (i in 1:100) { if (file.exists(pid_file)) break; Sys.sleep(0.1) }
  expect_true(file.exists(pid_file))
  pid <- as.integer(readLines(pid_file))
  tools::pskill(pid, tools::SIGKILL)
  Sys.sleep(0.5)

  sleeper_dirs <- list.files(loc, pattern = "^sleeper-")
  expect_length(sleeper_dirs, 1L)
  rec <- yaml::read_yaml(file.path(loc, sleeper_dirs[[1]], "run.yml"))
  expect_equal(rec$status, "started")
  expect_false(sleeper_dirs[[1]] %in% list_logs(loc)$id)
})

test_that("inside a knitr render, conditions are captured via knitr's calling.handlers", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not(child_bctu_has_run_logs(), "installed bctu lacks run logs")
  loc <- withr::local_tempdir()
  rmd <- file.path(withr::local_tempdir(), "doc.Rmd")
  md  <- sub("Rmd$", "md", rmd)
  writeLines(c(
    "```{r}",
    sprintf("bctu::start_log(location = '%s', name = 'knit', verbose = 0L)", loc),
    "```", "",
    "```{r}",
    "print('knit-output')",
    "warning('knit-warning')",
    "```", "",
    "```{r}",
    "bctu::stop_log(verbose = 0L)",
    "```"), rmd)
  # knit in a child process so the installed package and a clean session are used
  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("-e", shQuote(sprintf("knitr::knit('%s', output = '%s', quiet = TRUE)", rmd, md))),
                 stdout = TRUE, stderr = TRUE)

  idx <- list_logs(loc)
  expect_equal(nrow(idx), 1L)
  expect_equal(idx$status, "warnings")
  expect_equal(idx$warnings, 1L)
  rec <- read_log(idx$id, location = loc)
  expect_equal(rec$condition_capture, "knitr")
  transcript <- readLines(file.path(loc, idx$id, "run.log"))
  expect_true(any(grepl("## [warning", transcript, fixed = TRUE)))
  # the knitted document still shows the chunk output and the warning
  knitted <- readLines(md)
  expect_true(any(grepl("knit-output", knitted, fixed = TRUE)))
  expect_true(any(grepl("Warning", knitted, fixed = TRUE)))
})
