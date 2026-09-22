test_that("run_dvp rejects anything that is not a named list", {
  expect_error(run_dvp("not a function", list()), "must be a function")
  expect_error(run_dvp(function(data) data.frame(x = 1), NULL), "named list")
  expect_error(run_dvp(function(data) list(1, 2), NULL), "named list")
  expect_error(run_dvp(function(data) list(a = 1, a = 2), NULL), "named list")
})

test_that("run_dvp keeps a named list and normalises NULL elements to empty frames", {
  dvp <- function(data) list(
    always_empty = NULL,
    one_row      = data.frame(record_id = "E001", note = "x"))
  out <- run_dvp(dvp, NULL)
  expect_named(out, c("always_empty", "one_row"))
  expect_s3_class(out$always_empty, "data.frame")
  expect_equal(nrow(out$always_empty), 0L)
  expect_equal(nrow(out$one_row), 1L)
})

test_that("run_dvp rejects a check element that is not a data frame", {
  expect_error(run_dvp(function(data) list(bad = 1:3), NULL), "data frame")
})

test_that("compare_dvp labels findings new / unchanged / resolved whole-row", {
  before <- data.frame(record_id = c("E001", "E002", "E004"),
                       reason = c("r1", "r2", "r4"))
  after  <- data.frame(record_id = c("E001", "E002", "E003"),
                       reason = c("r1", "rX", "r3"))
  dvp <- function(data) list(demo = data)

  cmp <- compare_dvp(dvp, before, after)$demo
  status_of <- function(rid) cmp$status[cmp$record_id == rid]
  expect_equal(status_of("E001"), "unchanged")           # identical row
  expect_equal(sort(status_of("E002")), c("new", "resolved"))  # reason changed -> row differs
  expect_equal(status_of("E003"), "new")
  expect_equal(status_of("E004"), "resolved")
})

test_that("compare_dvp handles a check present in only one run", {
  dvp <- function(data) {
    if (identical(data, "after")) list(only_after = data.frame(id = 1))
    else list(only_before = data.frame(id = 9))
  }
  cmp <- compare_dvp(dvp, before = "before", after = "after")
  expect_setequal(names(cmp), c("only_after", "only_before"))
  expect_equal(cmp$only_after$status, "new")
  expect_equal(cmp$only_before$status, "resolved")
})

test_that("save_dvr writes a full set and an auditable manifest, trial name from the snapshot", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  lo <- 65; hi <- 90
  dvp <- function(data) {
    r <- data$records
    bad <- is.na(r$weight_kg) | r$weight_kg < lo | r$weight_kg > hi
    list(weight_range = data.frame(record_id = r$record_id[bad],
                                   value = r$weight_kg[bad],
                                   reason = "weight out of range"))
  }

  snap <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L, name = "DEMO"),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = out_dir, operator = "tester",
                  write_readable = TRUE, verbose = 0L)

  expect_length(res$dirs, 1L)
  expect_true(dir.exists(res$dirs[[1]]))
  expect_true(file.exists(file.path(res$dirs[[1]], "full", "weight_range.csv")))
  expect_false(res$compared)
  expect_equal(res$trial, "DEMO")

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$schema, "bctu-dvr/1")
  expect_equal(man$trial, "DEMO")
  expect_equal(man$after_snapshot$id, attr(snap, "id"))
  expect_null(man$before_snapshot)
})

test_that("save_dvr with a before snapshot writes full/new/resolved folders by default", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) {
    hit <- data$records$record_id[data$records$outcome == 1]
    list(outcome_positive = data.frame(record_id = hit,
                                       reason = rep("outcome positive", length(hit))))
  }

  snapA <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 40L, seed = 3L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir,
                  operator = "tester", write_readable = TRUE, verbose = 0L)
  expect_true(res$compared)
  expect_equal(res$status_output, "folders")
  expect_true(res$include_resolved)
  expect_true("status" %in% names(res$sheets$outcome_positive))
  expect_true(dir.exists(file.path(res$dirs[[1]], "full")))
  expect_true(dir.exists(file.path(res$dirs[[1]], "new")))
  expect_true(dir.exists(file.path(res$dirs[[1]], "resolved")))
  expect_false(dir.exists(file.path(res$dirs[[1]], "update")))

  # The folders carry the status, so the written files have no status column;
  # full = unchanged + new, resolved rows sit in resolved/ only.
  full_csv <- utils::read.csv(file.path(res$dirs[[1]], "full", "outcome_positive.csv"))
  expect_false("status" %in% names(full_csv))
  st <- res$sheets$outcome_positive$status
  expect_equal(nrow(full_csv), sum(st %in% c("new", "unchanged")))
  new_csv <- utils::read.csv(file.path(res$dirs[[1]], "new", "outcome_positive.csv"))
  expect_false("status" %in% names(new_csv))
  expect_equal(nrow(new_csv), sum(st == "new"))
  resolved_csv <- utils::read.csv(file.path(res$dirs[[1]], "resolved", "outcome_positive.csv"))
  expect_equal(nrow(resolved_csv), sum(st == "resolved"))

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_true(man$compared)
  expect_equal(man$status_output, "folders")
  expect_true(man$include_resolved)
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))
  expect_equal(man$after_snapshot$id, attr(snapB, "id"))

  # include_resolved = TRUE adds the resolved/ set (status still folder-borne).
  out_dir2 <- withr::local_tempdir()
  res2 <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir2,
                   operator = "tester", include_resolved = TRUE,
                   write_readable = TRUE, verbose = 0L)
  expect_true(dir.exists(file.path(res2$dirs[[1]], "resolved")))
  res_csv <- utils::read.csv(file.path(res2$dirs[[1]], "resolved", "outcome_positive.csv"))
  expect_false("status" %in% names(res_csv))
  expect_equal(nrow(res_csv), sum(st == "resolved"))
  expect_true(yaml::read_yaml(file.path(res2$dirs[[1]], "manifest.yml"))$include_resolved)
})

test_that("save_dvr status_output = 'column' keeps the status column and update set", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) {
    hit <- data$records$record_id[data$records$outcome == 1]
    list(outcome_positive = data.frame(record_id = hit,
                                       reason = rep("outcome positive", length(hit))))
  }

  snapA <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 40L, seed = 3L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir,
                  operator = "tester", status_output = "column",
                  write_readable = TRUE, verbose = 0L)
  expect_true(res$compared)
  expect_equal(res$status_output, "column")
  expect_true("status" %in% names(res$sheets$outcome_positive))
  expect_true(dir.exists(file.path(res$dirs[[1]], "update")))
  expect_false(dir.exists(file.path(res$dirs[[1]], "new")))
  expect_false(dir.exists(file.path(res$dirs[[1]], "resolved")))

  # Default keeps resolved rows, labelled, in full/ and update/.
  st <- res$sheets$outcome_positive$status
  full_csv <- utils::read.csv(file.path(res$dirs[[1]], "full", "outcome_positive.csv"))
  expect_true("status" %in% names(full_csv))
  expect_equal(sum(full_csv$status == "resolved"), sum(st == "resolved"))
  upd_csv <- utils::read.csv(file.path(res$dirs[[1]], "update", "outcome_positive.csv"))
  expect_true(all(upd_csv$status %in% c("new", "resolved")))

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_true(man$compared)
  expect_equal(man$status_output, "column")
  expect_true(man$include_resolved)

  # include_resolved = TRUE restores the resolved-labelled rows.
  out_dir2 <- withr::local_tempdir()
  res2 <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir2,
                   operator = "tester", status_output = "column",
                   include_resolved = TRUE, write_readable = TRUE, verbose = 0L)
  full2 <- utils::read.csv(file.path(res2$dirs[[1]], "full", "outcome_positive.csv"))
  expect_true("resolved" %in% full2$status)
  upd2 <- utils::read.csv(file.path(res2$dirs[[1]], "update", "outcome_positive.csv"))
  expect_true(all(upd2$status %in% c("new", "resolved")))
})

test_that("save_dvr writes to every path in paths", {
  skip_if_not_installed("openxlsx")
  store <- withr::local_tempdir()
  out1  <- withr::local_tempdir()
  out2  <- withr::local_tempdir()

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = c(out1, out2), write_readable = TRUE, verbose = 0L)

  expect_length(res$dirs, 2L)
  expect_true(all(vapply(res$dirs, dir.exists, logical(1))))
  expect_true(file.exists(file.path(res$dirs[[1]], "full", "all_records.csv")))
  expect_true(file.exists(file.path(res$dirs[[2]], "full", "all_records.csv")))
})

test_that("save_dvr splits per site plus overall when split_by is given", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  snap <- take_snapshot(datasource_example("redcap", n = 20L, seed = 1L),
                        store = store, verbose = 0L)
  snap$records$site <- rep(c("Site_A", "Site_B"), length.out = nrow(snap$records))

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  res <- save_dvr(dvp, snap, paths = out_dir, id_col = "record_id",
                  split_by = "site", verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  expect_true(length(list.files(full, pattern = "\\.xlsx$")) >= 1L)   # overall
  expect_true(dir.exists(file.path(full, "sites", "Site_A")))
  expect_true(dir.exists(file.path(full, "sites", "Site_B")))
})

test_that("a resolved finding whose record was removed from after is still sited from before", {
  skip_if_not_installed("openxlsx")
  out_dir <- withr::local_tempdir()

  before <- list(records = data.frame(
    record_id = c("E001", "E002"), site = c("Site_A", "Site_B"),
    stringsAsFactors = FALSE))
  after <- list(records = data.frame(
    record_id = "E002", site = "Site_B", stringsAsFactors = FALSE))  # E001 withdrawn

  dvp <- function(data) list(demo = data.frame(
    record_id = data$records$record_id, note = "issue", stringsAsFactors = FALSE))

  res <- save_dvr(dvp, after = after, before = before, paths = out_dir,
                  id_col = "record_id", split_by = "site",
                  include_resolved = TRUE, write_readable = TRUE, verbose = 0L)

  resolved <- file.path(res$dirs[[1]], "resolved")
  site_a_csv <- file.path(resolved, "sites", "Site_A", "demo.csv")
  expect_true(file.exists(site_a_csv))
  rows <- utils::read.csv(site_a_csv, stringsAsFactors = FALSE)
  expect_true("E001" %in% rows$record_id)
  expect_false(dir.exists(file.path(resolved, "sites", "NO_SITE")))
  expect_false(dir.exists(file.path(res$dirs[[1]], "full", "sites", "NO_SITE")))
})

test_that("write_findings_readable de-duplicates filenames that sanitise to the same stem", {
  dir <- withr::local_tempdir()
  sheets <- list(
    `a/b` = data.frame(x = 1L),
    `a b` = data.frame(x = 2L))

  write_findings_readable(sheets, dir)

  csvs <- sort(list.files(dir, pattern = "\\.csv$"))
  expect_length(csvs, 2L)
  first  <- utils::read.csv(file.path(dir, csvs[1]))
  second <- utils::read.csv(file.path(dir, csvs[2]))
  expect_false(identical(first$x, second$x))
})

test_that("finding_row_keys distinguishes a genuine NA from the literal string \"NA\"", {
  df <- data.frame(value = c(NA_character_, "NA"), stringsAsFactors = FALSE)
  keys <- finding_row_keys(df)
  expect_false(keys[1] == keys[2])
})

test_that("write_report_set warns per check with findings that cannot be mapped to a site", {
  dir <- withr::local_tempdir()
  snapshot <- list(records = data.frame(
    record_id = "E001", site = "Site_A", stringsAsFactors = FALSE))
  sheets <- list(demo = data.frame(
    record_id = "E999", note = "orphan", stringsAsFactors = FALSE))  # not in snapshot

  expect_warning(
    write_report_set(sheets, snapshot, dir, "base", id_col = "record_id",
                     split_by = "site", write_readable = TRUE),
    "could not be mapped to a site")
})

test_that("check_info writes the index, prepends the query column, and reaches the manifest", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) {
    r <- data$records
    bad <- is.na(r$weight_kg) | r$weight_kg < 65 | r$weight_kg > 90
    list(weight_range = data.frame(record_id = r$record_id[bad]),
         never_fires  = data.frame())
  }
  info <- data.frame(
    check    = c("weight_range", "never_fires"),
    query    = c("Please confirm the recorded weight.",
                 "Please confirm the visit date."),
    section  = c("Baseline", "Baseline"),
    critical = c(TRUE, FALSE))

  snap <- take_snapshot(datasource_example("redcap", n = 40L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = out_dir, check_info = info,
                  write_readable = TRUE, verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  idx <- utils::read.csv(file.path(full, "checks_index.csv"))
  expect_equal(idx$check, info$check)   # all checks listed, all-clear included
  expect_true(file.exists(file.path(full, "checks_index.txt")))

  found <- utils::read.csv(file.path(full, "weight_range.csv"))
  expect_gt(nrow(found), 0L)
  expect_equal(names(found)[1], "query")
  expect_equal(unique(found$query), "Please confirm the recorded weight.")

  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  wr <- Filter(function(chk) chk$name == "weight_range", man$checks)[[1]]
  expect_equal(wr$query, "Please confirm the recorded weight.")
  expect_equal(wr$section, "Baseline")
  expect_true(wr$critical)
})

test_that("check_info attr fallback works and the explicit argument wins", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  info_attr <- data.frame(check = "all_records", query = "Attribute text.")
  dvp <- function(data) {
    findings <- list(all_records = data.frame(record_id = data$records$record_id))
    attr(findings, "check_info") <- info_attr
    findings
  }
  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)

  res <- save_dvr(dvp, snap, paths = out_dir, write_readable = TRUE, verbose = 0L)
  found <- utils::read.csv(file.path(res$dirs[[1]], "full", "all_records.csv"))
  expect_equal(unique(found$query), "Attribute text.")

  out2 <- withr::local_tempdir()
  info_arg <- data.frame(check = "all_records", query = "Argument text.")
  res2 <- save_dvr(dvp, snap, paths = out2, check_info = info_arg,
                   write_readable = TRUE, verbose = 0L)
  found2 <- utils::read.csv(file.path(res2$dirs[[1]], "full", "all_records.csv"))
  expect_equal(unique(found2$query), "Argument text.")
})

test_that("the query column is added after comparison, so it never disturbs status", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  info <- data.frame(check = "all_records", query = "Confirm the record.")

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  snapB <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, before = snapA, paths = out_dir,
                  check_info = info, write_readable = TRUE, verbose = 0L)
  found <- utils::read.csv(file.path(res$dirs[[1]], "full", "all_records.csv"))
  expect_equal(names(found)[1], "query")
  expect_true(all(found$status == "unchanged"))
})

test_that("a check returning its own query column errors while check_info is in use", {
  skip_if_not_installed("openxlsx")
  store <- withr::local_tempdir()
  dvp <- function(data) list(clash =
    data.frame(record_id = data$records$record_id, query = "mine"))
  info <- data.frame(check = "clash", query = "Engine text.")
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)
  expect_error(
    save_dvr(dvp, snap, paths = withr::local_tempdir(), check_info = info,
             write_readable = TRUE, verbose = 0L),
    "reserved column")
})

test_that("check_info is validated and partial annotation warns", {
  skip_if_not_installed("openxlsx")
  store <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)
  run <- function(info) save_dvr(dvp, snap, paths = withr::local_tempdir(),
                                 check_info = info, write_readable = TRUE, verbose = 0L)

  expect_error(run(list(check = "a")), "must be a data frame")
  expect_error(run(data.frame(check = c("a", "a"), query = c("x", "y"))), "Duplicate")
  expect_error(run(data.frame(check = "all_records", query = " ")), "non-empty text")
  expect_error(run(data.frame(check = "all_records", query = "x", critical = "yes")),
               "logical")
  expect_warning(run(data.frame(check = c("all_records", "ghost"),
                                query = c("x", "y"))), "matching no check")
})

test_that("checks_index leads every workbook, per-site included", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  snap <- take_snapshot(datasource_example("redcap", n = 20L, seed = 1L),
                        store = store, verbose = 0L)
  snap$records$site <- rep(c("Site_A", "Site_B"), length.out = nrow(snap$records))

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  info <- data.frame(check = "all_records", query = "Confirm the record.")

  res <- save_dvr(dvp, snap, paths = out_dir, id_col = "record_id",
                  split_by = "site", check_info = info,                   verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  master <- list.files(full, pattern = "\\.xlsx$", full.names = TRUE)[1]
  expect_equal(openxlsx::getSheetNames(master)[1], "checks_index")

  site_wb <- list.files(file.path(full, "sites", "Site_A"),
                        pattern = "\\.xlsx$", full.names = TRUE)[1]
  expect_equal(openxlsx::getSheetNames(site_wb)[1], "checks_index")
  expect_false(file.exists(file.path(full, "sites", "Site_A", "checks_index.csv")))
})

test_that("findings carrying their own site column are sited from it, with snapshot fallback", {
  findings <- data.frame(record_id = c("1", "2", "3"),
                         Site = c("Alpha", NA, "Gamma"))
  snap <- structure(list(records = data.frame(record_id = c("1", "2", "3"),
                                              Site = c("X", "Beta", "Z"))),
                    class = c("bctu_snapshot", "list"))
  sites <- resolve_finding_sites(findings, list(snap), "record_id", "Site")
  expect_equal(sites, c("Alpha", "Beta", "Gamma"))

  no_map <- structure(list(records = data.frame(other = 1)),
                      class = c("bctu_snapshot", "list"))
  sites2 <- resolve_finding_sites(findings, list(no_map), "record_id", "Site")
  expect_equal(sites2, c("Alpha", "NO_SITE", "Gamma"))
})

test_that("a versioned report id carries the document version in id and directory", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)

  res <- save_dvr(dvp, snap, paths = out_dir, version = "0.5",
                  write_readable = TRUE, verbose = 0L)
  expect_equal(res$dirs[[1]],
               file.path(out_dir, attr(snap, "id"), "v0.5"))
  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$dvr_id, paste0("DVR-v0.5-", attr(snap, "id")))
  expect_equal(man$version, "0.5")
  expect_false(is.null(man$created_utc))

  rerun <- save_dvr(dvp, snap, paths = out_dir, version = "0.5",
                    write_readable = TRUE, verbose = 0L)
  expect_equal(rerun$dirs[[1]],
               file.path(out_dir, attr(snap, "id"), "v0.5_1"))

  out2 <- withr::local_tempdir()
  res2 <- save_dvr(dvp, snap, paths = out2, write_readable = TRUE, verbose = 0L)
  expect_equal(res2$dirs[[1]], file.path(out2, attr(snap, "id")))
})

test_that("the workbook is the delivered record: no per-check CSVs by default", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  info <- data.frame(check = "all_records", query = "Confirm the record.")
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)

  res <- save_dvr(dvp, snap, paths = out_dir, check_info = info, verbose = 0L)
  full <- file.path(res$dirs[[1]], "full")
  expect_length(list.files(full, pattern = "\\.xlsx$"), 1L)
  expect_false(file.exists(file.path(full, "all_records.csv")))
  expect_false(file.exists(file.path(full, "all_records.txt")))
  expect_false(file.exists(file.path(full, "checks_index.csv")))
})

test_that("an explicitly named empty DVP is a valid zero-check report; a bare list() is not", {
  skip_if_not_installed("openxlsx")
  out <- run_dvp(function(data) stats::setNames(list(), character(0)), NULL)
  expect_length(out, 0L)
  expect_error(run_dvp(function(data) list(), NULL), "named list")

  store <- withr::local_tempdir()
  snap <- take_snapshot(datasource_example("redcap", n = 5L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_cdi(function(s) stats::setNames(list(), character(0)), snap,
                  paths = withr::local_tempdir(), verbose = 0L)
  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$total_findings, 0L)
  expect_length(man$checks, 0L)
})

test_that("save_dvr defaults to comparing against the previous snapshot in the store", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 12L, seed = 2L),
                         store = store, verbose = 0L)

  res <- save_dvr(dvp, after = snapB, paths = out_dir, verbose = 0L)
  expect_true(res$compared)
  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))
})

test_that("the default comparison picks the snapshot before `after`, not the store's newest", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 10L, seed = 2L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapC <- take_snapshot(datasource_example("redcap", n = 10L, seed = 3L),
                         store = store, verbose = 0L)

  # rerun on the middle snapshot: before must be A, never C
  res <- save_dvr(dvp, after = snapB, paths = out_dir, verbose = 0L)
  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))

  # rerun on the OLDEST snapshot: nothing earlier, so uncompared, no error
  out2 <- withr::local_tempdir()
  res2 <- save_dvr(dvp, after = snapA, paths = out2, verbose = 0L)
  expect_false(res2$compared)
})

test_that("a store with one snapshot falls back to an uncompared report", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)
  res <- save_dvr(dvp, snap, paths = out_dir, verbose = 0L)
  expect_false(res$compared)
  man <- yaml::read_yaml(file.path(res$dirs[[1]], "manifest.yml"))
  expect_false(man$compared)
})

test_that("an unreadable earlier snapshot (pre-rebuild directory) falls back cleanly", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  # an old-format snapshot directory: valid id name, no manifest inside
  dir.create(file.path(store, "2020-01-01T000000Z"))
  snap <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                        store = store, verbose = 0L)

  res <- save_dvr(dvp, snap, paths = out_dir, verbose = 0L)
  expect_false(res$compared)
  expect_length(res$dirs, 1L)
})

test_that("explicit before = NULL and an explicit before snapshot are honoured", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 10L, seed = 2L),
                         store = store, verbose = 0L)

  out1 <- withr::local_tempdir()
  res_null <- save_dvr(dvp, after = snapB, before = NULL, paths = out1, verbose = 0L)
  expect_false(res_null$compared)

  out2 <- withr::local_tempdir()
  res_expl <- save_dvr(dvp, after = snapB, before = snapA, paths = out2, verbose = 0L)
  expect_true(res_expl$compared)
  man <- yaml::read_yaml(file.path(res_expl$dirs[[1]], "manifest.yml"))
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))
})

test_that("save_cdi carries the same default comparison and fallback", {
  skip_if_not_installed("openxlsx")
  store   <- withr::local_tempdir()
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  snapA <- take_snapshot(datasource_example("redcap", n = 10L, seed = 1L),
                         store = store, verbose = 0L)
  out1 <- withr::local_tempdir()
  res1 <- save_cdi(dvp, snapA, paths = out1, verbose = 0L)
  expect_false(res1$compared)

  Sys.sleep(1.1)
  snapB <- take_snapshot(datasource_example("redcap", n = 12L, seed = 2L),
                         store = store, verbose = 0L)
  out2 <- withr::local_tempdir()
  res2 <- save_cdi(dvp, snapB, paths = out2, verbose = 0L)
  expect_true(res2$compared)
  man <- yaml::read_yaml(file.path(res2$dirs[[1]], "manifest.yml"))
  expect_equal(man$before_snapshot$id, attr(snapA, "id"))
})

test_that("split_by nests the folders and writes a workbook at every level", {
  skip_if_not_installed("openxlsx")
  out_dir <- withr::local_tempdir()

  after <- list(records = data.frame(
    record_id = c("E001", "E002", "E003"),
    country   = c("UK", "UK", "Ireland"),
    site      = c("Site_01", "Site_02", "Site_03"),
    stringsAsFactors = FALSE))

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  res <- save_dvr(dvp, after, before = NULL, paths = out_dir,
                  id_col = "record_id", split_by = c("country", "site"),
                  verbose = 0L)

  full <- file.path(res$dirs[[1]], "full")
  uk <- file.path(full, "sites", "UK")
  expect_true(dir.exists(file.path(uk, "Site_01")))
  expect_true(dir.exists(file.path(uk, "Site_02")))
  expect_true(dir.exists(file.path(full, "sites", "Ireland", "Site_03")))
  expect_length(list.files(uk, pattern = "\\.xlsx$"), 1L)
  expect_length(list.files(file.path(uk, "Site_01"), pattern = "\\.xlsx$"), 1L)
  expect_identical(unlist(res$manifest$split_by), c("country", "site"))
})

test_that("split_by must name at least one column", {
  expect_error(resolve_split_by(character(0)), "at least one column")
  expect_error(resolve_split_by(c("country", NA)), "at least one column")
  expect_identical(resolve_split_by("site"), "site")
  expect_null(resolve_split_by(NULL))
})

test_that("bind_findings fills a missing column with NAs of that column's type", {
  x <- data.frame(record_id = "E001", stringsAsFactors = FALSE)
  x$visit_date <- as.Date("2026-01-01")
  y <- data.frame(record_id = "E002", stringsAsFactors = FALSE)

  out <- bind_findings(x, y)
  expect_s3_class(out$visit_date, "Date")
  expect_true(is.na(out$visit_date[2]))
})

test_that("an empty column takes the other frame's type, silently", {
  x <- data.frame(record_id = "E001", stringsAsFactors = FALSE)
  x$value <- NA
  y <- data.frame(record_id = "E002", value = 3, stringsAsFactors = FALSE)

  out <- bind_findings(x, y)
  expect_type(out$value, "double")
  expect_identical(out$value, c(NA_real_, 3))
})

test_that("a column that changed type between snapshots is widened to text", {
  x <- data.frame(record_id = "E001", value = 12, stringsAsFactors = FALSE)
  y <- data.frame(record_id = "E002", value = "12 or 13", stringsAsFactors = FALSE)

  expect_warning(out <- bind_findings(x, y, check = "demo"), "changed type")
  expect_type(out$value, "character")
  expect_identical(out$value, c("12", "12 or 13"))
})

test_that("compare_dvp survives a field that was empty in the before snapshot", {
  skip_if_not_installed("haven")
  before <- list(records = data.frame(record_id = c("E001", "E002"),
                                      stringsAsFactors = FALSE))
  before$records$coded <- NA
  after <- list(records = data.frame(record_id = c("E001", "E002"),
                                     stringsAsFactors = FALSE))
  after$records$coded <- haven::labelled(c(1, NA), labels = c(No = 0, Yes = 1))

  dvp <- function(data) list(demo = data$records[1, , drop = FALSE])

  out <- compare_dvp(dvp, before, after)
  expect_equal(nrow(out$demo), 2L)
  expect_setequal(out$demo$status, c("new", "resolved"))
})

test_that("an inner group appearing under two outer groups is an error", {
  skip_if_not_installed("openxlsx")
  out_dir <- withr::local_tempdir()
  after <- list(records = data.frame(
    record_id = c("E001", "E002"),
    country   = c("UK", "Ireland"),
    site      = c("Site_01", "Site_01"),
    stringsAsFactors = FALSE))
  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))

  expect_error(
    save_dvr(dvp, after, before = NULL, paths = out_dir,
             split_by = c("country", "site"), verbose = 0L),
    "does not nest")
})

test_that("a compared report writes resolved/ by default and a summary per group", {
  skip_if_not_installed("openxlsx")
  out_dir <- withr::local_tempdir()
  before <- list(records = data.frame(
    record_id = c("E001", "E002", "E003"),
    country   = c("UK", "UK", "Ireland"),
    site      = c("Site_01", "Site_02", "Site_03"),
    stringsAsFactors = FALSE))
  after <- before
  after$records <- after$records[-1, ]        # E001 resolved
  after$records <- rbind(after$records, data.frame(
    record_id = "E004", country = "Ireland", site = "Site_03",
    stringsAsFactors = FALSE))                 # E004 new

  dvp <- function(data) list(all_records =
    data.frame(record_id = data$records$record_id))
  res <- save_dvr(dvp, after, before = before, paths = out_dir,
                  split_by = c("country", "site"), verbose = 0L)

  dir <- res$dirs[[1]]
  expect_true(dir.exists(file.path(dir, "resolved", "sites", "UK", "Site_01")))
  expect_true(dir.exists(file.path(dir, "new", "sites", "Ireland", "Site_03")))

  summary <- readLines(file.path(dir, "summary.md"))
  expect_true(any(grepl("^## Findings by country and site$", summary)))
  expect_true(any(grepl("^\\| Overall \\| 3 \\| 1 \\| 2 \\| 1 \\|$", summary)))
  expect_true(any(grepl("^\\| UK \\| 1 \\| 0 \\| 1 \\| 1 \\|$", summary)))
  expect_true(any(grepl("^\\| Ireland / Site_03 \\| 2 \\| 1 \\| 1 \\| 0 \\|$", summary)))
})
