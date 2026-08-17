make_example_table <- function() {
  tab_data <- data.frame(
    characteristic = c("Age (years)", "Weight (kg)", "Female"),
    arm_a_n        = c(30, 30, 30),
    arm_a_stat     = c(54.2, 78.510, 16),
    arm_b_n        = c(31, 31, 31),
    arm_b_stat     = c(55.1, 80.3, 18),
    stringsAsFactors = FALSE)
  rt <- report_table(
    tab_data,
    columns = c(characteristic = "Characteristic",
                arm_a_n = "n", arm_a_stat = "Statistic",
                arm_b_n = "n", arm_b_stat = "Statistic"),
    caption = "Baseline characteristics by arm",
    group_headers = c(" " = 1, "Arm A" = 2, "Arm B" = 2),
    banner_rows = list(list(label = "Continuous", after = 0),
                       list(label = "Binary",     after = 2)))
  list(rt = rt, data = tab_data)
}

test_that("report_table_data returns the underlying numbers unchanged", {
  ex <- make_example_table()
  qc <- report_table_data(ex$rt)
  expect_equal(qc$arm_a_stat, ex$data$arm_a_stat)
  expect_equal(qc$arm_b_stat, ex$data$arm_b_stat)
})

test_that("render_table_markdown produces a non-empty grid table with the data", {
  ex <- make_example_table()
  md <- render_table_markdown(ex$rt)
  expect_true(nzchar(md))
  expect_match(md, "78.51", fixed = TRUE)
  expect_match(md, "Arm A", fixed = TRUE)
  expect_match(md, "Continuous", fixed = TRUE)
})

test_that("render_report writes a docx bundle with a provenance manifest", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")

  store <- withr::local_tempdir()
  snap  <- take_snapshot(datasource_example("redcap", n = 12L, seed = 1L),
                         store = store, verbose = 0L)
  ex <- make_example_table()
  report <- bctu_report(
    title = "EXAMPLE Monitoring Report",
    sections = list(
      intro  = report_heading("Introduction", level = 1),
      para   = report_paragraph("Assembled from explicit section objects."),
      table1 = ex$rt),
    meta = list(author = "Test Harness", trial = "EXAMPLE"))

  out_dir <- withr::local_tempdir()
  res <- render_report(report, output_dir = out_dir, formats = "docx",
                       snapshot = snap, verbose = 0L)

  expect_true(file.exists(res$outputs$docx))
  expect_gt(file.info(res$outputs$docx)$size, 0)
  expect_true(file.exists(res$manifest))

  man <- yaml::read_yaml(res$manifest)
  expect_false(is.null(man$outputs$docx$sha256))
  expect_equal(man$snapshot$id, attr(snap, "id"))
})

test_that("render_report embeds figure assets in the docx (resource path resolves)", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")

  fig <- file.path(withr::local_tempdir(), "figure.png")
  grDevices::png(fig, width = 480, height = 320)
  plot(1:3, 1:3)
  grDevices::dev.off()

  report <- bctu_report(
    title = "EXAMPLE Figure Report",
    sections = list(
      intro = report_paragraph("One figure follows."),
      fig   = report_figure(path = fig, caption = "Example figure")))

  out_dir <- withr::local_tempdir()
  res <- render_report(report, output_dir = out_dir, formats = "docx",
                       verbose = 0L)

  media <- utils::unzip(res$outputs$docx, list = TRUE)$Name
  expect_true(any(grepl("^word/media/.*\\.png$", media)))
})

test_that("render_table_markdown escapes pipe and newline so the grid table stays structurally valid", {
  df <- data.frame(id = 1:2,
                   note = c("has|pipe", "line1\nline2"),
                   stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)

  # the literal pipe is escaped, not left to look like a column boundary
  expect_match(md, "has\\|pipe", fixed = TRUE)
  # the embedded newline is collapsed so the row stays on one grid-table line
  expect_false(grepl("line1\nline2", md, fixed = TRUE))
  expect_match(md, "line1 line2", fixed = TRUE)

  # every content line has the same number of (unescaped) "|" as a border
  # line has "+", i.e. no cell content introduced a spurious column
  lines <- strsplit(md, "\n")[[1]]
  content_lines <- lines[startsWith(lines, "|")]
  border_lines  <- lines[startsWith(lines, "+")]
  n_pipes   <- lengths(regmatches(content_lines, gregexpr("(?<!\\\\)\\|", content_lines, perl = TRUE)))
  n_borders <- lengths(regmatches(border_lines, gregexpr("\\+", border_lines)))
  expect_true(all(n_pipes == n_borders[1]))
})

test_that("escape_grid_text preserves leading-space indentation as no-break spaces", {
  out <- bctu:::escape_grid_text(c("  3.1(b) Indented", "Flush", "   deeper"))
  expect_equal(substr(out[1], 1, 2), strrep("\u00a0", 2))
  expect_match(out[1], "3.1\\(b\\) Indented")
  expect_equal(out[2], "Flush")
  expect_equal(substr(out[3], 1, 3), strrep("\u00a0", 3))
  # interior spaces untouched
  expect_false(grepl(" \u00a0| \u00a0", out[1]))
})

test_that("escape_grid_text keeps list-like and heading-like cell text literal", {
  esc <- bctu:::escape_grid_text
  expect_equal(esc("II. Baby"), "II\\. Baby")
  expect_equal(esc("1. Pre-existing condition"), "1\\. Pre-existing condition")
  expect_equal(esc("a) option"), "a\\) option")
  expect_equal(esc("(a) option"), "(a\\) option")
  expect_equal(esc("- item"), "\\- item")
  expect_equal(esc("# note"), "\\# note")
  expect_equal(esc("> quote"), "\\> quote")
  # not list markers: no space after the dot, or version-like text
  expect_equal(esc("4.1(a) Abnormal"), "4.1(a) Abnormal")
  expect_equal(esc("v1.2 release"), "v1.2 release")
})

test_that("render_table_markdown renders a structurally valid table for zero rows", {
  df <- data.frame(id = integer(0), note = character(0), stringsAsFactors = FALSE)
  rt <- report_table(df)
  md <- render_table_markdown(rt)

  lines <- strsplit(md, "\n")[[1]]
  expect_true(length(lines) >= 4)
  expect_true(startsWith(lines[1], "+"))
  expect_true(startsWith(lines[length(lines)], "+"))
})

test_that("build_report_markdown carries geometry for PDF only, per orientation and margin", {
  report <- bctu_report(
    title = "Layout Demo",
    sections = list(h = report_heading("Section one")))
  pdf_md <- build_report_markdown(report, "pdf", tempdir(),
                                  orientation = "landscape", margin = "1in")
  # The metadata block must be contiguous lines or pandoc drops it entirely.
  expect_true(startsWith(pdf_md,
    '---\ntitle: "Layout Demo"\ngeometry: "landscape,margin=1in"\n---\n'))
  docx_md <- build_report_markdown(report, "docx", tempdir(),
                                   orientation = "landscape", margin = "1in")
  expect_false(grepl("geometry", docx_md, fixed = TRUE))
  default_md <- build_report_markdown(report, "pdf", tempdir())
  expect_true(grepl('geometry: "portrait,margin=1in"', default_md, fixed = TRUE))
})

test_that("render_report records the layout in the manifest and passes toc flags to pandoc", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")
  out <- withr::local_tempdir()
  report <- bctu_report(
    title = "Layout Demo",
    sections = list(h = report_heading("Section one"),
                    p = report_paragraph("Some text.")))
  res <- render_report(report, output_dir = out, formats = "docx",
                       orientation = "landscape", toc = TRUE,
                       number_sections = TRUE, verbose = 0L)
  man <- yaml::read_yaml(res$manifest)
  expect_equal(man$layout$orientation, "landscape")
  expect_equal(man$layout$margin, "1in")
  expect_true(man$layout$toc)
  expect_true(man$layout$number_sections)
})
