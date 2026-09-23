# End-to-end render of a report using every layout feature, checked in the docx.

test_that("a full trial report renders with every layout feature in place", {
  skip_if_not_installed("rmarkdown")
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not available")
  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "full.Rmd")
  writeLines(c(
    "---", 'trial-short-name: "TEST"', 'trial-long-name: "A test trial"', 'report-type: "Test Report"',
    'report-subtype: "Closed"', 'confidential: "Confidential."',
    "metadata:", '  - Version: "v9"', "theme:", '  colour.accent: "0057BF"', '  font.body: "Georgia"', "  font.size: 12",
    "include-toc: true", "toc-depth: 2", "output:", "  bctu::trial_report: default", "---", "",
    "# One", "", "Body text.", "",
    "```{r, echo=FALSE, results='asis'}",
    "tab <- data.frame(a = c('Cure', bctu::indent('Missing')), b = c('3/4 (75%)', '1'))",
    "cat(bctu::render_table(tab, caption = 'A table.', bold_rows = 1L, bold_headings = FALSE), '\\n')",
    "```", "", "::: footnote", "^a^ A note.", ":::", "",
    "## One A", "", "::: landscape", "", "# Two", "", "Wide.", "", ":::", ""), rmd)
  out <- rmarkdown::render(rmd, output_file = "full.docx", output_dir = dir, quiet = TRUE)
  work <- withr::local_tempdir()
  utils::unzip(out, exdir = work)
  read <- function(part) paste(readLines(file.path(work, part), warn = FALSE), collapse = "")
  document <- read("word/document.xml"); styles <- read("word/styles.xml")
  settings <- read("word/settings.xml"); theme <- read("word/theme/theme1.xml")
  header <- read("word/header1.xml"); footer <- read("word/footer1.xml")

  expect_false(grepl("w:dirty", document, fixed = TRUE))
  expect_false(grepl("updateFields", settings, fixed = TRUE))
  expect_false(grepl("w:dirty", paste(header, footer), fixed = TRUE))
  expect_true(grepl(">TEST<", header, fixed = TRUE))
  expect_true(grepl(">Test Report<", header, fixed = TRUE))
  expect_true(grepl(">Closed<", header, fixed = TRUE))
  expect_equal(lengths(regmatches(footer, gregexpr('fldCharType="separate"', footer)))[[1]], 2L)
  expect_true(grepl('w:val="TOC1"', document, fixed = TRUE))
  expect_true(grepl('w:val="TOC2"', document, fixed = TRUE))
  expect_true(grepl("<w:hyperlink w:anchor=\"one\"", document, fixed = TRUE))
  expect_true(grepl("gridSpan", document, fixed = TRUE))
  expect_true(grepl('<w:pStyle w:val="FootnoteBlockText"', document, fixed = TRUE))
  expect_equal(lengths(regmatches(document, gregexpr('w:orient="landscape"', document)))[[1]], 1L)
  expect_true(grepl("Confidential.", document, fixed = TRUE))
  expect_true(grepl('<a:accent2><a:srgbClr val="0057BF"/></a:accent2>', theme, fixed = TRUE))
  expect_true(grepl('<a:minorFont><a:latin typeface="Georgia"', theme, fixed = TRUE))
  expect_true(grepl('w:ascii="Georgia"', styles, fixed = TRUE))
  expect_false(grepl('w:ascii="Arial"', styles, fixed = TRUE))
  border <- regmatches(styles, regexpr('<w:top [^>]*w:themeColor="accent2"[^>]*/>', styles))
  expect_true(grepl('w:color="0057BF"', border, fixed = TRUE))

  stamp <- report_provenance(out)
  expect_equal(stamp$version, as.character(packageVersion("bctu")))
  expect_equal(stamp$template, "bctu")
  expect_match(stamp$rendered, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_null(report_provenance(system.file("rmarkdown", "templates", "report", "resources", "reference.docx", package = "bctu")))
})

test_that("theme values are validated", {
  expect_error(resolve_theme(report_theme(font.body = 3)), "typeface name")
  expect_error(resolve_theme(report_theme(font.size = NA_real_)), "positive number")
  expect_error(n_pct("a", 3), "numeric")
})
