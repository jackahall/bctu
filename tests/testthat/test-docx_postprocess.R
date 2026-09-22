make_fixture_docx <- function(path, document, rels) {
  work <- withr::local_tempdir()
  dir.create(file.path(work, "word", "_rels"), recursive = TRUE)
  writeLines(document, file.path(work, "word", "document.xml"))
  writeLines(rels, file.path(work, "word", "_rels", "document.xml.rels"))
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE)
  utils::zip(path, list.files(".", recursive = TRUE), flags = "-r9Xq")
  path
}

fixture_rels <- paste0(
  '<?xml version="1.0"?><Relationships>',
  '<Relationship Id="rId9" Target="header1.xml"/>',
  '<Relationship Id="rId10" Target="header2.xml"/>',
  '<Relationship Id="rId11" Target="footer1.xml"/>',
  '<Relationship Id="rId12" Target="footer2.xml"/>',
  '</Relationships>')

fixture_document <- paste0(
  '<?xml version="1.0"?><w:document><w:body>',
  '<w:p><w:pPr><w:sectPr>',
  '<w:headerReference w:type="default" r:id="rIdHdr1"/>',
  '<w:headerReference w:type="first" r:id="rIdHdr2"/>',
  '<w:footerReference w:type="default" r:id="rIdFtr1"/>',
  '<w:footerReference w:type="first" r:id="rIdFtr2"/>',
  '<w:pgSz w:w="11906" w:h="16838"/></w:sectPr></w:pPr></w:p>',
  '</w:body></w:document>')

test_that("the filter's placeholder ids are bound to pandoc's relationship ids", {
  out <- bind_header_footer_ids(fixture_document, fixture_rels, "fixture.docx")
  expect_false(grepl("rIdHdr1|rIdHdr2|rIdFtr1|rIdFtr2", out))
  expect_true(grepl('r:id="rId9"', out, fixed = TRUE))
  expect_true(grepl('r:id="rId10"', out, fixed = TRUE))
  expect_true(grepl('r:id="rId12"', out, fixed = TRUE))
})

test_that("a missing header part is an error, not a dangling id", {
  rels <- sub('<Relationship Id="rId10" Target="header2.xml"/>', "", fixture_rels, fixed = TRUE)
  expect_error(bind_header_footer_ids(fixture_document, rels, "fixture.docx"),
               "header2.xml")
})

test_that("repair_report_docx rewrites the docx in place", {
  path <- file.path(withr::local_tempdir(), "fixture.docx")
  make_fixture_docx(path, fixture_document, fixture_rels)
  repair_report_docx(path)
  work <- withr::local_tempdir()
  utils::unzip(path, exdir = work)
  document <- readLines(file.path(work, "word", "document.xml"), warn = FALSE)
  expect_false(any(grepl("rIdHdr1", document)))
  expect_true(any(grepl('r:id="rId9"', document, fixed = TRUE)))
  expect_error(repair_report_docx(file.path(work, "nope.docx")), "No file to repair")
})

test_that("full-width images in landscape sections are widened, others left alone", {
  drawing <- function(cx, cy) paste0(
    '<w:drawing><wp:inline><wp:extent cx="', cx, '" cy="', cy, '"/>',
    '<a:ext cx="', cx, '" cy="', cy, '"/></wp:inline></w:drawing>')
  landscape_break <- '<w:p><w:pPr><w:sectPr><w:pgSz w:w="16838" w:h="11906" w:orient="landscape"/></w:sectPr></w:pPr></w:p>'
  portrait_break <- '<w:p><w:pPr><w:sectPr><w:pgSz w:w="11906" w:h="16838"/></w:sectPr></w:pPr></w:p>'

  document <- paste0("<w:body>", drawing(5727700, 3014578), landscape_break,
                     drawing(5727700, 3014578), portrait_break, "</w:body>")
  out <- widen_landscape_images(document)
  extents <- regmatches(out, gregexpr('<wp:extent cx="[0-9]+" cy="[0-9]+"/>', out))[[1]]
  expect_true(grepl('cx="8863330"', extents[1], fixed = TRUE))
  expect_true(grepl('cx="5727700"', extents[2], fixed = TRUE))
  expect_true(grepl('<a:ext cx="8863330"', out, fixed = TRUE))

  small <- paste0("<w:body>", drawing(1000000, 500000), landscape_break, "</w:body>")
  expect_equal(widen_landscape_images(small), small)
})

test_that("a rendered trial report has no dangling relationship ids", {
  skip_if_not_installed("rmarkdown")
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not available")
  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "mini.Rmd")
  writeLines(c(
    "---", 'trial-short-name: "TEST"', 'report-type: "Test Report"',
    'confidential: "Confidential."', "output:", "  bctu::trial_report: default", "---",
    "", "# Section", "", "Text.", "",
    "| A | B |", "|---|---|", "| 1 | 2 |", ""), rmd)
  out <- rmarkdown::render(rmd, output_file = "mini.docx", output_dir = dir, quiet = TRUE)
  work <- withr::local_tempdir()
  utils::unzip(out, exdir = work)
  document <- paste(readLines(file.path(work, "word", "document.xml"), warn = FALSE), collapse = "")
  rels <- paste(readLines(file.path(work, "word", "_rels", "document.xml.rels"), warn = FALSE), collapse = "")
  ids <- unique(regmatches(document, gregexpr('(?<=r:id=")[^"]+', document, perl = TRUE))[[1]])
  declared <- regmatches(rels, gregexpr('(?<=Id=")[^"]+', rels, perl = TRUE))[[1]]
  expect_setequal(setdiff(ids, declared), character(0))
  expect_true(grepl('w:vAlign w:val="center"', document, fixed = TRUE))
  expect_true(grepl('<w:rPr><w:b/></w:rPr><w:t xml:space="preserve">Confidential.', document, fixed = TRUE))
  expect_true(grepl("</w:tbl>", document, fixed = TRUE))
})

test_that("adjacent landscape sections merge and stray page breaks are dropped", {
  skip_if_not_installed("rmarkdown")
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not available")
  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "land.Rmd")
  table_lines <- c("", "| A | B |", "|---|---|", "| 1 | 2 |", "")
  writeLines(c(
    "---", 'trial-short-name: "TEST"', 'report-type: "Test"',
    "output:", "  bctu::trial_report: default", "---", "", "# One",
    "", "::: landscape", table_lines, ":::", "",
    "::: landscape", table_lines, ":::", "",
    "\\newpage", "",
    "::: landscape", table_lines, ":::", ""), rmd)
  out <- rmarkdown::render(rmd, output_file = "land.docx", output_dir = dir, quiet = TRUE)
  work <- withr::local_tempdir()
  utils::unzip(out, exdir = work)
  document <- paste(readLines(file.path(work, "word", "document.xml"), warn = FALSE), collapse = "")

  sections <- regmatches(document, gregexpr("<w:sectPr.*?</w:sectPr>", document))[[1]]
  landscape <- grepl('w:orient="landscape"', sections, fixed = TRUE)
  expect_equal(sum(landscape), 1L)

  collapsed <- gsub("<w:sectPr.*?</w:sectPr>", "SECT", document)
  expect_false(grepl("SECT</w:pPr></w:p>\\s*<w:p><w:pPr>SECT", collapsed))
  expect_equal(lengths(regmatches(document, gregexpr('w:br w:type="page"', document)))[[1]], 2L)
})

test_that("the first table row gets keep-with-next", {
  row <- function(text, ppr = "") paste0("<w:tr><w:tc><w:p>", ppr, "<w:r><w:t>", text, "</w:t></w:r></w:p></w:tc></w:tr>")
  document <- paste0("<w:body><w:tbl>", row("a", "<w:pPr><w:jc w:val=\"left\"/></w:pPr>"), row("b"), row("c"), row("d"), "</w:tbl></w:body>")
  out <- keep_table_rows_together(document)
  rows <- regmatches(out, gregexpr("<w:tr>.*?</w:tr>", out))[[1]]
  expect_true(grepl("<w:pPr><w:keepNext/><w:jc", rows[1], fixed = TRUE))
  expect_false(grepl("keepNext", rows[2], fixed = TRUE))
  expect_false(grepl("keepNext", rows[4], fixed = TRUE))
  expect_equal(keep_table_rows_together("<w:body><w:p/></w:body>"), "<w:body><w:p/></w:body>")
})

test_that("a footnote div after a table is styled as the table's footnote block", {
  skip_if_not_installed("rmarkdown")
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not available")
  dir <- withr::local_tempdir()
  rmd <- file.path(dir, "foot.Rmd")
  writeLines(c(
    "---", 'trial-short-name: "TEST"', 'report-type: "Test"',
    "output:", "  bctu::trial_report: default", "---", "", "# One", "",
    "| A | B |", "|---|---|", "| 1 | 2 |", "", "::: footnote", "^a^ Note.", ":::", "",
    "After.", ""), rmd)
  out <- rmarkdown::render(rmd, output_file = "foot.docx", output_dir = dir, quiet = TRUE)
  work <- withr::local_tempdir()
  utils::unzip(out, exdir = work)
  document <- paste(readLines(file.path(work, "word", "document.xml"), warn = FALSE), collapse = "")
  expect_match(document, "</w:tbl>\\s*<w:p><w:pPr><w:pStyle w:val=\"FootnoteBlockText\"")
})
