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

test_that("an empty final section after a landscape break is dropped", {
  landscape <- "<w:p><w:pPr><w:sectPr><w:pgSz w:w=\"16838\" w:h=\"11906\" w:orient=\"landscape\"/></w:sectPr></w:pPr></w:p>"
  portrait <- "<w:sectPr>\n  <w:pgSz w:w=\"11906\" w:h=\"16838\"/>\n</w:sectPr>\n"
  empty_tail <- paste0("<w:body><w:p><w:r><w:t>x</w:t></w:r></w:p>", landscape, "<w:p/><w:bookmarkEnd w:id=\"1\"/>", portrait, "</w:body>")
  out <- drop_trailing_empty_section(empty_tail)
  expect_equal(lengths(regmatches(out, gregexpr("<w:sectPr>", out)))[[1]], 1L)
  expect_true(grepl('w:orient="landscape"', out, fixed = TRUE))
  expect_true(grepl("<w:t>x</w:t>", out, fixed = TRUE))
  content_tail <- paste0("<w:body>", landscape, "<w:p><w:r><w:t>after</w:t></w:r></w:p>", portrait, "</w:body>")
  expect_equal(drop_trailing_empty_section(content_tail), content_tail)
})

test_that("a theme resolves by inheritance and lands in the theme and the literals", {
  expect_equal(resolve_theme(report_theme())$table.header.fill, "F3EBCC")
  blue <- resolve_theme(report_theme(colour.accent = "#0057BF", font.body = "Georgia"))
  expect_equal(blue$rule.colour, "0057BF")
  expect_equal(blue$table.border.colour, "0057BF")
  expect_equal(blue$table.header.fill, tint("0057BF", 0.8))
  expect_equal(blue$font.heading, "Georgia")
  expect_equal(resolve_theme(report_theme(rule.colour = "FF0000"))$colour.accent, "C59A00")
  expect_error(as_report_theme(list(accent = "x")), "Unknown theme element")
  expect_error(resolve_theme(report_theme(link.colour = "blue")), "six-digit hex")
  expect_error(resolve_theme(report_theme(font.size = "big")), "positive number")

  theme <- "<a:dk2><a:srgbClr val=\"111111\"/></a:dk2><a:accent1><a:srgbClr val=\"222222\"/></a:accent1><a:majorFont><a:latin typeface=\"Calibri\"/></a:majorFont><a:minorFont><a:latin typeface=\"Cambria\"/></a:minorFont>"
  out <- set_theme_fonts(set_theme_colours(theme, c(accent1 = "ABCDEF")), c(body = "Georgia", heading = "Verdana"))
  expect_true(grepl("<a:accent1><a:srgbClr val=\"ABCDEF\"/></a:accent1>", out, fixed = TRUE))
  expect_true(grepl("111111", out, fixed = TRUE))
  expect_true(grepl("<a:majorFont><a:latin typeface=\"Verdana\"", out, fixed = TRUE))
  expect_true(grepl("<a:minorFont><a:latin typeface=\"Georgia\"", out, fixed = TRUE))

  xml <- paste0('<w:top w:val="single" w:color="C59A00" w:themeColor="accent2"/>',
                '<w:shd w:val="clear" w:fill="F5EDD4" w:themeFill="accent1" w:themeFillTint="33"/>',
                '<w:color w:val="3C3C3B" w:themeColor="text2"/>')
  out <- set_theme_literals(xml, c(accent2 = "0057BF", accent1 = "FF0000"))
  expect_true(grepl('w:color="0057BF" w:themeColor="accent2"', out, fixed = TRUE))
  expect_true(grepl('w:fill="FFCCCC" w:themeFill="accent1"', out, fixed = TRUE))
  expect_true(grepl('w:val="3C3C3B" w:themeColor="text2"', out, fixed = TRUE))

  styles <- paste0('<w:docDefaults><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr></w:docDefaults>',
                   '<w:style w:styleId="Heading1"><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="32"/></w:style>',
                   '<w:style w:styleId="TitleAcronym"><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"/></w:style>')
  out <- set_font_literals(styles, c(body = "Georgia", heading = "Verdana", title = "Impact", code = "Consolas"), report_template()$fonts)
  expect_true(grepl('<w:docDefaults><w:rPr><w:rFonts w:ascii="Georgia" w:hAnsi="Georgia"/>', out, fixed = TRUE))
  expect_true(grepl('Heading1"><w:rFonts w:ascii="Verdana" w:hAnsi="Verdana"/>', out, fixed = TRUE))
  expect_true(grepl('w:ascii="Impact" w:hAnsi="Impact"', out, fixed = TRUE))
  sized <- scale_font_sizes(styles, 22, 11)
  expect_true(grepl('<w:sz w:val="44"/>', sized, fixed = TRUE))
  expect_true(grepl('<w:sz w:val="64"/>', sized, fixed = TRUE))
})

test_that("the TOC field is filled with linked, numbered entries", {
  heading <- function(id, name, level, number, title) paste0(
    '<w:bookmarkStart w:id="', id, '" w:name="', name, '" /><w:p><w:pPr><w:pStyle w:val="Heading', level, '" /></w:pPr>',
    '<w:r><w:rPr><w:rStyle w:val="SectionNumber" /></w:rPr><w:t xml:space="preserve">', number, '</w:t></w:r><w:r><w:tab /></w:r>',
    '<w:r><w:t xml:space="preserve">', title, '</w:t></w:r></w:p>')
  toc <- paste0('<w:p><w:r><w:fldChar w:fldCharType="begin" w:dirty="true"/></w:r><w:r><w:instrText xml:space="preserve">TOC \\o &quot;1-2&quot; \\h \\z \\u</w:instrText></w:r>',
                '<w:r><w:fldChar w:fldCharType="separate"/></w:r><w:r><w:t>placeholder</w:t></w:r><w:r><w:fldChar w:fldCharType="end"/></w:r></w:p>')
  document <- paste0("<w:body>", toc, heading(1, "one", 1, "1", "One"), heading(2, "one-a", 2, "1.1", "One A"), heading(3, "deep", 3, "1.1.1", "Deep"), "</w:body>")
  out <- populate_toc(document)
  expect_false(grepl("placeholder", out, fixed = TRUE))
  expect_equal(lengths(regmatches(out, gregexpr('<w:pStyle w:val="TOC[12]"/>', out)))[[1]], 2L)
  expect_false(grepl('w:val="TOC3"', out, fixed = TRUE))
  expect_true(grepl('<w:hyperlink w:anchor="one-a" w:history="1">', out, fixed = TRUE))
  expect_true(grepl('PAGEREF one \\h </w:instrText></w:r><w:r><w:fldChar w:fldCharType="separate"/></w:r><w:r><w:t xml:space="preserve"></w:t>', out, fixed = TRUE))
  expect_false(grepl("dirty", out, fixed = TRUE))
  expect_equal(lengths(regmatches(out, gregexpr('fldCharType="begin"', out)))[[1]], 3L)
  expect_equal(lengths(regmatches(out, gregexpr('fldCharType="end"', out)))[[1]], 3L)
})

test_that("report_styles lists the template's styles by pandoc name", {
  styles <- report_styles()
  expect_true(all(c("Table Caption", "Footnote Block Text", "Section Number", "TOC1") %in% c(styles$name, styles$id)))
  expect_setequal(unique(styles$type), c("paragraph", "character", "table"))
  expect_false(any(duplicated(styles$id)))
})
