nbsp <- "\u00a0"

pandoc_docx_text <- function(markdown) {
  md <- withr::local_tempfile(fileext = ".md")
  docx <- withr::local_tempfile(fileext = ".docx")
  writeLines(enc2utf8(markdown), md, useBytes = TRUE)
  system2("pandoc", c(shQuote(md), "-o", shQuote(docx)))
  system2("pandoc", c(shQuote(docx), "-t", "plain", "--wrap=none"), stdout = TRUE)
}

test_that("indent prefixes four non-breaking spaces per level", {
  expect_equal(indent("a"), paste0(strrep(nbsp, 4), "a"))
  expect_equal(indent(c("a", "b"), levels = 2L), paste0(strrep(nbsp, 8), c("a", "b")))
})

test_that("n_pct formats n/N (p%) and drops the percentage when N is 0", {
  expect_equal(n_pct(c(3, 0), c(12, 0)), c("3/12 (25%)", "0/0"))
  expect_equal(n_pct(1, 3, digits = 1L), "1/3 (33.3%)")
})

test_that("col_widths clamps the longest line to between 8 and the cap", {
  df <- data.frame(a = c("x", "a much longer cell text"), b = c("one\ntwo", "y"))
  expect_equal(col_widths(df, caps = 12), c(12L, 8L))
  expect_equal(col_widths(df, caps = c(40, 20)), c(23L, 8L))
  expect_error(col_widths(df, caps = c(1, 2, 3)), "one per column")
})

test_that("grid_table keeps non-breaking-space indentation", {
  df <- data.frame(Characteristic = indent("Male"), Total = "10")
  out <- grid_table(df, widths = c(12, 8))
  expect_match(out, paste0("| ", strrep(nbsp, 4), "Male"), fixed = TRUE)
})

test_that("grid_table aligns rows, wraps text and separates in-cell paragraphs", {
  df <- data.frame(a = c("first\nsecond", "a b c d e f g h i j"), b = c("1", "2"))
  lines <- strsplit(grid_table(df, widths = c(10, 8)), "\n")[[1]]
  expect_equal(length(unique(nchar(lines))), 1L)
  expect_equal(lines[1], "+------------+----------+")
  expect_equal(lines[3], "+============+==========+")
  expect_true("| first      | 1        |" %in% lines)
  expect_true("|            |          |" %in% lines)
  expect_true("| second     |          |" %in% lines)
  expect_true(all(nchar(trimws(substr(lines, 3, 12))) <= 10))
})

test_that("grid_table renders span rows as one full-width cell", {
  df <- data.frame(a = c("Banner text", "x"), b = c("", "1"))
  lines <- strsplit(grid_table(df, widths = c(8, 8), span_rows = c(TRUE, FALSE)), "\n")[[1]]
  expect_true("| Banner text         |" %in% lines)
  expect_error(grid_table(df, widths = 8), "one value per column")
})

test_that("banner_row copies the template's columns with text in the first", {
  tab <- data.frame(Characteristic = "x", Total = "1", check.names = FALSE)
  row <- banner_row(tab, "Sex")
  expect_equal(names(row), names(tab))
  expect_equal(unlist(row, use.names = FALSE), c("Sex", ""))
})

test_that("render_table escapes leading list markers, blanks NA and adds a caption", {
  df <- data.frame(Level = c("+", "++", "- (-), 0"), n = c(1, NA, 3))
  out <- render_table(df, caption = "Dipstick")
  expect_match(out, "^Table: Dipstick\n\n\\+-")
  expect_match(out, "| \\+ ", fixed = TRUE)
  expect_match(out, "| \\++ ", fixed = TRUE)
  expect_match(out, "| \\- (-), 0 ", fixed = TRUE)
  expect_false(grepl("NA", out))
  indented <- render_table(data.frame(Level = indent("*"), n = "4"))
  expect_match(indented, "| \\* ", fixed = TRUE)
})

test_that("render_table bolds headings and makes value-less headings banners", {
  tab <- rbind(banner_row(data.frame(a = "", b = ""), "Sex"),
               data.frame(a = indent("Male"), b = "3"),
               data.frame(a = "Total", b = "3"))
  lines <- strsplit(render_table(tab, full_width = FALSE), "\n")[[1]]
  expect_true(any(grepl("^\\| \\*\\*Sex\\*\\* +\\|$", lines)))
  expect_true(any(grepl("^\\| \\*\\*Total\\*\\* +\\| 3 +\\|$", lines)))
})

test_that("render_table turns indent levels into merged label columns", {
  tab <- data.frame(a = c("Cure", indent("Missing"), indent("Deep", 2L)), b = c("3/4", "1", "0"))
  lines <- strsplit(render_table(tab, full_width = FALSE, bold_headings = FALSE), "\n")[[1]]
  expect_match(lines[1], "^\\+-+\\+-+\\+-+\\+-+\\+$")
  expect_true(any(grepl("^\\| a +\\| b +\\|$", lines)))
  expect_true(any(grepl("^\\| Cure +\\| 3/4 +\\|$", lines)))
  expect_true(any(grepl("^\\| +\\| Missing +\\| 1 +\\|$", lines)))
  expect_true(any(grepl("^\\| +\\| +\\| Deep +\\| 0 +\\|$", lines)))
  expect_equal(indent_depth(c("x", indent("y"), indent("z", 3L))), c(0L, 1L, 3L))
})

test_that("render_table bolds every cell of bold_rows", {
  tab <- data.frame(a = c("Cure", indent("Missing")), b = c("3/4 (75%)", "1"))
  lines <- strsplit(render_table(tab, full_width = FALSE, bold_rows = 1L), "\n")[[1]]
  expect_true(any(grepl("^\\| \\*\\*Cure\\*\\* +\\| \\*\\*3/4 \\(75%\\)\\*\\* +\\|$", lines)))
  expect_true(any(grepl("Missing +\\| 1 +\\|$", lines)))
})

test_that("render_table scales narrow tables past pandoc's wrap column", {
  df <- data.frame(a = "x", b = "y")
  wide <- strsplit(render_table(df), "\n")[[1]][1]
  narrow <- strsplit(render_table(df, full_width = FALSE), "\n")[[1]][1]
  expect_gte(nchar(wide), 300L)
  expect_equal(nchar(narrow), 23L)
  expect_match(render_table(df, col_names = c("Name A", "Name B")), "| Name A ", fixed = TRUE)
  expect_error(render_table(df, col_names = "A"), "one name per column")
})

test_that("'+' and '- (-), 0' cells survive pandoc conversion to docx", {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not available")
  df <- data.frame(Level = c("+", "++", "- (-), 0"), n = c("1", "2", "3"))
  text <- pandoc_docx_text(render_table(df))
  expect_true(any(grepl("^\\s*\\+\\s+1\\s*$", text)))
  expect_true(any(grepl("^\\s*\\+\\+\\s+2\\s*$", text)))
  expect_true(any(grepl("- (-), 0", text, fixed = TRUE)))
})

test_that("trial_report builds a word_document format with the bundled assets", {
  skip_if_not_installed("rmarkdown")
  fmt <- trial_report()
  expect_s3_class(fmt, "rmarkdown_output_format")
  args <- fmt$pandoc$args
  expect_true(any(grepl("title_page.lua", args, fixed = TRUE)))
  expect_true(file.exists(system.file("rmarkdown", "templates", "report", "resources",
                                      "reference.docx", package = "bctu")))
  expect_warning(trial_report(reference_docx = "other.docx"), "instead of the bundled")
})
