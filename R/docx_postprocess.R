################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       docx_postprocess.R                                           #
#   Description:  Repairs applied to a rendered trial report docx: header and  #
#                 footer relationship ids, and landscape image widths.         #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

# ---- Constants ----

HEADER_FOOTER_PLACEHOLDERS <- c(rIdHdr1 = "header1.xml", rIdHdr2 = "header2.xml",
                                rIdFtr1 = "footer1.xml", rIdFtr2 = "footer2.xml")

TWIP_EMU <- 635L
A4_LONG_TWIPS <- 16838L
A4_SHORT_TWIPS <- 11906L
PAGE_MARGIN_TWIPS <- 2880L

#' Repair a rendered trial report docx
#'
#' Applied by [trial_report()] to the file pandoc has just written. It binds
#' the title-page headers and footers to the relationship ids pandoc chose,
#' widens full-width images inside landscape sections to the landscape text
#' width, and keeps each table on one page with its caption.
#'
#' @param path Path to the docx.
#' @return `path`, invisibly.
#' @examples
#' \dontrun{
#' repair_report_docx("report.docx")
#' }
#' @export
repair_report_docx <- function(path) {
  if (!file.exists(path))
    cli::cli_abort("No file to repair at {.file {path}}.")
  path <- normalizePath(path, winslash = "/")
  work <- file.path(tempdir(), paste0("bctu-docx-", snapshot_id()))
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = work)

  doc_path <- file.path(work, "word", "document.xml")
  rels_path <- file.path(work, "word", "_rels", "document.xml.rels")
  if (!file.exists(doc_path) || !file.exists(rels_path))
    cli::cli_abort("{.file {path}} is not a Word document bctu can repair.")

  document <- readChar(doc_path, file.size(doc_path), useBytes = TRUE)
  rels <- readChar(rels_path, file.size(rels_path), useBytes = TRUE)
  document <- bind_header_footer_ids(document, rels, path)
  document <- widen_landscape_images(document)
  document <- keep_table_rows_together(document)
  writeChar(document, doc_path, eos = NULL, useBytes = TRUE)

  old <- setwd(work)
  on.exit(setwd(old), add = TRUE)
  unlink(path)
  status <- utils::zip(path, list.files(".", recursive = TRUE, all.files = TRUE),
                       flags = "-r9Xq")
  if (!identical(status, 0L) || !file.exists(path))
    cli::cli_abort("Could not rewrite {.file {path}}: the zip step failed.")
  invisible(path)
}

# ---- Header and footer relationship ids ----

#' Point the filter's header and footer placeholders at pandoc's ids
#'
#' The Lua filter writes fixed placeholder ids in its section breaks. Pandoc
#' numbers the header and footer relationships itself, so each placeholder is
#' replaced by the id whose target is that header or footer part.
#'
#' @param document The document.xml text.
#' @param rels The document.xml.rels text.
#' @param path Path to the docx, named in errors.
#' @return The document.xml text with the placeholders replaced.
#' @keywords internal
bind_header_footer_ids <- function(document, rels, path) {
  entries <- regmatches(rels, gregexpr("<Relationship[^>]*/>", rels))[[1]]
  target_id <- function(part) {
    hit <- entries[grepl(paste0("Target=\"", part, "\""), entries, fixed = FALSE)]
    if (!length(hit)) return(NA_character_)
    sub('^.*\\bId="([^"]+)".*$', "\\1", hit[1])
  }
  for (placeholder in names(HEADER_FOOTER_PLACEHOLDERS)) {
    if (!grepl(placeholder, document, fixed = TRUE)) next
    id <- target_id(HEADER_FOOTER_PLACEHOLDERS[[placeholder]])
    if (is.na(id))
      cli::cli_abort(c(
        "{.file {path}} has no relationship for {.val {HEADER_FOOTER_PLACEHOLDERS[[placeholder]]}}.",
        "i" = "The reference document must carry the BCTU headers and footers."
      ))
    document <- gsub(paste0("r:id=\"", placeholder, "\""),
                     paste0("r:id=\"", id, "\""), document, fixed = TRUE)
  }
  document
}

# ---- Landscape image widths ----

#' Widen full-width images inside landscape sections
#'
#' Pandoc sizes every image against the portrait text width of the reference
#' document, so a figure in a landscape section arrives at about two thirds
#' of the page. Each image that fills the portrait text width and sits in a
#' landscape section is scaled to the landscape text width, keeping its
#' aspect ratio. Narrower images are left alone.
#'
#' @param document The document.xml text.
#' @return The document.xml text with the extents rewritten.
#' @keywords internal
widen_landscape_images <- function(document) {
  FULL_WIDTH_FRACTION <- 0.98
  portrait_width <- (A4_SHORT_TWIPS - PAGE_MARGIN_TWIPS) * TWIP_EMU
  landscape_width <- (A4_LONG_TWIPS - PAGE_MARGIN_TWIPS) * TWIP_EMU

  sections <- gregexpr("<w:sectPr[^>]*>.*?</w:sectPr>", document)[[1]]
  if (identical(as.integer(sections), -1L)) return(document)
  section_end <- as.integer(sections) + attr(sections, "match.length") - 1L
  section_landscape <- grepl("w:orient=\"landscape\"",
                             substring(document, as.integer(sections), section_end),
                             fixed = TRUE)

  drawings <- gregexpr("<w:drawing>.*?</w:drawing>", document)[[1]]
  if (identical(as.integer(drawings), -1L)) return(document)
  starts <- as.integer(drawings)
  ends <- starts + attr(drawings, "match.length") - 1L

  for (i in rev(seq_along(starts))) {
    following <- which(section_end > starts[i])
    if (!length(following) || !section_landscape[following[1]]) next
    block <- substring(document, starts[i], ends[i])
    extent <- regmatches(block, regexpr("cx=\"[0-9]+\" cy=\"[0-9]+\"", block))
    if (!length(extent)) next
    width <- as.numeric(sub('^cx="([0-9]+)".*$', "\\1", extent))
    height <- as.numeric(sub('^.*cy="([0-9]+)"$', "\\1", extent))
    if (width < FULL_WIDTH_FRACTION * portrait_width) next
    new_extent <- sprintf("cx=\"%.0f\" cy=\"%.0f\"", landscape_width,
                          round(height * landscape_width / width))
    block <- gsub(extent, new_extent, block, fixed = TRUE)
    document <- paste0(substring(document, 1L, starts[i] - 1L), block,
                       substring(document, ends[i] + 1L))
  }
  document
}

# ---- Tables kept with their captions ----

#' Keep each table on one page with its caption
#'
#' The caption style carries keep-with-next, but Word only holds a caption to
#' a table when the table's own rows are kept together. Every paragraph in
#' every row but the last gets keep-with-next, so a table that fits on a page
#' moves to the next page as one block with its caption. A table longer than
#' a page still breaks.
#'
#' @param document The document.xml text.
#' @return The document.xml text with the paragraph properties added.
#' @keywords internal
keep_table_rows_together <- function(document) {
  tables <- gregexpr("<w:tbl>.*?</w:tbl>", document)[[1]]
  if (identical(as.integer(tables), -1L)) return(document)
  starts <- as.integer(tables)
  ends <- starts + attr(tables, "match.length") - 1L
  keep_paragraphs <- function(row) {
    row <- gsub("<w:p>(?!<w:pPr>)", "<w:p><w:pPr><w:keepNext/></w:pPr>", row, perl = TRUE)
    row <- gsub("(<w:p [^>]*>)(?!<w:pPr>)", "\\1<w:pPr><w:keepNext/></w:pPr>", row, perl = TRUE)
    gsub("(<w:p(?: [^>]*)?><w:pPr>)(?!<w:keepNext)", "\\1<w:keepNext/>", row, perl = TRUE)
  }
  for (i in rev(seq_along(starts))) {
    table <- substring(document, starts[i], ends[i])
    rows <- gregexpr("<w:tr(?: [^>]*)?>.*?</w:tr>", table, perl = TRUE)[[1]]
    if (identical(as.integer(rows), -1L) || length(rows) < 2L) next
    row_starts <- as.integer(rows)
    row_ends <- row_starts + attr(rows, "match.length") - 1L
    for (j in rev(seq_len(length(rows) - 1L))) {
      kept <- keep_paragraphs(substring(table, row_starts[j], row_ends[j]))
      table <- paste0(substring(table, 1L, row_starts[j] - 1L), kept,
                      substring(table, row_ends[j] + 1L, nchar(table)))
    }
    document <- paste0(substring(document, 1L, starts[i] - 1L), table,
                       substring(document, ends[i] + 1L, nchar(document)))
  }
  document
}
