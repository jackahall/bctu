################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       docx_toc.R                                                   #
#   Description:  Populate the Word table of contents of a rendered report:   #
#                 the TOC field stays Word's own and opens already filled     #
#                 with the headings; Word fills the page numbers on its       #
#                 first update.                                               #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Fill a report's table of contents
#'
#' Pandoc leaves a Word TOC field whose result is a placeholder sentence and
#' asks Word to update every field on opening, which Word does only after a
#' prompt. This writes the field result itself, as Word would: one entry per
#' heading within the field's depth, styled `TOC1` to `TOC3` (the template's
#' styles carry the tab stops and dot leaders), linked to the heading's
#' bookmark, ending in a `PAGEREF` field. The entries show at once in any
#' viewer; the page numbers appear when the field is updated in Word (F9 on
#' the table, or right-click, Update Field), which only Word's layout can
#' do. Nothing is flagged for update on opening, so Word opens quietly.
#'
#' @param document The document.xml text.
#' @return The document.xml text.
#' @keywords internal
populate_toc <- function(document) {
  field <- regexpr('<w:p>(?:(?!</w:p>).)*<w:instrText[^>]*>\\s*TOC \\\\o &quot;1-([0-9])&quot;.*?</w:p>', document, perl = TRUE)
  if (field == -1L) return(document)
  depth <- as.integer(sub('.*TOC \\\\o &quot;1-([0-9])&quot;.*', "\\1", regmatches(document, field)))
  headings <- toc_headings(document, depth)
  if (!nrow(headings)) return(document)
  entry <- function(h, first, last) paste0(
    '<w:p><w:pPr><w:pStyle w:val="TOC', h$level, '"/></w:pPr>',
    if (first) paste0('<w:r><w:fldChar w:fldCharType="begin"/></w:r>',
                      '<w:r><w:instrText xml:space="preserve">TOC \\o "1-', depth, '" \\h \\z \\u</w:instrText></w:r>',
                      '<w:r><w:fldChar w:fldCharType="separate"/></w:r>'),
    '<w:hyperlink w:anchor="', h$bookmark, '" w:history="1">',
    if (nzchar(h$number)) paste0('<w:r><w:t xml:space="preserve">', h$number, '</w:t></w:r><w:r><w:tab/></w:r>'),
    '<w:r><w:t xml:space="preserve">', h$title, '</w:t></w:r><w:r><w:tab/></w:r>',
    '<w:r><w:fldChar w:fldCharType="begin"/></w:r>',
    '<w:r><w:instrText xml:space="preserve"> PAGEREF ', h$bookmark, ' \\h </w:instrText></w:r>',
    '<w:r><w:fldChar w:fldCharType="separate"/></w:r>',
    '<w:r><w:t xml:space="preserve"></w:t></w:r>',
    '<w:r><w:fldChar w:fldCharType="end"/></w:r>',
    '</w:hyperlink>',
    if (last) '<w:r><w:fldChar w:fldCharType="end"/></w:r>',
    '</w:p>')
  n <- nrow(headings)
  entries <- vapply(seq_len(n), function(i) entry(headings[i, ], i == 1L, i == n), character(1))
  regmatches(document, field) <- paste(entries, collapse = "")
  document
}

#' The headings a TOC lists
#'
#' @param document The document.xml text.
#' @param depth The deepest heading level to include.
#' @return A data frame: `level`, `bookmark`, `number`, `title`.
#' @keywords internal
toc_headings <- function(document, depth) {
  pattern <- paste0('<w:bookmarkStart w:id="[0-9]+" w:name="([^"]+)"\\s*/>\\s*',
                    '<w:p><w:pPr><w:pStyle w:val="Heading([1-', depth, '])"\\s*/></w:pPr>(.*?)</w:p>')
  hits <- gregexpr(pattern, document, perl = TRUE)[[1]]
  if (identical(as.integer(hits), -1L)) return(data.frame(level = integer(), bookmark = character(), number = character(), title = character()))
  rows <- lapply(regmatches(document, list(hits))[[1]], function(m) {
    parts <- regmatches(m, regexec(pattern, m, perl = TRUE))[[1]]
    runs <- parts[4]
    number <- regmatches(runs, regexpr('<w:rStyle w:val="SectionNumber"\\s*/></w:rPr><w:t[^>]*>[^<]*', runs))
    number <- if (length(number)) sub(".*>", "", number) else ""
    text <- gsub("<w:tab\\s*/>", " ", runs)
    text <- paste(regmatches(text, gregexpr("(?<=<w:t>|<w:t xml:space=\"preserve\">)[^<]*", text, perl = TRUE))[[1]], collapse = "")
    title <- trimws(sub(paste0("^", number, "\\s*"), "", text))
    data.frame(level = as.integer(parts[3]), bookmark = parts[2], number = number, title = title, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
