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
#' width, keeps each caption with the start of its table, drops the empty
#' final section a report ends in when its last content is landscape,
#' applies any theme given, and fills the table of contents with the
#' headings (see [populate_toc()]).
#'
#' @param path Path to the docx.
#' @param theme A [report_theme()], a named list of its elements (the YAML
#'   `theme` key), or `NULL` for the template defaults.
#' @param template The template the docx was rendered from, from
#'   [report_template()].
#' @return `path`, invisibly.
#' @examples
#' \dontrun{
#' repair_report_docx("report.docx")
#' }
#' @export
repair_report_docx <- function(path, theme = NULL, template = report_template()) {
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
  document <- drop_trailing_empty_section(document)
  document <- populate_toc(document)
  writeChar(document, doc_path, eos = NULL, useBytes = TRUE)

  apply_theme(work, resolve_theme(as_report_theme(theme)), template)

  zip_docx(work, path)
  invisible(path)
}

zip_docx <- function(work, path) {
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE)
  unlink(path)
  status <- utils::zip(path, list.files(".", recursive = TRUE, all.files = TRUE), flags = "-r9Xq")
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

#' Keep the start of each table with its caption
#'
#' The caption style carries keep-with-next, but Word only holds a caption to
#' a table when the table's header row is kept with the row below it too.
#' Every paragraph in the first row gets keep-with-next, so the caption moves
#' to the next page with the start of the table, while a long table still
#' flows across pages and a tall first body row can still split.
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
    for (j in seq_len(min(1L, length(rows) - 1L))) {
      kept <- keep_paragraphs(substring(table, row_starts[j], row_ends[j]))
      table <- paste0(substring(table, 1L, row_starts[j] - 1L), kept,
                      substring(table, row_ends[j] + 1L, nchar(table)))
    }
    document <- paste0(substring(document, 1L, starts[i] - 1L), table,
                       substring(document, ends[i] + 1L, nchar(document)))
  }
  document
}

# ---- Trailing empty section ----

#' Drop an empty final section
#'
#' A `::: landscape` div closes with a section break paragraph, so a report
#' whose last content is landscape ends in a portrait section holding nothing
#' but the body's own section properties, which Word prints as a blank page.
#' When nothing but empty paragraphs and bookmarks follows the last section
#' break paragraph, that paragraph is removed and its section properties
#' become the body's, so the document ends in the landscape section.
#'
#' @param document The document.xml text.
#' @return The document.xml text.
#' @keywords internal
drop_trailing_empty_section <- function(document) {
  breaks <- gregexpr("<w:p>\\s*<w:pPr>\\s*<w:sectPr>.*?</w:sectPr>\\s*</w:pPr>\\s*</w:p>", document, perl = TRUE)[[1]]
  if (identical(as.integer(breaks), -1L)) return(document)
  last <- length(breaks)
  start <- as.integer(breaks)[last]
  end <- start + attr(breaks, "match.length")[last] - 1L
  body_sect <- regexpr("(?s)<w:sectPr>(?:(?!<w:sectPr>).)*</w:sectPr>\\s*</w:body>", document, perl = TRUE)
  if (body_sect == -1L || body_sect < end) return(document)
  between <- substring(document, end + 1L, body_sect - 1L)
  if (grepl("<w:t\\b|<w:tbl>|<w:drawing>|<w:br\\b", between, perl = TRUE)) return(document)
  section <- regmatches(substring(document, start, end), regexpr("(?s)<w:sectPr>.*</w:sectPr>", substring(document, start, end), perl = TRUE))
  body_end <- body_sect + attr(body_sect, "match.length") - 1L
  paste0(substring(document, 1L, start - 1L), between, section, "\n  </w:body>",
         substring(document, body_end + 1L, nchar(document)))
}

# ---- Theme ----

# Word's names for the theme colour slots as they appear in styles
# (w:themeColor) against their tags in theme1.xml.
THEME_SLOT_REFS <- c(dk1 = "text1", lt1 = "background1", dk2 = "text2", lt2 = "background2",
                     accent1 = "accent1", accent2 = "accent2", accent3 = "accent3",
                     accent4 = "accent4", accent5 = "accent5", accent6 = "accent6",
                     hlink = "hyperlink", folHlink = "followedHyperlink")

#' Apply a resolved theme to an unpacked report
#'
#' Each colour element is written to its theme slot in `theme1.xml` and to
#' the literal fallbacks beside every reference to that slot in the
#' document, styles, headers and footers (Word reads the slot, other
#' renderers the literal, so both agree and Word's Design menu still
#' works). Font elements replace the template's typefaces by role, and
#' `font.size` scales every size in the template.
#'
#' @param work The unpacked docx folder.
#' @param values A resolved theme, from [resolve_theme()].
#' @param template The template specification, from [report_template()]:
#'   its `fonts` and `font_size` are what the theme's replace.
#' @return `work`, invisibly.
#' @keywords internal
apply_theme <- function(work, values, template) {
  edit <- function(path, f) {
    if (!file.exists(path)) return(invisible())  # a docx built without a theme part keeps its literals only
    xml <- readChar(path, file.size(path), useBytes = TRUE)
    writeChar(f(xml), path, eos = NULL, useBytes = TRUE)
  }
  theme_path <- file.path(work, "word", "theme", "theme1.xml")
  parts <- list.files(file.path(work, "word"), "^(document|styles|header[0-9]*|footer[0-9]*)\\.xml$", full.names = TRUE)
  slots <- vapply(Filter(function(s) !is.null(s$slot), template$elements), `[[`, character(1), "slot")
  colours <- stats::setNames(unlist(values[names(slots)]), slots)
  roles <- vapply(Filter(function(s) !is.null(s$role), template$elements), `[[`, character(1), "role")
  fonts <- stats::setNames(unlist(values[names(roles)]), roles)
  edit(theme_path, function(xml) set_theme_fonts(set_theme_colours(xml, colours), fonts))
  for (part in parts) edit(part, function(xml)
    scale_font_sizes(set_font_literals(set_theme_literals(xml, colours), fonts, template$fonts),
                     values$font.size, template$font_size))
  invisible(work)
}

#' Replace entries of a theme's colour scheme
#'
#' @param theme The theme1.xml text.
#' @param colours Hex colours named by slot tag (`accent1`, `dk2`, `hlink`).
#' @return The theme1.xml text with those entries replaced.
#' @keywords internal
set_theme_colours <- function(theme, colours) {
  for (tag in names(colours))
    theme <- sub(paste0("(?s)<a:", tag, ">.*?</a:", tag, ">"),
                 paste0("<a:", tag, "><a:srgbClr val=\"", colours[[tag]], "\"/></a:", tag, ">"),
                 theme, perl = TRUE)
  theme
}

#' Rewrite the literal fallbacks of theme-bound colours
#'
#' Word reads a `w:themeColor` or `w:themeFill` attribute and ignores the
#' literal `w:color`, `w:val` or `w:fill` beside it; other renderers do the
#' reverse. Every literal that sits beside a reference to a slot being set
#' is rewritten to the new colour, with `w:themeFillTint` and
#' `w:themeShade` applied, so both readings agree.
#'
#' @param xml The text of a document, styles, header or footer part.
#' @param colours Hex colours named by slot tag.
#' @return The part with its literal colours rewritten.
#' @keywords internal
set_theme_literals <- function(xml, colours) {
  scheme <- stats::setNames(colours, THEME_SLOT_REFS[names(colours)])
  mix <- function(base, amount, towards) {
    rgb <- grDevices::col2rgb(paste0("#", base))
    sub("^#", "", grDevices::rgb(t(round(rgb * (1 - amount) + towards * amount)), maxColorValue = 255))
  }
  rewrite <- function(match) {
    ref <- sub('.*w:theme(?:Color|Fill)="([A-Za-z0-9]+)".*', "\\1", match)
    if (!ref %in% names(scheme)) return(match)
    colour <- scheme[[ref]]
    tint <- regmatches(match, regexpr('w:theme(?:Fill)?Tint="[0-9A-Fa-f]+"', match))
    shade <- regmatches(match, regexpr('w:theme(?:Fill)?Shade="[0-9A-Fa-f]+"', match))
    if (length(tint)) colour <- mix(colour, 1 - strtoi(sub('.*"([0-9A-Fa-f]+)"', "\\1", tint), 16L) / 255, 255)
    if (length(shade)) colour <- mix(colour, 1 - strtoi(sub('.*"([0-9A-Fa-f]+)"', "\\1", shade), 16L) / 255, 0)
    sub('(w:(?:color|fill|val))="[0-9A-Fa-f]{6}"', paste0("\\1=\"", toupper(colour), "\""), match, perl = TRUE)
  }
  pattern <- '<w:[a-zA-Z]+ [^>]*w:theme(?:Color|Fill)="[A-Za-z0-9]+"[^>]*/?>'
  hits <- gregexpr(pattern, xml, perl = TRUE)
  regmatches(xml, hits) <- list(vapply(regmatches(xml, hits)[[1]], rewrite, character(1), USE.NAMES = FALSE))
  xml
}

#' Set the theme's major and minor fonts
#'
#' @param theme The theme1.xml text.
#' @param fonts Typefaces named by role; `heading` sets the major font and
#'   `body` the minor.
#' @return The theme1.xml text.
#' @keywords internal
set_theme_fonts <- function(theme, fonts) {
  for (role in intersect(names(fonts), c("heading", "body"))) {
    tag <- if (role == "heading") "majorFont" else "minorFont"
    theme <- sub(paste0("(?s)(<a:", tag, ">\\s*<a:latin typeface=\")[^\"]*"),
                 paste0("\\1", fonts[[role]]), theme, perl = TRUE)
  }
  theme
}

#' Swap the template's typefaces for the fonts named
#'
#' Every `w:rFonts` attribute naming a template font is rewritten to the
#' font chosen for that role: `heading` in the heading styles, `title` for
#' the title acronym, `code` for code, `body` everywhere else.
#'
#' @param xml The text of a document, styles, header or footer part.
#' @param fonts Typefaces named by role.
#' @param template_fonts The typefaces the template is written in, by role.
#' @return The part with its fonts rewritten.
#' @keywords internal
set_font_literals <- function(xml, fonts, template_fonts) {
  swap <- function(text, from, to)
    gsub(paste0('(w:(?:ascii|hAnsi|cs|eastAsia))="', from, '"'), paste0('\\1="', to, '"'), text, perl = TRUE)
  styles <- gregexpr("(?s)<w:style [^>]*>.*?</w:style>|<w:docDefaults>.*?</w:docDefaults>", xml, perl = TRUE)
  regmatches(xml, styles) <- list(vapply(regmatches(xml, styles)[[1]], function(style) {
    role <- if (grepl('w:styleId="Heading[0-9]', style)) "heading" else "body"
    style <- swap(style, template_fonts[["body"]], fonts[[role]])
    style <- swap(style, template_fonts[["title"]], fonts[["title"]])
    swap(style, template_fonts[["code"]], fonts[["code"]])
  }, character(1), USE.NAMES = FALSE))
  swap(xml, template_fonts[["body"]], fonts[["body"]])
}

#' Scale every font size in a part
#'
#' @param xml The text of a document, styles, header or footer part.
#' @param body_pt The body size in points wanted.
#' @param template_pt The template's body size in points; every `w:sz` and
#'   `w:szCs` is scaled by their ratio.
#' @return The part with its sizes scaled.
#' @keywords internal
scale_font_sizes <- function(xml, body_pt, template_pt) {
  factor <- body_pt / template_pt
  if (factor == 1) return(xml)
  hits <- gregexpr('<w:sz(?:Cs)? w:val="[0-9]+"', xml)
  regmatches(xml, hits) <- list(vapply(regmatches(xml, hits)[[1]], function(m)
    sub('"[0-9]+"', paste0('"', max(2L, round(as.integer(sub('.*"([0-9]+)"', "\\1", m)) * factor)), '"'), m),
    character(1), USE.NAMES = FALSE))
  xml
}
