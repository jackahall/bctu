################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       slides.R                                                     #
#   Description:  PowerPoint decks on the University of Birmingham slide       #
#                 template: a title slide, section dividers, and one slide per #
#                 figure or table, built with officer and flextable.           #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

SLIDE_MASTER <- "UOB"
NOTE_PT <- 14

#' PowerPoint decks on the UoB template
#'
#' `trial_slides()` writes a PowerPoint deck on the bundled University of
#' Birmingham slide template: a title slide, then one slide per element of
#' `slides`. An element is a section divider (`slide_section()`) or a content
#' slide (`slide_content()`) holding a ggplot figure or a data frame.
#'
#' Figures take the slide's background colour and a larger base text size.
#' Data frames become native PowerPoint tables in the template's table style
#' (UoB green header with white bold text, banded green rows, white rules,
#' the theme's Calibri font). A table's font grows to fill the slide, from
#' `font_size` (at least 14 pt) up to 24 pt. Rows indented with [indent()]
#' stay indented. A table too long for one slide at `font_size` continues on
#' further slides headed "(continued)", so no text is shrunk.
#' Needs the officer and flextable packages.
#'
#' @param path Output `.pptx` path.
#' @param title,subtitle Title slide text.
#' @param slides A list of `slide_section()` and `slide_content()` elements.
#' @param template Path to a `.potx` or `.pptx` template with an `"UOB"`
#'   master. Default the bundled UoB template from `slide_template()`.
#' @return `trial_slides()`: `path`, invisibly.
#' @examples
#' \dontrun{
#' trial_slides("deck.pptx", "CURLY Trial Management Group", "Data extraction 30 September 2026",
#'   list(slide_section("Recruitment"),
#'        slide_content("Recruitment by month", plot),
#'        slide_content("Form return rates", table, note = "Forms returned / due (%).")))
#' }
#' @export
trial_slides <- function(path, title, subtitle = NULL, slides, template = slide_template()) {
  for (pkg in c("officer", "flextable", "ggplot2"))
    if (!requireNamespace(pkg, quietly = TRUE))
      cli::cli_abort(c("Package {.pkg {pkg}} is needed for {.fn trial_slides}.",
                       "i" = "Install it with {.code install.packages(\"{pkg}\")}."))
  deck <- officer::read_pptx(template_as_pptx(template))
  for (i in rev(seq_len(length(deck)))) deck <- officer::remove_slide(deck, i)
  background <- slide_background(template)

  deck <- officer::add_slide(deck, "Light Title Slide", SLIDE_MASTER)
  deck <- officer::ph_with(deck, title, officer::ph_location_type("ctrTitle"))
  if (!is.null(subtitle)) deck <- officer::ph_with(deck, subtitle, officer::ph_location_type("subTitle"))
  area <- officer::layout_properties(deck, "Title and Content", SLIDE_MASTER)
  area <- area[area$ph_label == "3. Content Placeholder", ]

  for (s in slides) {
    if (inherits(s, "slide_section")) {
      deck <- officer::add_slide(deck, "Light Divider Slide - 1", SLIDE_MASTER)
      deck <- officer::ph_with(deck, s$title, officer::ph_location_type("ctrTitle"))
      next
    }
    if (!inherits(s, "slide_content"))
      cli::cli_abort("Each element of {.arg slides} must come from {.fn slide_section} or {.fn slide_content}.")
    note_h <- if (is.null(s$note)) 0
              else ceiling(nchar(s$note) * 0.5 * NOTE_PT / 72 / area$cx) * NOTE_PT * 1.25 / 72 + 0.1
    height <- area$cy - note_h
    pages <- if (inherits(s$content, "ggplot")) list(s$content)
             else slide_table_pages(s$content, area$cx, height, s$font_size, s$bold_rows, s$bold_headings)
    for (k in seq_along(pages)) {
      deck <- officer::add_slide(deck, "Title and Content", SLIDE_MASTER)
      deck <- officer::ph_with(deck, if (k == 1L) s$title else paste(s$title, "(continued)"),
                               officer::ph_location_type("title"))
      box <- officer::ph_location(left = area$offx, top = area$offy, width = area$cx, height = height)
      content <- if (inherits(pages[[k]], "ggplot"))
        slide_text_layers(pages[[k]], s$font_size) + ggplot2::theme(
          text = ggplot2::element_text(size = s$font_size),
          plot.background = ggplot2::element_rect(fill = background, colour = NA),
          panel.background = ggplot2::element_rect(fill = background, colour = NA),
          legend.background = ggplot2::element_rect(fill = background, colour = NA))
      else pages[[k]]
      deck <- officer::ph_with(deck, content, box)
      if (!is.null(s$note))
        deck <- officer::ph_with(deck, officer::fpar(officer::ftext(s$note, officer::fp_text(font.size = NOTE_PT, italic = TRUE,
                                                                                             font.family = "Calibri"))),
                                 officer::ph_location(left = area$offx, top = area$offy + height,
                                                      width = area$cx, height = note_h))
    }
  }
  print(deck, target = path)
  invisible(path)
}

#' @describeIn trial_slides A section divider slide
#' @export
slide_section <- function(title) {
  structure(list(title = title), class = "slide_section")
}

#' @describeIn trial_slides A slide holding one figure or table
#' @param content A ggplot or a data frame.
#' @param note Optional footnote text under the content (14 pt), in a box
#'   sized to its length.
#' @param bold_rows,bold_headings For a table, as in [render_table()]: body
#'   rows shown in bold, and whether unindented rows of an indented table are
#'   bold.
#' @param font_size For a table, the smallest text size in points (the
#'   table grows to fill the slide, up to 24); for a figure, the base text
#'   size. At least 14.
#' @export
slide_content <- function(title, content, note = NULL, bold_rows = NULL, bold_headings = TRUE, font_size = 16) {
  if (!inherits(content, "ggplot") && !is.data.frame(content))
    cli::cli_abort("{.arg content} must be a ggplot or a data frame.")
  if (font_size < 14) cli::cli_abort("{.arg font_size} must be at least 14 points.")
  structure(list(title = title, content = content, note = note, bold_rows = bold_rows,
                 bold_headings = bold_headings, font_size = font_size), class = "slide_content")
}

#' @describeIn trial_slides Path to the bundled UoB slide template
#' @export
slide_template <- function() {
  system.file("powerpoint", "uob-slides-template.potx", package = "bctu", mustWork = TRUE)
}

#' A .pptx copy of a template that officer can open
#'
#' officer reads presentations, not templates: a `.potx` differs only in the
#' content type of its main part, so a copy with that type changed is written
#' to a temporary file. A `.pptx` is returned unchanged.
#' @param template Path to the `.potx` or `.pptx`.
#' @return Path to a `.pptx`.
#' @noRd
template_as_pptx <- function(template) {
  if (!file.exists(template)) cli::cli_abort("No slide template at {.file {template}}.")
  if (!grepl("\\.potx$", template, ignore.case = TRUE)) return(template)
  work <- tempfile("bctu-slides-")
  dir.create(work)
  utils::unzip(template, exdir = work)
  types <- file.path(work, "[Content_Types].xml")
  xml <- readChar(types, file.size(types), useBytes = TRUE)
  writeChar(sub("presentationml.template.main+xml", "presentationml.presentation.main+xml", xml, fixed = TRUE),
            types, eos = NULL, useBytes = TRUE)
  out <- file.path(work, "template.pptx")
  zip::zip(out, list.files(work, recursive = TRUE, all.files = TRUE), root = work)
  out
}

#' A ggplot with its text layers at least a slide-readable size
#'
#' Value labels drawn with geom_text or geom_label at a fixed size are
#' raised to at least three quarters of the slide text size; larger labels
#' are left alone.
#' @param plot A ggplot.
#' @param font_size The slide text size, points.
#' @return The ggplot.
#' @noRd
slide_text_layers <- function(plot, font_size) {
  smallest <- 0.75 * font_size / ggplot2::.pt
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    if (inherits(layer$geom, c("GeomText", "GeomLabel")) && !is.null(layer$aes_params$size) && layer$aes_params$size < smallest)
      plot$layers[[i]]$aes_params$size <- smallest
  }
  plot
}

#' The template's slide background colour, from its slide master
#'
#' @param template Path to the `.potx` or `.pptx`.
#' @return A `"#RRGGBB"` colour.
#' @noRd
slide_background <- function(template) {
  work <- tempfile("bctu-slides-")
  utils::unzip(template, files = "ppt/slideMasters/slideMaster1.xml", exdir = work)
  xml <- paste(readLines(file.path(work, "ppt", "slideMasters", "slideMaster1.xml"), warn = FALSE), collapse = "")
  hex <- regmatches(xml, regexec('<p:bg>.*?<a:srgbClr val="([0-9A-Fa-f]{6})"', xml))[[1]][2]
  if (is.na(hex)) cli::cli_abort("The slide master of {.file {template}} has no solid background colour.")
  paste0("#", hex)
}

#' A data frame as template-styled tables, split into slide-sized pages
#'
#' The font is the largest from 24 pt down to `min_size` at which the whole
#' table fits one slide, first without wrapping, then with wrapping. Columns
#' take their natural width; when the table is wider than the space, the wide
#' columns share what the narrow ones leave and their text wraps. A table
#' that does not fit at `min_size` goes onto pages, each with the header,
#' filled to the estimated height (from an average Calibri character width).
#' A data frame with no column names gets no header row.
#' @param df A data frame.
#' @param width,height The space available, in inches.
#' @param min_size Smallest font size, points.
#' @param bold_rows,bold_headings As in [render_table()].
#' @return A list of flextables, one per slide.
#' @noRd
slide_table_pages <- function(df, width, height, min_size, bold_rows = NULL, bold_headings = TRUE) {
  MAX_SIZE <- 24
  CHAR_EM  <- 0.5    # average Calibri character width, in ems
  PAD_IN   <- 0.2    # left plus right cell padding, inches
  ROW_PAD  <- 0.06   # top plus bottom cell padding, inches
  HEADER <- "#007838"; BAND1 <- "#CBE3D5"; BAND2 <- "#E7F1EB"
  df <- as.data.frame(lapply(df, function(col) { x <- as.character(col); x[is.na(x)] <- ""; x }),
                      check.names = FALSE, stringsAsFactors = FALSE)
  header <- names(df)
  show_header <- any(nzchar(trimws(header)))
  names(df) <- paste0("col", seq_along(df))
  depth <- indent_depth(df[[1]])
  df[[1]] <- sub(paste0("^(", NBSP, ")*"), "", df[[1]])
  indent_in <- 0.25 * depth

  layout_at <- function(size) {
    char_in <- CHAR_EM * size / 72
    cells <- if (show_header) rbind(header, as.matrix(df)) else as.matrix(df)
    chars <- nchar(cells)
    natural <- apply(chars, 2, max) * char_in + PAD_IN
    natural[1] <- natural[1] + max(indent_in)
    widths <- natural
    if (sum(natural) > width) {
      narrow <- natural <= width / length(natural)
      widths[!narrow] <- (width - sum(natural[narrow])) * natural[!narrow] / sum(natural[!narrow])
    }
    room <- matrix(widths - PAD_IN, nrow(cells), ncol(cells), byrow = TRUE)
    room[, 1] <- room[, 1] - c(if (show_header) 0, indent_in)
    lines <- pmax(ceiling(chars * char_in / pmax(room, char_in)), 1)  # matrix first, so pmax keeps its dimensions
    row_in <- apply(lines, 1, max) * size * 1.15 / 72 + ROW_PAD
    list(size = size, widths = widths, wraps = sum(natural) > width,
         header_in = if (show_header) row_in[1] else 0, body_in = if (show_header) row_in[-1] else row_in)
  }
  fits <- function(l) l$header_in + sum(l$body_in) <= height
  sizes <- seq(MAX_SIZE, min_size)
  layouts <- lapply(sizes, layout_at)
  pick <- Filter(function(l) fits(l) && !l$wraps, layouts)
  if (!length(pick)) pick <- Filter(fits, layouts)
  fit <- if (length(pick)) pick[[1]] else layouts[[length(layouts)]]
  size <- fit$size
  if (fit$header_in + max(fit$body_in) > height)
    cli::cli_abort("A row of this table does not fit one slide at {size} pt; shorten its text.")

  page <- integer(nrow(df)); used <- fit$header_in; p <- 1L
  for (i in seq_len(nrow(df))) {
    if (used + fit$body_in[i] > height) { p <- p + 1L; used <- fit$header_in }
    page[i] <- p; used <- used + fit$body_in[i]
  }
  widths <- fit$widths

  lapply(split(seq_len(nrow(df)), page), function(rows) {
    ft <- flextable::flextable(df[rows, , drop = FALSE])
    ft <- if (show_header) flextable::set_header_labels(ft, values = stats::setNames(as.list(header), names(df)))
          else flextable::delete_part(ft, part = "header")
    ft <- flextable::font(ft, fontname = "Calibri", part = "all")
    ft <- flextable::fontsize(ft, size = size, part = "all")
    ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 6, padding.right = 6, part = "all")
    if (show_header) {
      ft <- flextable::bg(ft, bg = HEADER, part = "header")
      ft <- flextable::color(ft, color = "white", part = "header")
      ft <- flextable::bold(ft, part = "header")
    }
    ft <- flextable::bg(ft, bg = rep(c(BAND1, BAND2), length.out = length(rows)), part = "body")
    ft <- flextable::align(ft, j = 1, align = "left", part = "all")
    if (ncol(df) > 1L) ft <- flextable::align(ft, j = seq(2L, ncol(df)), align = "center", part = "all")
    for (d in setdiff(unique(depth[rows]), 0L))
      ft <- flextable::padding(ft, i = which(depth[rows] == d), j = 1, padding.left = 6 + 18 * d, part = "body")
    bold <- which(rows %in% c(bold_rows, if (bold_headings && any(depth > 0L)) which(depth == 0L & nzchar(df[[1]]))))
    if (length(bold)) ft <- flextable::bold(ft, i = bold, part = "body")
    white <- officer::fp_border(color = "white", width = 1)
    ft <- flextable::border_remove(ft)
    ft <- flextable::border_inner(ft, border = white, part = "all")
    if (show_header) ft <- flextable::hline(ft, border = white, part = "header")
    flextable::width(ft, width = widths)
  })
}
