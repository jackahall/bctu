################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       docx_theme.R                                                 #
#   Description:  report_theme(): a ggplot2-style theme for trial_report,      #
#                 built from named elements that inherit from one another,     #
#                 and the registry that maps each element onto the Word       #
#                 template.                                                    #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

# ---- Templates ---------------------------------------------------------------

# Every report template the package ships, by name. A template is a folder
# of resources (the Word reference document, the pandoc filter) plus the
# registry of themable elements that map [report_theme()] onto it. Adding a
# template is adding a folder and an entry here; a PDF or HTML template
# would carry the same element names with its own `format` and mapping.
REPORT_TEMPLATES <- list(
  bctu = list(
    format    = "docx",
    folder    = "report",
    reference = "reference.docx",
    filter    = "title_page.lua",
    fonts     = c(body = "Arial", heading = "Arial", title = "Times New Roman", code = "Consolas"),
    font_size = 11
  )
)

#' A report template's specification
#'
#' @param name The template name, one of `names(REPORT_TEMPLATES)`.
#' @return The template's list, with `resources` set to its folder and
#'   `elements` to its theme registry.
#' @keywords internal
report_template <- function(name = "bctu") {
  spec <- REPORT_TEMPLATES[[name]]
  if (is.null(spec))
    cli::cli_abort(c("Unknown report template {.val {name}}.", "i" = "Available: {.val {names(REPORT_TEMPLATES)}}."))
  spec$name <- name
  spec$resources <- system.file("rmarkdown", "templates", spec$folder, "resources", package = "bctu")
  spec$elements <- THEME_ELEMENTS
  spec
}

# ---- Theme elements ----------------------------------------------------------

# Every themable element of the template, in dependency order. An element
# has a `default`, or an `inherit` parent (with an optional `modify` of the
# parent's value), and the place it lands in the docx: a theme colour `slot`
# (Word's Design, Colors menu can then change it too), a font `role`, or
# `size`. Adding an element here is all it takes to make it settable.
THEME_ELEMENTS <- list(
  colour.accent         = list(default = "C59A00"),
  rule.colour           = list(inherit = "colour.accent", slot = "accent2"),
  table.border.colour   = list(inherit = "rule.colour", slot = "accent3"),
  table.header.fill     = list(inherit = "colour.accent", modify = function(x) tint(x, 0.8), slot = "accent1"),
  text.secondary.colour = list(default = "3C3C3B", slot = "dk2"),
  link.colour           = list(default = "0057BF", slot = "hlink"),
  font.body             = list(default = "Arial", role = "body"),
  font.heading          = list(inherit = "font.body", role = "heading"),
  font.title            = list(default = "Times New Roman", role = "title"),
  font.code             = list(default = "Consolas", role = "code"),
  font.size             = list(default = 11, size = TRUE)
)

#' Theme a BCTU trial report
#'
#' Builds a theme for [trial_report()] in the style of [ggplot2::theme()]:
#' every argument is one element of the Word template, elements inherit
#' from one another, and anything not given keeps the template default.
#' The same theme can be written in the YAML header under `theme:`, with
#' the element names as keys.
#'
#' @param colour.accent The accent colour every other colour inherits from
#'   (UoB gold, `"C59A00"`).
#' @param rule.colour The title-page and running-header rules and the
#'   report-type line. Inherits `colour.accent`.
#' @param table.border.colour The top and bottom rules of every table.
#'   Inherits `rule.colour`.
#' @param table.header.fill The shading of every table's header row.
#'   Inherits a light tint of `colour.accent`.
#' @param text.secondary.colour Captions, the report subtype and the lower
#'   headings (dark grey).
#' @param link.colour Hyperlinks (UoB blue).
#' @param font.body The body typeface (Arial).
#' @param font.heading The heading typeface. Inherits `font.body`.
#' @param font.title The title-page acronym typeface (Times New Roman).
#' @param font.code The code typeface (Consolas).
#' @param font.size The body size in points (11); every size in the
#'   template scales with it.
#' @return A `report_theme` object: a named list of the elements given.
#' @examples
#' report_theme(colour.accent = "#0057BF")
#' report_theme(rule.colour = "0057BF", table.header.fill = "F5EDD4", font.size = 10)
#' @export
report_theme <- function(colour.accent = NULL, rule.colour = NULL, table.border.colour = NULL,
                         table.header.fill = NULL, text.secondary.colour = NULL, link.colour = NULL,
                         font.body = NULL, font.heading = NULL, font.title = NULL, font.code = NULL,
                         font.size = NULL) {
  given <- Filter(Negate(is.null), as.list(environment()))
  structure(given, class = "report_theme")
}

#' Build a report theme from a named list
#'
#' @param x A named list, such as the parsed YAML `theme` key, a
#'   `report_theme`, or `NULL` for the defaults.
#' @return A `report_theme`.
#' @keywords internal
as_report_theme <- function(x) {
  if (is.null(x)) return(report_theme())
  if (inherits(x, "report_theme")) return(x)
  unknown <- setdiff(names(x), names(THEME_ELEMENTS))
  if (length(unknown))
    cli::cli_abort(c("Unknown theme element{?s}: {.val {unknown}}.",
                     "i" = "The elements are {.val {names(THEME_ELEMENTS)}}."))
  do.call(report_theme, x)
}

#' Resolve a theme to a value for every element
#'
#' Elements not given take their default, or their parent's value, in
#' registry order, so a parent set by the caller flows down to every child
#' that was not.
#'
#' @param theme A `report_theme`.
#' @return A named list with one value per element of `THEME_ELEMENTS`.
#' @keywords internal
resolve_theme <- function(theme) {
  values <- list()
  for (name in names(THEME_ELEMENTS)) {
    spec <- THEME_ELEMENTS[[name]]
    values[[name]] <- if (!is.null(theme[[name]])) theme[[name]]
                      else if (!is.null(spec$inherit)) (spec$modify %||% identity)(values[[spec$inherit]])
                      else spec$default
  }
  colours <- names(Filter(function(s) !is.null(s$slot), THEME_ELEMENTS))
  values[colours] <- lapply(values[colours], normalise_hex)
  if (!is.numeric(values$font.size) || length(values$font.size) != 1L || values$font.size <= 0)
    cli::cli_abort("{.arg font.size} must be one positive number of points.")
  values
}

normalise_hex <- function(x) {
  hex <- toupper(sub("^#", "", as.character(x)))
  if (length(hex) != 1L || !grepl("^[0-9A-F]{6}$", hex))
    cli::cli_abort("Theme colours must be six-digit hex, not {.val {x}}.")
  hex
}

#' Lighten a hex colour towards white
#' @param hex A six-digit hex colour.
#' @param amount The share of the way to white, 0 to 1.
#' @return A six-digit hex colour.
#' @keywords internal
tint <- function(hex, amount) {
  rgb <- grDevices::col2rgb(paste0("#", normalise_hex(hex)))
  toupper(sub("^#", "", grDevices::rgb(t(round(rgb + (255 - rgb) * amount)), maxColorValue = 255)))
}

#' @export
print.report_theme <- function(x, ...) {
  resolved <- resolve_theme(x)
  set <- names(resolved) %in% names(x)
  cat(sprintf("%-24s %s%s\n", names(resolved), vapply(resolved, format, character(1)),
              ifelse(set, "", "  (default)")), sep = "")
  invisible(x)
}
