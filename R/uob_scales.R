################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       uob_scales.R                                                 #
#   Description:  University of Birmingham colour palettes, tints and the      #
#                 ggplot2 colour and fill scales built from them.              #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

# ---- Palettes ----

#' University of Birmingham colour palettes
#'
#' Palettes from the UoB secondary brand palette, the one the brand
#' guidelines designate for data representation. Each hue has a *deep*
#' variant (use with white text) and a *standard* variant (block fills
#' only). Tints at 70, 50, 30 and 15 percent come from [uob_tint()].
#'
#' Palettes: `deep`, `standard`, `categorical` (= `standard`), `cool`,
#' `warm`, `neutral`, `mono_blue`, `mono_green`, `mono_purple`, `mono_red`,
#' `diverging`.
#'
#' @source \url{https://brand.birmingham.ac.uk}
#' @export
uob_palettes <- list(
  deep = c(blue = "#0057BF", orange = "#CF4527", green = "#007838",
           purple = "#501D83", red = "#C20019", turquoise = "#00788E",
           pink = "#DB0661", grey = "#3C3C3B"),
  standard = c(blue = "#2581C4", orange = "#F07F3C", green = "#3AAA35",
               purple = "#5E4F9C", red = "#E30513", turquoise = "#00ACA9",
               pink = "#EA5284", grey = "#58595B"),
  mono_blue   = c("#CCDDF2", "#99BBE5", "#669ADD", "#3378D0", "#0057BF"),
  mono_green  = c("#CCE4D7", "#99C9B0", "#66AD88", "#339261", "#007838"),
  mono_purple = c("#DCD2E6", "#B9A5CD", "#9678B4", "#724B9B", "#501D83"),
  mono_red    = c("#F3CCD1", "#E899A3", "#DC6675", "#D13347", "#C20019"),
  diverging   = c("#C20019", "#F09197", "#FFFFFF", "#A8C7E1", "#0057BF")
)
uob_palettes$categorical <- uob_palettes$standard
uob_palettes$cool        <- uob_palettes$deep[c("blue", "turquoise", "green", "purple")]
uob_palettes$warm        <- uob_palettes$deep[c("red", "orange", "pink")]
uob_palettes$neutral     <- c("#D8D8D8", "#B1B1B1", "#8B8B8C", "#646566", "#3C3C3B")

#' Tint a colour toward white
#'
#' UoB guidelines define tints at 70, 50, 30 and 15 percent. `pct` is the
#' proportion of the source colour retained.
#'
#' @param colour Hex or named colour(s).
#' @param pct Numeric in \[0, 1\].
#' @returns Hex colour vector; when both `colour` and `pct` have length
#'   greater than one, a character matrix with one column per `pct`.
#' @examples
#' uob_tint(uob_palettes$deep[["blue"]], c(0.7, 0.3))
#' @export
uob_tint <- function(colour, pct) {
  if (!is.numeric(pct) || any(pct < 0 | pct > 1))
    cli::cli_abort("{.arg pct} must be numeric in [0, 1].")
  rgb_col <- grDevices::col2rgb(colour) / 255
  vapply(pct, function(p) {
    m <- rgb_col * p + (1 - p)
    grDevices::rgb(m["red", ], m["green", ], m["blue", ])
  }, character(length(colour)))
}

#' UoB palette generator, ggsci style
#'
#' Returns a function that takes the number of colours needed and gives that
#' many UoB colours. Asking for more colours than the palette holds
#' interpolates between them with [grDevices::colorRampPalette()].
#'
#' @param palette Name of a palette in [uob_palettes].
#' @param alpha Opacity in (0, 1].
#' @param reverse Reverse the palette.
#' @returns A function `function(n)` returning `n` hex colours.
#' @examples
#' pal_uob()(3)
#' pal_uob("deep", alpha = 0.7)(3)
#' @export
pal_uob <- function(palette = "categorical", alpha = 1, reverse = FALSE) {
  if (!palette %in% names(uob_palettes))
    cli::cli_abort("Unknown palette {.val {palette}}. Available: {.val {names(uob_palettes)}}.")
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha > 1)
    cli::cli_abort("{.arg alpha} must be a single number in (0, 1].")
  raw <- unname(uob_palettes[[palette]])
  if (reverse) raw <- rev(raw)
  function(n) {
    cols <- if (n <= length(raw)) raw[seq_len(n)] else grDevices::colorRampPalette(raw)(n)
    if (alpha < 1) {
      m <- grDevices::col2rgb(cols)
      cols <- grDevices::rgb(m[1L, ], m[2L, ], m[3L, ],
                             alpha = alpha * 255L, maxColorValue = 255L)
    }
    cols
  }
}

# ---- Scales ----

#' UoB discrete ggplot2 scales
#'
#' @inheritParams pal_uob
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @returns A ggplot2 scale.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg, fill = factor(cyl))) +
#'     ggplot2::geom_boxplot() +
#'     scale_fill_uob("deep")
#' }
#' @export
scale_color_uob <- function(palette = "categorical", alpha = 1, reverse = FALSE, ...) {
  require_ggplot2()
  ggplot2::discrete_scale("colour", palette = pal_uob(palette, alpha, reverse), ...)
}

#' @rdname scale_color_uob
#' @export
scale_colour_uob <- scale_color_uob

#' @rdname scale_color_uob
#' @export
scale_fill_uob <- function(palette = "categorical", alpha = 1, reverse = FALSE, ...) {
  require_ggplot2()
  ggplot2::discrete_scale("fill", palette = pal_uob(palette, alpha, reverse), ...)
}

#' UoB continuous ggplot2 scales
#'
#' @inheritParams pal_uob
#' @param ... Passed to [ggplot2::scale_color_gradientn()] or
#'   [ggplot2::scale_fill_gradientn()].
#' @returns A ggplot2 scale.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = hp)) +
#'     ggplot2::geom_point() +
#'     scale_color_uob_c()
#' }
#' @export
scale_color_uob_c <- function(palette = "mono_blue", alpha = 1, reverse = FALSE, ...) {
  require_ggplot2()
  ggplot2::scale_color_gradientn(colours = pal_uob(palette, alpha, reverse)(256L), ...)
}

#' @rdname scale_color_uob_c
#' @export
scale_colour_uob_c <- scale_color_uob_c

#' @rdname scale_color_uob_c
#' @export
scale_fill_uob_c <- function(palette = "mono_blue", alpha = 1, reverse = FALSE, ...) {
  require_ggplot2()
  ggplot2::scale_fill_gradientn(colours = pal_uob(palette, alpha, reverse)(256L), ...)
}

# ---- Preview ----

#' Preview UoB palettes
#'
#' @param palettes Palettes to show. Default: all.
#' @param n Number of colours per row.
#' @returns Invisible `NULL`.
#' @examples
#' show_uob_palettes(c("deep", "standard"))
#' @export
show_uob_palettes <- function(palettes = names(uob_palettes), n = 10L) {
  bad <- setdiff(palettes, names(uob_palettes))
  if (length(bad)) cli::cli_abort("Unknown palette{?s}: {.val {bad}}.")
  op <- graphics::par(mfrow = c(length(palettes), 1L), mar = c(1, 8, 1, 1))
  on.exit(graphics::par(op), add = TRUE)
  for (p in palettes) {
    graphics::image(seq_len(n), 1, matrix(seq_len(n)), col = pal_uob(p)(n),
                    axes = FALSE, xlab = "", ylab = "")
    graphics::mtext(p, side = 2, las = 1, line = 0.5, cex = 0.9)
  }
  invisible(NULL)
}

#' Stop with an install instruction when ggplot2 is missing
#' @returns Nothing, called for its error.
#' @keywords internal
require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    cli::cli_abort(c("Package {.pkg ggplot2} is needed for the bctu ggplot2 scales and theme.",
                     "i" = "Install it with {.code install.packages(\"ggplot2\")}."))
  invisible(NULL)
}
