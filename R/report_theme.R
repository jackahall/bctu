################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       report_theme.R                                               #
#   Description:  Shared ggplot2 figure theme for BCTU trial reports.          #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Shared ggplot theme for BCTU trial reports
#'
#' The figure theme used across BCTU Word trial reports: `theme_bw` base,
#' white panel and strips, minor grid removed, bold titles, legend below.
#'
#' @param base_size Base font size.
#' @returns A ggplot2 theme object.
#' @seealso [scale_color_uob()], [fig_portrait()], [render_table()].
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_bctu_report()
#' }
#' @export
theme_bctu_report <- function(base_size = 10) {
  require_ggplot2()
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor    = ggplot2::element_blank(),
      strip.background    = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text          = ggplot2::element_text(face = "bold", colour = "#1B1B1B",
                                                  size = ggplot2::rel(0.9)),
      plot.title          = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.95)),
      plot.title.position = "plot",
      axis.title          = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.position     = "bottom",
      legend.key.size     = grid::unit(0.8, "lines"),
      plot.margin         = ggplot2::margin(5, 10, 5, 5)
    )
}
