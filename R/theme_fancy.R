#' Adding a color to the background.
#'
#' @import ggplot2
#'
#' @param fill_color, background color
#'
#' @export
theme_fancy <- function(
    fill_color = "#ededed"
) {
    theme(plot.background = element_rect(fill = fill_color, color = NA),
          panel.background = element_rect(fill = fill_color, color = NA),
          legend.background = element_rect(fill = fill_color, color = NA))
}
