#' Completely blank theme for ggplot2, with Avenir default fonts.
#'
#' Particularly useful for pie charts.
#'
#' @import ggplot2
#'
#' @param base_family,base_size base family and size
#' @param plot_title_size,plot_title_face,plot_title_margin title size + style
#' @param legend_size legend size
#' @param strip_text_size strip text size
#'
#' @export
theme_blank <- function(
    base_family = "Avenir", base_size = 10,
    plot_title_size = 16,
    plot_title_face="bold", plot_title_margin = 10,
    legend_size = 12,
    strip_text_size = 12
    ) {

    theme_minimal(base_family = base_family, base_size = base_size)+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=plot_title_size),
        legend.title = element_text(size=legend_size),
        strip.text = element_text(size = strip_text_size)
    )
}
