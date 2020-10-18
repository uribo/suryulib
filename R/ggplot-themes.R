#' @title Theme of data science for dark mode
#' @description aaaa
#' @importFrom ggplot2 %+replace% theme theme_light element_rect element_text element_line

#' @name theme_ds_dark
NULL

#'
#' @description
#' @export
#' @rdname theme_ds_dark
theme_ds_dark <- function() {
  ggplot2::theme_light() %+replace%
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#272728"),
          legend.background = ggplot2::element_rect(fill = "#272728",
                                                    color = NA),
          plot.title = ggplot2::element_text(color = "#FEFEFE"),
          axis.title = ggplot2::element_text(color = "#FEFEFE"),
          axis.text = ggplot2::element_text(color = "#FEFEFE"),
          panel.background = ggplot2::element_rect(fill = "#303031",
                                          colour = NA),
          panel.grid = ggplot2::element_line(color = "#FEFEFE",
                                             linetype = "dotted",
                                             size = 0.2),
          strip.background = ggplot2::element_rect(fill = "#717297"),
          strip.text = ggplot2::element_text(color = "#FEFEFE"),
          legend.title = ggplot2::element_text(color = "#FEFEFE"),
          legend.text = ggplot2::element_text(color = "#FEFEFE"),
          legend.key = ggplot2::element_rect(fill = "#272728",
                                             color = NA),
          legend.box.background = ggplot2::element_rect(fill = "#272728",
                                                        color = NA),
          complete = TRUE)
}
