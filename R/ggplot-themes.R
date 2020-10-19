#' Theme of data science for dark mode
#' @param base_size base font size
#' @param ... Other arguments passed to [theme_light][ggplot2::theme_light]
#' @rdname theme_ds_dark
#' @export
theme_ds_dark <- function(base_size = 11, ...) {
  ret <-
    ggplot2::theme_light(base_size = base_size, ...)
  ret$plot.background <-
    ggplot2::element_rect(fill = "#272728")
  ret$legend.background <-
    ggplot2::element_rect(fill = "#272728",
                          color = NA)
  ret$plot.title <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$axis.title <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$axis.text <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$panel.background <-
    ggplot2::element_rect(fill = "#303031",
                          colour = NA)
  ret$panel.grid <-
    ggplot2::element_line(color = "#FEFEFE",
                          linetype = "dotted",
                          size = 0.2)
  ret$strip.background <-
    ggplot2::element_rect(fill = "#717297")
  ret$strip.text <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$legend.title <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$legend.text <-
    ggplot2::element_text(color = "#FEFEFE")
  ret$legend.key <-
    ggplot2::element_rect(fill = "#272728",
                          color = NA)
  ret$legend.box.background <-
    ggplot2::element_rect(fill = "#272728",
                          color = NA)
  ret
}
