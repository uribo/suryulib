#' Color scale for data science plot
#'
#' @param pallet main
#' @param discrete discrete variables
#' @param reverse color order
#' @param na.value na color
#' @param ... path to other functions
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @export
#' @rdname scale_color_ds
scale_color_ds <- function(palette = "main", discrete = TRUE, reverse = FALSE, na.value = "gray60", ...) {
  pal <- ds_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour",
                            paste0("dsview_", palette),
                            palette = pal,
                            na.value = na.value,
                            ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256),
                                   na.value = na.value,
                                   ...)
  }
}

#' @inheritParams scale_color_ds
#' @export
#' @rdname scale_color_ds
scale_fill_ds <- function(palette = "main", discrete = TRUE, reverse = FALSE, na.value = "gray60", ...) {
  pal <- ds_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill",
                   paste0("dsview_", palette),
                   palette = pal,
                   na.value = na.value,
                   ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256),
                         na.value = na.value,
                         ...)
  }
}
