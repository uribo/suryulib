#' Japanese font
#' @export
#' @rdname jpfont
jpfont <- function() {
  dplyr::if_else(grepl("mac", sessioninfo::os_name()),
                 "IPAexGothic",
                 "IPAGothic")
}
