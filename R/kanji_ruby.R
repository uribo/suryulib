#' Ruby in html
#'
#' @param x word
#' @param view View html output in the viewer
#' @examples
#' kanji_ruby(intToUtf8(c(35576L, 34892L, 28961L, 24120L)))
#' @export
kanji_ruby <- function(x, view = FALSE) {
  if (!requireNamespace("gibasa", quietly = TRUE)) {
    rlang::check_installed("gibasa")
  } else {
    df <-
      gibasa::gbs_tokenize(x) |>
      gibasa::prettify(col_select = "Yomi1")
    res <-
      htmltools::HTML(glue::glue("<ruby>{x}<rt>{df$Yomi1}</rt></ruby>"))
    if (view == TRUE) {
      htmltools::html_print(res)
    } else {
    res
    }
  }
}
