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
      gibasa::prettify(gibasa::gbs_tokenize(x),
                       col_select = "Yomi1")
    res <-
      htmltools::HTML(glue::glue("<ruby>{x}<rt>{df$Yomi1}</rt></ruby>"))
    if (view == TRUE) {
      htmltools::html_print(res)
    } else {
    res
    }
  }
}

#' html showing the reading of the word
#'
#' @inheritParams kanji_ruby
#' @param yomi character
#' @param style style for yomi text
#' @rdname html_yomi
#' @examples
#' html_yomi("CRAN", "The Comprehensive R Archive Network", "parenthesis")
#' @export
html_yomi <- function(x, yomi, style = "space") {
  rlang::arg_match(style,
                   c("space", "parenthesis"))
  if (style == "space") {
    glue::glue("<span>{x}</span> <span>{yomi}</span>")
  } else if (style == "parenthesis") {
    glue::glue("<span>{x}</span> <span>({yomi})</span>")
  }
}
