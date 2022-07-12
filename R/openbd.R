#' Request to openBD API
#'
#' @param isbn isbn code
#' @examples
#' \dontrun{
#' get_openbd("9784478108536")
#' }
#' @export
get_openbd <- function(isbn) {
  if (length(isbn) == 0) {
    rlang::abort("ISBNを入力してください")
  } else {
    x <-
      httr2::request("https://api.openbd.jp/v1/get") %>%
      httr2::req_url_query(isbn = paste0(isbn, collapse = ","))
    x %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
  }
}
