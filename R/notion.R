#' Convert notion url to app link
#'
#' @param url url (character)
#' @examples
#' \dontrun{
#' convert_notionapp_link("https://www.notion.so/728460a1ba514e6c9a3cfde8a8c371a5")
#' }
#' @export
#' @rdname convert_notionapp_link
convert_notionapp_link <- function(url) {
  if (is_url_notion(url)) {
    res <-
      gsub("https://www.notion.so", "notion://", x = url)
    clipr::write_clip(res)
    cat(cli::col_br_green("app linkをコピーしました\n"))
    res
  } else {
    rlang::abort("URLwを確認してください。\n'https://www.notion.so/'で始まる必要があります")
  }
}

#' @noRd
#' is_url_notion("https://www.notion.so/bc665334afca4de4a04c4c6bfa9243d0")
is_url_notion <- function(url) {
  grepl("https://www.notion.so/[[:alnum:]]",
        url)
}
