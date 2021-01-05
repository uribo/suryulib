#' Line Notification
#' @param token Personal access token
#' @param message text
#' @param img_file path to upload image file
#' @param .disabled option
#' @export
line_notify <- function(token, message, img_file = NULL, .disabled = FALSE) {
  if (is.null(img_file) == FALSE) {
    if (file.exists(img_file) == TRUE) {
      img_file <- httr::upload_file(img_file)
    }
  }
  url <- "https://notify-api.line.me/api/notify"
  body <-
    list(message = message,
         imageFile = img_file,
         notificationDisabled = .disabled)
  res <-
    httr::POST(url,
             httr::add_headers(Authorization = paste("Bearer ", token, sep = "")),
             body = body,
             encode = "multipart",
             content_type = "application/x-www-form-urlencoded")
  if (res$status_code == 200L) {
    cli::col_cyan(glue::glue("Send: {message}"))
  } else {
    res$status_code
  }
}
