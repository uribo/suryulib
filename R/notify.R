line_notify <- function(token, message, img_file = NULL, .disabled = FALSE) {
  if (is.null(img_file) == FALSE) {
    if (file.exists(img_file) == TRUE) {
      img_file <- httr::upload_file(img_file)
    }
  }
  body <-
    list(message = message,
         imageFile = img_file,
         notificationDisabled = .disabled)
  httr::POST(url,
             httr::add_headers(Authorization = paste("Bearer ", token, sep = "")),
             body = body,
             encode = "multipart",
             content_type = "application/x-www-form-urlencoded")
}
