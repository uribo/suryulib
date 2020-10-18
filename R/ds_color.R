#' @importFrom rlang is_null
ds_col <- function(...) {
  .ds_cols <-
    c(`cave`   = "#F4A935",
      `crater` = "#E94E77",
      `forest` = "#7BA085",
      `grain`  = "#E1D5A3",
      `river`  = "#4886a5")
  cols <- c(...)
  if (rlang::is_null(cols))
    return(.ds_cols)
  .ds_cols[cols]
}

#' @importFrom grDevices colorRampPalette
ds_pal <- function(palette = "main", reverse = FALSE, ...) {
  ds_palettes <- list(`main`  = ds_col())
  pal <- ds_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}
