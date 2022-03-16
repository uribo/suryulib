insertNA <- function(x, y) {
  x <-
    names(x)
  y[c(which(x %in% x[!x %in% names(y)]))] <- NA
  rlang::set_names(y, x)
}

exec_stats <- function(data, .fun, na.rm = TRUE, .trim = NULL) {
  fun <-
    rlang::arg_match(.fun,
                     c("n", "unique_n", "missing_n",
                       "mean", "sd", "median", "trimmed",
                       "mad",
                       "min", "max", "diff",
                       "skewness", "kurtosis",
                       "se"))
  if (fun %in% c("skewness", "kurtosis")) {
    if (!requireNamespace("moments", quietly = TRUE)) {
      rlang::check_installed("moments2")
    } else {
      loc <- if ("moments" %in% loadedNamespaces()) dirname(getNamespaceInfo("moments", "path"))
      do.call(
        "library",
        list("moments", lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
      )
    }
  }
  if (fun == "trimmed") {
    fun <- "mean"
    if (is.null(.trim))
      .trim <- 0.1
  } else {
    .trim <- NULL
  }
  args <-
    list(na.rm = na.rm,
         trim = .trim) %>%
    purrr::discard(is.null)
  if (fun %in% c("n", "unique_n", "missing_n")) {
    if (fun == "n") {
      res <-
        data %>%
        purrr::map_dbl(
          ~ length(.x)) %>%
        unname()
    }
    if (fun == "unique_n") {
      res <-
        data %>%
        purrr::map_dbl(
          ~ length(unique(.x))) %>%
        unname()
    }
    if (fun == "missing_n") {
      res <-
        data %>%
        purrr::map_dbl(
          ~ sum(is.na(.x))) %>%
        unname()
    }
    res <-
      rlang::set_names(res,
                       names(data))
  } else {
    x <-
      data %>%
      purrr::keep(is.numeric)
    if (fun == "diff") {
      res <-
        x %>%
        purrr::map(
          ~ range(.x, na.rm = na.rm)) %>%
        purrr::map_dbl(diff)
    } else if (fun == "mad") {
      res <-
        x %>%
        purrr::map_dbl(
          ~ stats::median(abs(.x - stats::median(.x, na.rm = na.rm))))
    } else if (fun == "se") {
      res <-
        x %>%
        purrr::map_dbl(
          ~ sd(.x) / sqrt(length(.x)))
    } else {
      res <-
        x %>%
        purrr::map_dbl(
          ~ rlang::exec(fun, .x, !!!args))
    }
    res <-
      insertNA(data, res)
  }
  return(res)
}

#' Tidy summary for data.frame
#'
#' @description Alternative broom::tidy() for data.frame.
#' @param data data.frame
#' @param .trim Option.
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @export
summary_df <- function(data, .trim = NULL, na.rm = TRUE) {
  x <- colnames(data)
  stats <-
    c("n", "unique_n", "missing_n",
      "mean", "sd", "median", "trimmed",
      "mad",
      "min", "max", "diff",
      "skewness", "kurtosis",
      "se")
  x1 <-
    tibble::tibble(
    column = x,
    class = data %>%
      purrr::map_chr(
        ~ class(.x)) %>%
      unname())
  x2 <-
    stats %>%
    rlang::set_names(stats) %>%
    purrr::map_dfc(~ exec_stats(data, .x, na.rm = na.rm, .trim = .trim))
  tibble::as_tibble(cbind(x1, x2))
}
