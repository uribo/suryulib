#' Get the latest since_id of tweets for a query in a file
#'
#' @param dir A character string indicating the directory path where tweet data is stored.
#' @param query A character string indicating the query used to search for tweets.
#' @return A tibble with two columns, "since_id" and "file".
#' "since_id" column contains the latest since_id for the query,
#' and "file" column contains the name of the file that contains the tweet data.
#' @examples
#' \dontrun{
#' detect_tweet_latest_since_id("data-raw", "rstats")
#' }
#' @export
#' @rdname detect_tweet_latest_since_id-name
detect_tweet_latest_since_id <- function(dir, query) {
  end <- since_id <- NULL
  files <-
    list.files(glue::glue("{dir}/q={query}"),
               full.names = TRUE)
  if (length(files) == 0) {
    tibble::tibble(
      since_id = NA_character_,
      file = NA_character_
    )
  } else {
    tibble::tibble(file = files) |>
      dplyr::mutate(
        end = file |>
          basename() |>
          stringr::str_remove(".rds$") |>
          stringr::str_split("to") |>
          purrr::map(
            \(x) lubridate::as_datetime(as.numeric(x[2]), tz = "Asia/Tokyo")
          ) |>
          purrr::reduce(c)
      ) |>
      dplyr::slice_max(order_by = end, n = 1) |>
      dplyr::mutate(since_id = file |>
                      purrr::map_chr(
                        \(x) readr::read_rds(x) |>
                          rtweet::since_id()
                      )
      ) |>
      dplyr::mutate(file = basename(file)) |>
      dplyr::select(since_id, file)
  }
}

#' Search recent tweets and save to RDS
#'
#' @inheritParams detect_tweet_latest_since_id
#' @param since_id A character string. This argument is used to retrieve only
#' tweets that have been posted after a specific tweet.
#' If `since_id` is `NA`, it is set to `NULL`.
#' @param ... Additional arguments to be passed to the `write_search_tweets_rds` function.
#' @examples
#' \dontrun{
#' write_search_tweets_rds("data-raw", "rstats", since_id = "1621390633146667008")
#' }
#' @export
#' @rdname write_search_tweets_rds
write_search_tweets_rds <- function(dir, query, since_id, ...) {
  if (is.na(since_id)) {
    since_id <- NULL
  }
  res <-
    rtweet::search_tweets(q = query,
                          type = "recent",
                          n = Inf,
                          since_id = since_id,
                          retryonratelimit = TRUE)
  cat(glue::glue("{nrow(res)}\n"))
  if (nrow(res) > 0) {
    res |>
      readr::write_rds(glue::glue("{dir}/q={query}/{ts}.rds",
                                  ts = res$created_at |>
                                    range() |>
                                    as.numeric() |>
                                    stringr::str_c(collapse = "to")))
  }
}
