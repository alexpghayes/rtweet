#' Search all Tweets
#' 
#' Make a call to the [`GET /2/tweets/search/all`](https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all) Twitter API endpoint. This endpoint is only available to those users who have been approved for the Academic Research product track.
#' 
#' The full-archive search endpoint returns the complete history of public Tweets matching a search query; since the first Tweet was created March 26, 2006.
#' 
#' The Tweets returned by this endpoint count towards the Project-level Tweet cap.
#'
#' @param end_time [date (ISO 8601)] YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339). Used with start_time. The newest, most recent UTC timestamp to which the Tweets will be provided. Timestamp is in second granularity and is exclusive (for example, 12:00:01 excludes the first second of the minute). If used without start_time, Tweets from 30 days before end_time will be returned by default. If not specified, end_time will default to [now - 30 seconds].
#' @param expansions [TODO] Comma-separated list of expansions. Expansions enable requests to expand an ID into a full object in the includes response object. Make sure to not include a space between commas and fields.
#' @param max_results [integer] The maximum number of search results to be returned by a request. A number between 10 and the system limit (currently 500). By default, a request response will return 10 results.
#' @param media.fields [TODO] Comma-separated list of additional fields to return in the media object. The response will contain the selected fields only if a Tweet contains media attachments. Make sure to not include a space between commas and fields.
#' @param next_token [string] This parameter is used to get the next 'page' of results. The value used with the parameter is pulled directly from the response provided by the API, and should not be modified. You can learn more by visiting our page on pagination.
#' @param place.fields [TODO] Comma-separated list of additional fields to return in the place object. The response will contain the selected fields only if location data is present in any of the response objects. Make sure to not include a space between commas and fields.
#' @param poll.fields [TODO] Comma-separated list of additional fields to return in the poll object. The response will contain the selected fields only if a Tweet contains a poll. Make sure to not include a space between commas and fields.
#' @param query [string] One query for matching Tweets. Up to 1024 characters.
#' @param since_id [string]	Returns results with a Tweet ID greater than (for example, more recent than) the specified ID. The ID specified is exclusive and responses will not include it. If included with the same request as a start_time parameter, only since_id will be used.
#' @param start_time [date (ISO 8601)]	YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339). The oldest UTC timestamp from which the Tweets will be provided. Timestamp is in second granularity and is inclusive (for example, 12:00:01 includes the first second of the minute). By default, a request will return Tweets from up to 30 days ago if you do not include this parameter.
#' @param tweet.fields [TODO] Comma-separated list of additional fields to return in the Tweet object. By default, the endpoint only returns id and text. Make sure to not include a space between commas and fields.
#' @param until_id [string] Returns results with a Tweet ID less than (that is, older than) the specified ID. Used with since_id. The ID specified is exclusive and responses will not include it.
#' @param user.fields [TODO] Comma-separated list of additional fields to return in the user object. By default, the endpoint does not return any user field. To use this parameter, you must include the author_id expansion parameter in the request. Make sure to not include a space between commas and fields.
#'
#' @return 
#' 
#' - id [Default] 	string	Unique identifier of this Tweet. This is returned as a string in order to avoid complications with languages and tools that cannot handle large integers.
#' - text [Default] 	string	The content of the Tweet. To return this field, add tweet.fields=text in the request's query parameter.
#' - created_at	[date (ISO 8601)]	Creation time of the Tweet. To return this field, add tweet.fields=created_at in the request's query parameter.
#' - `TODO`
#' 
#' @section Supported authentication:
#' 
#' - OAuth 2.0 Bearer token
#' 
#' @section Rate limits:
#' 
#' - 300 requests per 15-minute window (app auth)
#' - 1 request per second (app auth)
#' 
#' @export
#'
#' @examples
v2_tweets_search_all <- function(
  end_time = NULL,
  expansions = NULL,
  max_results = NULL,
  media.fields = NULL,
  next_token = NULL,
  place.fields = NULL,
  poll.fields = NULL,
  query,
  since_id = NULL,
  start_time = NULL,
  tweet.fields = NULL,
  until_id = NULL,
  user.fields = NULL
) {
  
  
  NULL
}


# https://developer.twitter.com/en/docs/pagination
#' @rdname TWIT_paginate_max_id
#'  
#' @param cursor Which page of results to return. The default will return 
#'   the first page; can be used for manual pagination. 
TWIT_paginate_cursor <- function(token, query, params, 
                                 n = 5000, 
                                 page_size = 5000, 
                                 cursor = "-1", 
                                 retryonratelimit = FALSE,
                                 verbose = TRUE) {
  params$count <- page_size
  
  # TODO: consider if its worth using fastmap::faststack() here
  results <- list()
  i <- 1
  n_seen <- 0
  
  repeat({
    params$cursor <- cursor
    json <- catch_rate_limit(
      TWIT_get(
        token, query, params, 
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )
    if (is_rate_limit(json)) {
      warn_early_term(json, 
                      hint = paste0("Set `cursor = '", cursor, "' to continue."),
                      hint_if = !identical(cursor, "-1")
      )
      break
    }
    
    results[[i]] <- json
    cursor <- json$next_cursor_str
    n_seen <- n_seen + length(json$ids)
    i <- i + 1
    
    if (identical(cursor, "0") || n_seen >= n) {
      break
    }
  })
  
  results
}

get_followers <- function(user, n = 5000,
                          cursor = "-1",
                          retryonratelimit = FALSE,
                          parse = TRUE,
                          verbose = TRUE,
                          token = NULL,
                          page = lifecycle::deprecated()) {
  
  if (lifecycle::is_present(page)) {
    lifecycle::deprecate_warn("1.0.0", "get_followers(page)", "get_followers(cursor)")
    cursor <- page
  }
  
  stopifnot(is_n(n), is.atomic(user), isTRUE(length(user) == 1))
  
  if (identical(n, Inf)) {
    usr <- lookup_users(user)
    n <- usr$followers_count
  }
  
  params <- list(stringify_ids = TRUE)
  params[[.id_type(user)]] <- user
  
  results <- TWIT_paginate_cursor(token, "/1.1/followers/ids", params, 
                                  page_size = 5000, 
                                  n = n,
                                  retryonratelimit = retryonratelimit,
                                  cursor = cursor,
                                  verbose = verbose
  )
  
  if (parse) {
    results <- lapply(results, parse.piper.fs, n = n)
    results <- do.call("rbind", results)
  }
  
  results
}
