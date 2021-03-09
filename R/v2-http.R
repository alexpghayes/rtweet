# after reading https://developer.twitter.com/en/docs/twitter-api/pagination
# i'm under the impression that TWIT_paginate_max_id and TWIT_paginate_cursor
# are not appropriate for v2 endpoints

# things that i want to make sure i'm doing "correctly" / consistently with 
# the rest of rtweet
#
#   - auth / tokens
#   - pagination per hadley's redesign
#   - handling rate limits


#' Title
#'
#' @param token 
#' @param query 
#' @param params 
#' @param n 
#' @param page_size 
#' @param cursor 
#' @param retryonratelimit 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
TWIT_paginate_token <- function(token, query, params, 
                                max_results = 500,
                                pagination_token = NULL,
                                retryonratelimit = FALSE,
                                verbose = TRUE) {
  params$count <- page_size
  
  # TODO: consider if its worth using fastmap::faststack() here
  results <- list()
  i <- 1
  n_seen <- 0
  
  repeat({
    params$pagination_token <- pagination_token
    json <- catch_rate_limit(
      TWIT_get(
        token, query, params, 
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )
    )
    if (is_rate_limit(json)) {
      warn_early_term(json, 
                      hint = paste0("Set `pagination_token` = '", TODO, "' to continue."),
                      hint_if = !is.null(pagination_token)
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
