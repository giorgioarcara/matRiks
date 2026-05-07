#' Response list (Method)
#'
#' Generate the response list from a matriks (correct response and distractors)
#'
#' @inheritParams difference
#'
#'
#' @return An object of class responses of length 11, containing the correct response + 10 distractors (3 repetition, 1 difference, 2 wrong principles, 4 incomplete correlate)
#' @export response_list
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty", vrules = "size")
#' # draw the matrix
#' draw(m1)
#' # draw the responses
#' draw(response_list(m1))
#'
#' # change the difference distractor by changing the random seed
#' draw(response_list(m1, seed = 8))
response_list <-function(obj, seed, ...) {
  UseMethod("response_list")
}

#' @describeIn response_list Response list
#'
#' Generate the response list from a matriks (correct response and distractors)
#'
#' @inheritParams response_list
#'
#'
#' @return An object of class responses of length 11, containing the correct response + 10 distractors (3 repetition, 1 difference, 2 wrong principles, 4 incomplete correlate)
#' @export response_list.matriks
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty", vrules = "size")
#' # draw the matrix
#' draw(m1)
#' # draw the responses
#' draw(response_list(m1))
#'
#' # change the difference distractors by changing the random seed
#' draw(response_list(m1, seed = 8))
response_list.matriks <- function(obj, seed = 666, ...) {
  seed <- seed
  my_repetion <- repetition(obj)
  my_wp <- wp(obj)
  resp <- list(correct = correct(obj),
               r_diag = my_repetion$r_diag,
               r_left = my_repetion$r_left,
               r_top = my_repetion$r_top,
               wp_copy = my_wp$wp_copy,
               wp_matrix = my_wp$wp_matrix,
               difference = difference(obj, seed = seed),
               ic_neg = ic_neg(obj),
               ic_flip = ic_flip(obj),
               ic_size = ic_size(obj),
               ic_inc = ic_inc(obj) )
  class(resp) <- "responses"
  return(resp)
}

#' Sample a random subset of responses (including correct one)
#'
#' @param responses A responses object (named list)
#' @param n_resp integer, number of responses to sample. Default is 4.
#' @param ... other arguments
#'
#' @return A responses object of length n_resp, always including the correct
#'   response (index 1), with order shuffled.
#' @export
sample_resp <- function(responses, n_resp = 4, ...) {
  UseMethod("sample_resp")
}

#' @describeIn sample_resp Sample responses
#' @export
sample_resp.responses <- function(responses, n_resp = 4, ...) {
  stopifnot("length of responses must be >= n_resp" = length(responses) >= n_resp)
  
  # always include correct response (index 1), sample the rest
  idx <- c(1, sample(2:length(responses), n_resp - 1))
  # shuffle
  idx_shuffled <- idx[sample(n_resp)]
  
  resp_shuffled <- responses[idx_shuffled]
  class(resp_shuffled) <- "responses"
  
  return(resp_shuffled)
}
