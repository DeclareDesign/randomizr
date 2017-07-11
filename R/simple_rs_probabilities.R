#' Inclusion Probabilities: Simple Random Sampling
#'
#' @inheritParams simple_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#' probs <- simple_ra_probabilities(N = 100)
#' table(probs)
#'
#' probs <- simple_ra_probabilities(N = 100, prob = 0.3)
#' table(probs)
#'
#' @export
simple_rs_probabilities <-
  function(N,
           prob = NULL,
           check_inputs = TRUE) {
    if (check_inputs) {
      input_check <-
        check_samplr_arguments(N = N,
                               prob = prob)
    }
    if (is.null(prob)) {
      prob <- 0.5
    }
    
    prob_vec <- rep(prob, N)
    return(prob_vec)
  }
