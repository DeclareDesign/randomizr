#' Inclusion Probabilities: Complete Random Sampling
#'
#' @inheritParams complete_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#' probs <- complete_rs_probabilities(N = 100)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N = 100, n = 50)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N=100, prob = .3)
#' table(probs)
#'
#' @export
complete_rs_probabilities <- function(N,
                                      n = NULL,
                                      prob = NULL,
                                      check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  if(is.null(n) && is.null(prob)) {
    prob_vec <- .5
  } else if (is.numeric(n)) {
    
    if (N == 1) {
      
      # we "know" below should be .5 becasue prob 
      prob_vec <- if(n == 0) 0 else if(n == 1) .5 else 
        stop("The number of units sampled (n) must be less than or equal to the total number of units (N)")
      
    } else {
      
      prob_vec <- n / N
    }
    
    
  } else if (is.numeric(prob)) {
  
    prob_vec <- if(N == 1) prob else {
        n_floor <- floor(N * prob)
        n_ceiling <- ceiling(N * prob)
        ifelse(n_ceiling == N,  n_floor / N, prob)
    }
    
  }

  rep_len(prob_vec, N)
}
