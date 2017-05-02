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
  if (check_inputs) {
    check_inputs <-
      check_samplr_arguments(N = N,
                             n = n,
                             prob = prob)
    
  }
  
  if (N == 1) {
    if (is.null(n) & is.null(prob)) {
      prob_vec <- rep(.5, N)
      return(prob_vec)
    }
    if (!is.null(n)) {
      if (!n %in% c(0, 1)) {
        stop(
          "The number of units sampled (n) must be less than or equal to the total number of units (N)"
        )
      }
      if (n == 0) {
        prob_vec <- rep(0, N)
        return(prob_vec)
      }
      if (n == 1) {
        prob_vec <- rep(.5, N)
        return(prob_vec)
      }
    }
    if (!is.null(prob)) {
      prob_vec <- rep(prob, N)
      return(prob_vec)
    }
  }
  
  if (N > 1) {
    if (is.null(n) & is.null(prob)) {
      prob_vec <- rep(.5, N)
      return(prob_vec)
    }
    if (!is.null(n)) {
      prob <- n / N
      prob_vec <- rep(prob, N)
      return(prob_vec)
    }
    if (!is.null(prob)) {
      n_floor <- floor(N * prob)
      n_ceiling <- ceiling(N * prob)
      if (n_ceiling == N) {
        n <- n_floor
        prob_vec <- rep(n / N, N)
        return(prob_vec)
      }
      
      prob_vec <- rep(prob, N)
      return(prob_vec)
    }
  }
}
