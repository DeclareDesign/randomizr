#' Inclusion Probabilities: Complete Random Sampling
#'
#' @inheritParams complete_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#' probs <- complete_rs_probabilities(N = 100)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N = 100, m = 50)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N=100, prob = .3)
#' table(probs)
#'
#' @export
complete_rs_probabilities <- function(N,
                                      m = NULL,
                                      prob = NULL) {
  check_inputs <-
    check_samplr_arguments(N = N,
                           m = m,
                           prob = prob)
  
  if (N == 1) {
    if (is.null(m) & is.null(prob)) {
      prob_vec <- rep(.5, N)
      return(prob_vec)
    }
    if (!is.null(m)) {
      if (!m %in% c(0, 1)) {
        stop(
          "The number of units sampled (m) must be less than or equal to the total number of units (N)"
        )
      }
      if (m == 0) {
        prob_vec <- rep(0, N)
        return(prob_vec)
      }
      if (m == 1) {
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
    if (is.null(m) & is.null(prob)) {
      prob_vec <- rep(.5, N)
      return(prob_vec)
    }
    if (!is.null(m)) {
      prob <- m / N
      prob_vec <- rep(prob, N)
      return(prob_vec)
    }
    if (!is.null(prob)) {
      m_floor <- floor(N * prob)
      m_ceiling <- ceiling(N * prob)
      if (m_ceiling == N) {
        m <- m_floor
        prob_vec <- rep(m / N, N)
        return(prob_vec)
      }
      
      prob_vec <- rep(prob, N)
      return(prob_vec)
    }
  }
}
