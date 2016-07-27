#' Inclusion Probabilities: Stratified Random Sampling
#'
#' @inheritParams strata_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' strata_var <- rep(c("A", "B","C"), times = c(50, 100, 200))

#' probs <- strata_rs_probabilities(strata_var = strata_var)
#' table(strata_var, probs)
#'
#' probs <- strata_rs_probabilities(strata_var = strata_var, prob = .2)
#' table(strata_var, probs)
#'
#' probs <- strata_rs_probabilities(strata_var = strata_var, strata_prob = c(.1, .2, .3))
#' table(strata_var, probs)
#'
#' probs <- strata_rs_probabilities(strata_var = strata_var, strata_m = c(10, 40, 70))
#' table(strata_var, probs)
#'
#' @export
strata_rs_probabilities <- function(strata_var,
                                    prob = NULL,
                                    strata_m = NULL,
                                    strata_prob = NULL,
                                    balance_load = FALSE) {
  #if(all(balance_load & is.null(prob) & is.null(prob_each) & is.null(strata_prob_each))){
  #  stop("If you use the experimental feature 'balance_load', then you must specify one of prob, prob_each, or strata_prob_each.")
  #}
  
  check_inputs <- check_samplr_arguments(
    strata_var = strata_var,
    prob = prob,
    strata_m = strata_m,
    strata_prob = strata_prob
  )
  
  N_per_stratum <- check_inputs$N_per_stratum
  
  if (is.null(prob) & is.null(strata_m) & is.null(strata_prob)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    prob_vec <- rep(prob, length(strata_var))
    return(prob_vec)
  }
  
  strata <- sort(unique(strata_var))
  prob_vec <- rep(NA, length(strata_var))
  
  # Case 2: strata_m is specified
  if (!is.null(strata_m)) {
    for (i in 1:length(strata)) {
      prob_vec[strata_var == strata[i]] <-
        complete_rs_probabilities(N = N_per_stratum[i], m = strata_m[i])
    }
    return(prob_vec)
  }
  
  # Case 3: strata_prob is specified
  if (!is.null(strata_prob)) {
    for (i in 1:length(strata)) {
      prob_vec[strata_var == strata[i]] <-
        complete_rs_probabilities(N = N_per_stratum[i], prob = strata_prob[i])
    }
    return(prob_vec)
  }
}
