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
#' probs <- strata_rs_probabilities(strata_var = strata_var, strata_n = c(10, 40, 70))
#' table(strata_var, probs)
#'
#' @export
strata_rs_probabilities <- function(strata_var,
                                    prob = NULL,
                                    strata_n = NULL,
                                    strata_prob = NULL,
                                    check_inputs = TRUE) {
  if (check_inputs) {
    check_inputs <- check_samplr_arguments(
      strata_var = strata_var,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
    N_per_stratum <- check_inputs$N_per_stratum
  }
  
  
  strata_spots <-
    unlist(split(1:length(strata_var), strata_var), FALSE, FALSE)
  
  if (is.null(prob) & is.null(strata_n) & is.null(strata_prob)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    prob_vec <- rep(prob, length(strata_var))
    return(prob_vec)
  }
  
  strata <- sort(unique(strata_var))
  prob_vec <- rep(NA, length(strata_var))
  
  # Case 2: strata_n is specified
  if (!is.null(strata_n)) {
    prob_vec_list <-
      mapply(
        FUN = complete_rs_probabilities,
        N = N_per_stratum,
        n = strata_n,
        MoreArgs = list(prob = prob,
                        check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    prob_vec <-
      unlist(prob_vec_list, FALSE, FALSE)[order(strata_spots)]
    return(prob_vec)
  }
  
  # Case 3: strata_prob is specified
  if (!is.null(strata_prob)) {
    prob_vec_list <-
      mapply(
        FUN = complete_rs_probabilities,
        N = N_per_stratum,
        prob = strata_prob,
        MoreArgs = list(check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    prob_vec <-
      unlist(prob_vec_list, FALSE, FALSE)[order(strata_spots)]
    return(prob_vec)
  }
}
