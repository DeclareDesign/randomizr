#' Inclusion Probabilities: Stratified Random Sampling
#'
#' @inheritParams strata_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' strata <- rep(c("A", "B","C"), times = c(50, 100, 200))

#' probs <- strata_rs_probabilities(strata = strata)
#' table(strata, probs)
#'
#' probs <- strata_rs_probabilities(strata = strata, prob = .2)
#' table(strata, probs)
#'
#' probs <- strata_rs_probabilities(strata = strata, strata_prob = c(.1, .2, .3))
#' table(strata, probs)
#'
#' probs <- strata_rs_probabilities(strata = strata, strata_n = c(10, 40, 70))
#' table(strata, probs)
#'
#' @export
strata_rs_probabilities <- function(strata = strata_var,
                                    prob = NULL,
                                    n = NULL,
                                    strata_n = NULL,
                                    strata_prob = NULL,
                                    check_inputs = TRUE,
                                    strata_var = NULL) {
  warn_deprecated_args(strata_var=strata_var)
  if (check_inputs) {
    input_check <- check_samplr_arguments(
      strata = strata,
      prob = prob,
      n = n,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    N_per_stratum <- input_check$N_per_stratum
  } else{
    N_per_stratum <- tapply(strata, strata, length)
    attributes(N_per_stratum) <- NULL
  }
  
  
  strata_spots <-
    unlist(split(1:length(strata), strata), FALSE, FALSE)
  
  if (is.null(prob) & is.null(strata_n) & is.null(strata_prob) & is.null(n)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    prob_vec <- rep(prob, length(strata))
    return(prob_vec)
  }
  
  prob_vec <- rep(NA, length(strata))
  
  # Case 2: strata_n is specified
  if(!is.null(n)){
    strata_n <- rep(n, length(N_per_stratum))
  }
  
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
