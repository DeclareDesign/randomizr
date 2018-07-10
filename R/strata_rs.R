#' Stratified Random Sampling
#'
#' strata_rs implements a random sampling procedure in which units that are grouped into strata defined by covariates are sample using complete random sampling within stratum For example, imagine that 50 of 100 men are sampled and 75 of 200 women are sampled.
#'
#' @param strata A vector of length N that indicates which stratum each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a design in which either floor(N_stratum*prob) or ceiling(N_stratum*prob) units are sampled within each stratum. The probability of  being sampled is exactly prob because with probability 1-prob, floor(N_stratum*prob) units will be sampled and with probability prob, ceiling(N_stratum*prob) units will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param n Use for a design in which the scalar n describes the fixed number of units to sample in each stratum. This number does not vary across strata.
#' @param strata_n Use for a design in which the numeric vector strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of being sampled can vary across strata.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#'
#' @examples
#'
#' strata <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' Z <- strata_rs(strata = strata)
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata, prob = .3)
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata, n = 20)
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata, strata_prob = c(.1, .2, .3))
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
#' table(strata, Z)
#'
#'
strata_rs <- function(strata = NULL,
                      prob = NULL,
                      n = NULL,
                      strata_n = NULL,
                      strata_prob = NULL,
                      check_inputs = TRUE) {
  if (check_inputs) {
    .invoke_check(check_samplr_arguments_new)
  } else{
    N_per_stratum <- tapply(strata, strata, length)
    attributes(N_per_stratum) <- NULL
  }
  
  strata_spots <-
    unlist(split(seq_along(strata), strata), FALSE, FALSE)
  
  # Setup: obtain number of arms and conditions
  
  if (is.null(prob) &&
      is.null(strata_n) && is.null(strata_prob) && is.null(n)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        MoreArgs = list(prob = prob,
                        check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
  
  # Case 2: strata_n is specified
  
  if (!is.null(n)) {
    strata_n <- rep(n, length(N_per_stratum))
  }
  
  if (!is.null(strata_n)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        n = strata_n,
        MoreArgs = list(check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
  
  # Case 3: strata_prob is specified
  if (!is.null(strata_prob)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        prob = strata_prob,
        MoreArgs = list(check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
}

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
strata_rs_probabilities <- function(strata = NULL,
                                    prob = NULL,
                                    n = NULL,
                                    strata_n = NULL,
                                    strata_prob = NULL,
                                    check_inputs = TRUE) {
  if (check_inputs) {
    .invoke_check(check_samplr_arguments_new)
  } else{
    N_per_stratum <- tapply(strata, strata, length)
    attributes(N_per_stratum) <- NULL
  }
  
  
  strata_spots <-
    unlist(split(seq_along(strata), strata), FALSE, FALSE)
  
  if (is.null(prob) &&
      is.null(strata_n) && is.null(strata_prob) && is.null(n)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (is.numeric(prob)) {
    prob_vec <- rep_len(prob, length(strata))
    return(prob_vec)
  }
  
  
  # Case 2: strata_n is specified, or n is
  if (is.numeric(n) && !is.numeric(strata_n)) {
    strata_n <- rep_len(n, length(N_per_stratum))
  }
  
  if (is.numeric(strata_n)) {
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
  if (is.numeric(strata_prob)) {
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
  
  warning("Could not calculate sampling probabilities")
  invisible(NULL)
}
