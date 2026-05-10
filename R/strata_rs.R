#' Stratified Random Sampling
#'
#' strata_rs implements a random sampling procedure in which units that are grouped into strata defined by covariates are sample using complete random sampling within stratum For example, imagine that 50 of 100 men are sampled and 75 of 200 women are sampled.
#'
#' @param strata A vector of length N that indicates which stratum each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a design in which either floor(N_stratum*prob) or ceiling(N_stratum*prob) units are sampled within each stratum. The probability of  being sampled is exactly prob because with probability 1-prob, floor(N_stratum*prob) units will be sampled and with probability prob, ceiling(N_stratum*prob) units will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Must of be of length N. tapply(prob_unit, strata, unique) will be passed to \code{strata_prob}.
#' @param n Use for a design in which the scalar n describes the fixed number of units to sample in each stratum. This number does not vary across strata.
#' @param n_unit Must be of length N. tapply(m_unit, strata, unique) will be passed to \code{strata_n}.
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
#' Z <- strata_rs(strata = strata,
#'                prob_unit = rep(c(.1, .2, .3), times = c(50, 100, 200)))
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
#' table(strata, Z)
#'
#' Z <- strata_rs(strata = strata,
#'                n_unit = rep(c(20, 30, 40), times = c(50, 100, 200)))
#' table(strata, Z)
#'
#'
strata_rs <- function(strata = NULL,
                      prob = NULL,
                      prob_unit = NULL,
                      n = NULL,
                      n_unit = NULL,
                      strata_n = NULL,
                      strata_prob = NULL,
                      check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  block_ra(
    blocks       = strata,
    prob         = prob,
    prob_unit    = prob_unit,
    m            = n,
    m_unit       = n_unit,
    block_m      = strata_n,
    block_prob   = strata_prob,
    conditions   = c(0, 1),
    num_arms     = 2L,
    check_inputs = FALSE
  )
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
                                    prob_unit = NULL,
                                    n = NULL,
                                    n_unit = NULL,
                                    strata_n = NULL,
                                    strata_prob = NULL,
                                    check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  if (!is.null(strata_prob) && !is.numeric(strata_prob)) {
    warning("Could not calculate sampling probabilities")
    return(invisible(NULL))
  }
  prob_mat <- block_ra_probabilities(
    blocks       = strata,
    prob         = prob,
    prob_unit    = prob_unit,
    m            = n,
    m_unit       = n_unit,
    block_m      = strata_n,
    block_prob   = strata_prob,
    conditions   = c(0, 1),
    num_arms     = 2L,
    check_inputs = FALSE
  )
  prob_mat[, "prob_1"]
}
