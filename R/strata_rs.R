#' Stratified Random Sampling
#'
#' \code{strata_rs} draws a sample separately within each of several groups (strata) defined by covariates, using complete random sampling inside every stratum. For example, 50 of 100 men and 75 of 200 women might be sampled. Stratifying guarantees how much of the sample comes from each group, which keeps small groups from being underrepresented by chance.
#'
#' The number sampled per stratum can be left to the function, set as a common count or probability across strata (\code{n}, \code{prob}), or set stratum by stratum (\code{strata_n}, \code{strata_prob}). When the probability varies across strata the sample is not self-weighting, and \code{\link{strata_rs_probabilities}()} gives the inclusion probabilities needed to weight it.
#'
#' @section Terminology:
#' The name follows \code{\link{block_ra}()}, as the \code{_rs} functions follow their random assignment counterparts. In the survey sampling literature this design is stratified simple random sampling without replacement (stratified SRSWOR; Lohr 2022, \emph{Sampling: Design and Analysis}, 3rd ed.; Särndal, Swensson, and Wretman 1992, \emph{Model Assisted Survey Sampling}). Strata are sampled independently. When \code{N_stratum*prob} is not a whole number, a stratum's sample size is a mixture of the floor and the ceiling, as in \code{\link{complete_rs}()}.
#'
#' @seealso \code{\link{complete_rs}()}, \code{\link{strata_and_cluster_rs}()}, \code{\link{block_ra}()}, \code{\link{strata_rs_probabilities}()}
#'
#' @param strata A vector of length N indicating which stratum each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a design in which either \code{floor(N_stratum*prob)} or \code{ceiling(N_stratum*prob)} units are sampled within each stratum. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_stratum*prob} and the floor otherwise, which makes each unit's probability of inclusion exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Must be of length N. \code{tapply(prob_unit, strata, unique)} will be passed to \code{strata_prob}, so it must be constant within each stratum. (optional)
#' @param n Use for a design in which the scalar \code{n} gives the fixed number of units to sample in every stratum. This count does not vary across strata. (optional)
#' @param n_unit Must be of length N. \code{tapply(n_unit, strata, unique)} will be passed to \code{strata_n}, so it must be constant within each stratum. (optional)
#' @param strata_n Use for a design in which the numeric vector \code{strata_n} gives the number of units to sample within each stratum. Must be as long as the number of strata, in the same order as \code{sort(unique(strata))}. (optional)
#' @param strata_prob Use for a design in which \code{strata_prob} gives the probability of being sampled within each stratum. Must be in the same order as \code{sort(unique(strata))}. Differs from \code{prob} in that the probability of being sampled can vary across strata. (optional)
#' @param check_inputs Logical. Whether to verify before sampling that the arguments are internally consistent: that counts do not exceed the stratum sizes, that probabilities lie between 0 and 1, that stratum-level arguments have one entry per stratum, and so on. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many samples from arguments that have already been verified; declaring the design once with \code{\link{declare_rs}()} and drawing from it with \code{\link{draw_rs}()} does this for you. (optional)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0).
#' @export
#'
#' @examples
#'
#' strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#'
#' S <- strata_rs(strata = strata)
#' table(strata, S)
#'
#' # The same probability in every stratum
#' S <- strata_rs(strata = strata, prob = 0.3)
#' table(strata, S)
#'
#' # The same count in every stratum
#' S <- strata_rs(strata = strata, n = 20)
#' table(strata, S)
#'
#' # A different probability in each stratum, in the order of sort(unique(strata))
#' S <- strata_rs(strata = strata, strata_prob = c(0.1, 0.2, 0.3))
#' table(strata, S)
#'
#' # The same, specified unit by unit
#' S <- strata_rs(strata = strata,
#'                prob_unit = rep(c(0.1, 0.2, 0.3), times = c(50, 100, 200)))
#' table(strata, S)
#'
#' # A different count in each stratum
#' S <- strata_rs(strata = strata, strata_n = c(20, 30, 40))
#' table(strata, S)
#'
#' S <- strata_rs(strata = strata,
#'                n_unit = rep(c(20, 30, 40), times = c(50, 100, 200)))
#' table(strata, S)
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
  S <- block_ra(
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
  if (.strata_rs_uses_block_prob(n, n_unit, strata_n, prob_unit, strata_prob)) {
    r <- .strata_rs_rounded_down(strata, prob_unit, strata_prob)
    for (s in which(r$rounded_down)) {
      if (runif(1L) < r$frac[s]) S[r$stratum == s] <- 1
    }
  }
  S
}

# block_ra() never rounds a block's count up to the whole block, which keeps
# every block's control arm nonempty. Sampling has no second arm to protect,
# so a stratum whose count block_ra() rounds down to N_s - 1 is instead taken
# whole with probability equal to the fractional part of N_s * prob. Mixing
# the whole stratum with block_ra()'s N_s - 1 draw makes inclusion exactly
# prob, and the extra draw comes after block_ra()'s, so seeds for designs
# that never hit the case are unchanged. block_ra() reaches the rule only
# through block_prob, which prob_unit and strata_prob become; a scalar prob
# takes another branch that already rounds up.
.strata_rs_uses_block_prob <- function(n, n_unit, strata_n, prob_unit, strata_prob) {
  is.null(n) && is.null(n_unit) && is.null(strata_n) &&
    (!is.null(prob_unit) || is.numeric(strata_prob))
}

.strata_rs_rounded_down <- function(strata, prob_unit, strata_prob) {
  strata <- droplevels(as.factor(strata))
  prob <- if (!is.null(prob_unit)) as.numeric(tapply(prob_unit, strata, unique)) else strata_prob
  N_s <- tabulate(as.integer(strata))
  Np <- N_s * prob
  list(
    stratum = as.integer(strata),
    prob = prob,
    frac = Np - floor(Np),
    rounded_down = N_s > 1 & ceiling(Np) == N_s & floor(Np) < Np
  )
}

#' Inclusion probabilities: Stratified Random Sampling
#'
#' Returns each unit's probability of being sampled under stratified random
#' sampling. Units in different strata routinely have different probabilities, and a
#' sample drawn that way is not self-weighting.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each sampled unit by the reciprocal of its inclusion probability, which
#' \code{\link{obtain_inclusion_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{strata_rs}()}
#'
#' @inheritParams strata_rs
#' @return A numeric vector of length N giving each unit's probability of being included in the sample.
#'
#' @examples
#'
#' strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#'
#' probs <- strata_rs_probabilities(strata = strata)
#' table(strata, probs)
#'
#' probs <- strata_rs_probabilities(strata = strata, prob = 0.2)
#' table(strata, probs)
#'
#' probs <- strata_rs_probabilities(strata = strata, strata_prob = c(0.1, 0.2, 0.3))
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
  probs <- prob_mat[, "prob_1"]
  if (.strata_rs_uses_block_prob(n, n_unit, strata_n, prob_unit, strata_prob)) {
    r <- .strata_rs_rounded_down(strata, prob_unit, strata_prob)
    hit <- r$rounded_down[r$stratum]
    probs[hit] <- r$prob[r$stratum][hit]
  }
  probs
}
