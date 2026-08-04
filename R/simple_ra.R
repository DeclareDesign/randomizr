#' Simple Random Assignment
#'
#' \code{simple_ra} assigns units to treatment conditions independently, with each unit's assignment drawn as a separate Bernoulli trial. Because units are assigned independently, the number of units assigned to each condition varies from draw to draw. For most experimental applications in which the number of units is known in advance, \code{\link{complete_ra}} is preferable because it fixes the counts in each condition and thereby reduces sampling variability.
#'
#' Simple random assignment is appropriate when units arrive sequentially and the total sample size is not known in advance, or when the assignment must proceed without coordinating across units. If only \code{N} is specified, a two-arm trial with \code{prob = 0.5} is assumed.
#'
#' @seealso \code{\link{complete_ra}}, \code{\link{block_ra}}, \code{\link{simple_rs}}, \code{\link{simple_ra_probabilities}}
#'
#' @param N The number of units. Must be a positive integer. (required)
#' @param prob Use for a two-arm design. The probability of assignment to treatment; must be a real number between 0 and 1 and of length 1. (optional)
#' @param prob_unit Use for a two-arm design. The probability of assignment to treatment for each unit; must be a real number between 0 and 1 and of length N. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector or N-by-conditions matrix giving the probability of assignment to each condition; entries must be nonnegative and sum to 1. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named 0 and 1 in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param check_inputs Logical. Defaults to \code{TRUE}.
#' @param simple Logical. Internal use only.
#'
#' @return A vector of length N indicating the treatment condition of each unit. Numeric in a two-arm trial; a factor (ordered by \code{conditions}) in a multi-arm trial.
#' @export
#'
#' @examples
#' # Two Group Designs
#'
#' Z <- simple_ra(N=100)
#' table(Z)
#'
#' Z <- simple_ra(N=100, prob=0.5)
#' table(Z)
#'
#' Z <- simple_ra(N=100, prob_each = c(0.3, 0.7),
#'                conditions = c("control", "treatment"))
#' table(Z)
#'
#' # Multi-arm Designs
#' Z <- simple_ra(N=100, num_arms=3)
#' table(Z)
#'
#' Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4))
#' table(Z)
#'
#' Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4),
#'                conditions=c("control", "placebo", "treatment"))
#' table(Z)
#'
#' Z <- simple_ra(N=100, conditions=c("control", "placebo", "treatment"))
#' table(Z)
simple_ra <- function(N,
                      prob = NULL,
                      prob_unit = NULL,
                      prob_each = NULL,
                      num_arms = NULL,
                      conditions = NULL,
                      check_inputs = TRUE,
                      simple = TRUE) {
  if (check_inputs) {
    .invoke_check(check_randomizr_arguments_new)
  }
  prob_mat <- simple_ra_probabilities(N, prob, prob_unit, prob_each, num_arms, conditions, FALSE)
  assignment <- conditions[vsample(prob_mat)]
  assignment <- clean_condition_names(assignment, conditions)
  return(assignment)
}

#' Probabilities of assignment: Simple Random Assignment
#'
#' Returns the probability that each unit is assigned to each condition under
#' simple random assignment. Every unit is assigned independently, so the
#' probabilities do not depend on how the other units came out.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in,
#' which \code{\link{obtain_condition_probabilities}} extracts for you.
#'
#' @seealso \code{\link{simple_ra}}
#'
#' @inheritParams simple_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # Two Group Designs
#' prob_mat <- simple_ra_probabilities(N=100)
#' head(prob_mat)
#'
#' prob_mat <- simple_ra_probabilities(N=100, prob=0.5)
#' head(prob_mat)
#'
#' prob_mat <- simple_ra_probabilities(N=100, prob_each = c(0.3, 0.7),
#'                         conditions = c("control", "treatment"))
#' head(prob_mat)
#'
#' # Multi-arm Designs
#' prob_mat <- simple_ra_probabilities(N=100, num_arms=3)
#' head(prob_mat)
#'
#' prob_mat <- simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4))
#' head(prob_mat)
#'
#' prob_mat <- simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4),
#'                         conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- simple_ra_probabilities(N=100, conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' @export
simple_ra_probabilities <-
  function(N,
           prob = NULL,
           prob_unit = NULL,
           prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE, 
           simple = TRUE) {
    if (check_inputs) .invoke_check(check_randomizr_arguments_new)
    
    # Three easy cases
    condition_probabilities <-
      if (is.matrix(prob_each)) {
        t(prob_each)
      } else if (is.numeric(prob_each)) {
        prob_each
      } else if (is.numeric(prob_unit)) {
        rbind(1 - prob_unit, prob_unit)
      } else if (is.numeric(prob)) {
        c(1 - prob, prob)
      } else{
        1 / num_arms
      }

    # Build prob_mat
    prob_mat <- matrix(
      condition_probabilities,
      byrow = TRUE,
      nrow = N,
      ncol = length(conditions),
      dimnames = list(NULL,  paste0("prob_", conditions))
    )
    return(prob_mat)
    
  }

