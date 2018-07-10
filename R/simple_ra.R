#' Simple Random Assignment
#'
#' simple_ra implements a random assignment procedure in which units are independently assigned to treatment conditions. Because units are assigned independently, the number of units that are assigned to each condition can vary from assignment to assignment. For most experimental applications in which the number of experimental units is known in advance, \code{\link{complete_ra}} is better because the number of units assigned to each condition is fixed across assignments.\cr\cr
#' In most cases, users should specify N and not more than one of prob, prob_each, or num_arms. \cr \cr
#' If only N is specified, a two-arm trial with prob = 0.5 is assumed.

#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param prob Use for a two-arm design. prob is the probability of assignment to treatment and must be a real number between 0 and 1 inclusive and must be length 1 or N. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilities of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. It may be a conditions-length vector or a N-by-conditions matrix.  (optional)
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An exception is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by conditions) in a multi-arm trial.
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
                      prob_each = NULL,
                      num_arms = NULL,
                      conditions = NULL,
                      check_inputs = TRUE) {
  if (check_inputs) {
    .invoke_check(check_randomizr_arguments_new)
  }
  prob_mat <-
    simple_ra_probabilities(N, prob, prob_each, num_arms, conditions, FALSE)
  assignment <- conditions[vsample(prob_mat)]
  assignment <- clean_condition_names(assignment, conditions)
  return(assignment)
}

#' probabilities of assignment: Simple Random Assignment
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
           prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE) {
    if (check_inputs) .invoke_check(check_randomizr_arguments_new)
    
    # Three easy cases
    condition_probabilities <-
      if (is.matrix(prob_each)) {
        t(prob_each)
      } else if (is.numeric(prob_each)) {
        prob_each
      } else if (length(prob) > 1) {
        rbind(1 - prob, prob)
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

