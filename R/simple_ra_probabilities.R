#' Probabilties of assignment: Simple Random Assignment
#'
#' @inheritParams simple_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # Two Group Designs
#' simple_ra_probabilities(N=100)
#' simple_ra_probabilities(N=100, prob=0.5)
#' simple_ra_probabilities(N=100, prob_each = c(0.3, 0.7),
#'                         condition_names = c("control", "treatment"))
#' # Multi-arm Designs
#' simple_ra_probabilities(N=100, num_arms=3)
#' simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4))
#' simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4),
#'                         condition_names=c("control", "placebo", "treatment"))
#' simple_ra_probabilities(N=100, condition_names=c("control", "placebo", "treatment"))
#'
#' @export
simple_ra_probabilities <-
  function(N,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL) {
    clean_inputs <-
      check_randomizr_arguments(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
    
    num_arms <- clean_inputs$num_arms
    condition_names <- clean_inputs$condition_names
    
    # Three easy cases
    
    if (is.null(prob) & is.null(prob_each)) {
      condition_probabilities <- rep(1 / num_arms, num_arms)
    }
    if (!is.null(prob)) {
      condition_probabilities <- c(1 - prob, prob)
    }
    if (!is.null(prob_each)) {
      condition_probabilities <- prob_each
    }
    
    # Build prob_mat
    prob_mat <- matrix(
      rep(condition_probabilities, N),
      byrow = TRUE,
      ncol = length(condition_probabilities),
      dimnames = list(NULL,  paste0("prob_", condition_names))
    )
    return(prob_mat)
    
  }
