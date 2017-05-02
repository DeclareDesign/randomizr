#' Probabilties of assignment: Simple Random Assignment
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
#'                         condition_names = c("control", "treatment"))
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
#'                         condition_names=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'                          
#' prob_mat <- simple_ra_probabilities(N=100, condition_names=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' @export
simple_ra_probabilities <-
  function(N,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           check_inputs = TRUE) {
    
    if(check_inputs) {
      
      check_inputs <-
        check_randomizr_arguments(
          N = N,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
      
    }
    num_arms <- check_inputs$num_arms
    condition_names <- check_inputs$condition_names
    
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
