#' Simple Random Assignment
#'
#' This function conducts simple random assignment, a procedure in which units are assigned to treatment conditions with a known probability, but the number of units assigned to any condition might vary from one randomization to the next. 
#' @param N The total number of units in the experimental sample.
#' @param prob The probability of assignment to treatment. If specified, a two-group design is assumed.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the length of prob_each or condition_names.
#' @param prob_each A numeric vector giving probabilites of assignment to each treatment group. Must sum to 1. If unspecified, equal probabilities will be assumed.
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named T1, T2, T3, etc.
#' @return A vector of length N that indicates the treatment condition of each unit.
#' @export
#' 
#' @importFrom stats rbinom
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
#'                condition_names = c("control", "treatment"))
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
#'                condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
#' Z <- simple_ra(N=100, condition_names=c("control", "placebo", "treatment"))
#' table(Z)
simple_ra <- function(N, prob=NULL, num_arms=NULL, prob_each=NULL, condition_names = NULL){
  
  clean_inputs <- check_randomizr_arguments(N = N, prob = prob, num_arms = num_arms, prob_each = prob_each, condition_names = condition_names)
  
  # Simple 2 group design, returns zeros and ones
  if(is.null(prob_each) & is.null(condition_names) & is.null(num_arms)){
    if(is.null(prob)){
      prob <- 0.5
    }
    assign <- rbinom(n = N, size = 1, prob = prob)
    return(assign)
  }
  
  # All other types
  num_arms <- clean_inputs$num_arms
  condition_names <- clean_inputs$condition_names
  
  if(is.null(prob_each)){
    prob_each <- rep(1/num_arms, num_arms)
  }
  
  assign <- sample(x = condition_names, size = N, replace = TRUE, prob = prob_each)
  assign <- clean_condition_names(assign, condition_names)
  return(assign) 
}