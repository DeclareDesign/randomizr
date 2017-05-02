#' Probabilties of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_rs
#'
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' # Two Group Designs
#' clust_var <- rep(letters, times = 1:26)
#' probs <- cluster_rs_probabilities(clust_var = clust_var)
#' table(probs, clust_var)
#'
#' prob_mat <- cluster_rs_probabilities(clust_var = clust_var, n = 10)
#' table(probs, clust_var)
#'
#' prob_mat <- cluster_rs_probabilities(clust_var = clust_var, prob = .3)
#' table(probs, clust_var)
#'
#'
#' @export
cluster_rs_probabilities <-
  function(clust_var,
           n = NULL,
           prob = NULL,
           simple = FALSE,
           check_inputs = TRUE) {
    if (check_inputs) {
      check_inputs <-
        check_samplr_arguments(n = n,
                               clust_var = clust_var,
                               prob = prob)
    }
    
    n_per_clust <- tapply(clust_var, clust_var, length)
    unique_clust <- names(n_per_clust)
    n_clust <- length(unique_clust)
    
    if (simple) {
      probs_clust <-
        simple_rs_probabilities(N = n_clust,
                                prob = prob)
    } else{
      probs_clust <-
        complete_rs_probabilities(N = n_clust,
                                  n = n,
                                  prob = prob)
    }
    
    
    prob_vec <- rep(probs_clust, n_per_clust)
    prob_vec <-
      prob_vec[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE))]
    return(prob_vec)
  }
