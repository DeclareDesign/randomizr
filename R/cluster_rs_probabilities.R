#' Inclusion Probabilities: Cluster Sampling
#'
#' @inheritParams cluster_rs
#'
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' # Two Group Designs
#' clusters <- rep(letters, times = 1:26)
#' probs <- cluster_rs_probabilities(clusters = clusters)
#' table(probs, clusters)
#'
#' prob_mat <- cluster_rs_probabilities(clusters = clusters, n = 10)
#' table(probs, clusters)
#'
#' prob_mat <- cluster_rs_probabilities(clusters = clusters, prob = .3)
#' table(probs, clusters)
#'
#'
#' @export
cluster_rs_probabilities <-
  function(clusters = clust_var,
           n = NULL,
           prob = NULL,
           simple = FALSE,
           check_inputs = TRUE,
           clust_var = NULL) {
    warn_deprecated_args(clust_var=clust_var)
    if (check_inputs) {
      input_check <-
        check_samplr_arguments(n = n,
                               clusters = clusters,
                               prob = prob)
    }
    
    n_per_clust <- tapply(clusters, clusters, length)
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
      prob_vec[order(unlist(split(1:length(clusters), clusters), FALSE, FALSE))]
    return(prob_vec)
  }
