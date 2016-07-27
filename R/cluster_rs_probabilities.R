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
#' prob_mat <- cluster_rs_probabilities(clust_var = clust_var, m = 10)
#' table(probs, clust_var)
#'
#' prob_mat <- cluster_rs_probabilities(clust_var = clust_var, prob = .3)
#' table(probs, clust_var)
#'
#'
#' @export
cluster_rs_probabilities <-
  function(clust_var,
           m = NULL,
           prob = NULL,
           simple = FALSE) {
    unique_clus <- unique(clust_var)
    n_clus <- length(unique_clus)
    
    if (simple) {
      probs_clus <-
        simple_rs_probabilities(N = n_clus,
                                prob = prob)
    } else{
      probs_clus <-
        complete_rs_probabilities(N = n_clus,
                                  m = m,
                                  prob = prob)
    }
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    merged <- merged[order(merged$init_order),]
    prob_vec <- merged$probs_clus
    return(prob_vec)
  }
