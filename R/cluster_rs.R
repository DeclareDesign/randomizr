#' Cluster Random Sampling
#'
#' cluster_rs implements a random sampling procedure in which groups of units are sampled together (as a cluster). This function conducts complete random sampling at the cluster level, unless simple = TRUE, in which case \code{\link{simple_rs}} analogues are used.
#'
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param n Use for a design in which n clusters are sampled. (optional)
#' @param prob Use for a design in which either floor(N_clusters*prob) or ceiling(N_clusters*prob) clusters are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N_clusters*prob) clusters will be sampled and with probability prob, ceiling(N_clusters*prob) clusters will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param simple logical, defaults to FALSE. If TRUE, simple random sampling of clusters. When simple = TRUE, please do not specify n.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#' @examples
#' clusters <- rep(letters, times=1:26)
#'
#' S <- cluster_rs(clusters = clusters)
#' table(S, clusters)
#'
#' S <- cluster_rs(clusters = clusters, n = 13)
#' table(S, clusters)
#'
cluster_rs <- function(clusters=NULL,
                       n = NULL,
                       prob = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  n_per_clust <- tapply(clusters, clusters, length)
  unique_clust <- names(n_per_clust)
  n_clust <- length(unique_clust)
  
  if (simple) {
    S_clust <- simple_rs(N = n_clust,
                         prob = prob)
    
  } else{
    S_clust <- complete_rs(N = n_clust,
                           n = n,
                           prob = prob)
  }
  assignment <- rep(S_clust, n_per_clust)
  assignment <-
    assignment[order(unlist(split(1:length(clusters), clusters), FALSE, FALSE))]
  return(assignment)
}
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
  function(clusters = NULL,
           n = NULL,
           prob = NULL,
           simple = FALSE,
           check_inputs = TRUE) {
    if (check_inputs) .invoke_check(check_samplr_arguments_new)
    
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
