#' Cluster Random Sampling
#'
#' cluster_rs implements a random sampling procedure in which groups of units are sampled together (as a cluster). This function conducts complete random sampling at the cluster level, unless simple = TRUE, in which case \code{\link{simple_rs}} analogues are used.
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param n Use for a design in which n clusters are sampled. (optional)
#' @param prob Use for a design in which either floor(N_clusters*prob) or ceiling(N_clusters*prob) clusters are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N_clusters*prob) clusters will be sampled and with probability prob, ceiling(N_clusters*prob) clusters will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param simple logical, defaults to FALSE. If TRUE, simple random sampling of clusters. When simple = TRUE, please do not specify n.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#' @examples
#' clust_var <- rep(letters, times=1:26)
#'
#' S <- cluster_rs(clust_var = clust_var)
#' table(S, clust_var)
#'
#' S <- cluster_rs(clust_var = clust_var, n = 13)
#' table(S, clust_var)
#'
cluster_rs <- function(clust_var,
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
    if (!is.null(n)) {
      stop("Please do not specify n when simple = TRUE")
    }
    S_clust <- simple_rs(N = n_clust,
                         prob = prob)
    
  } else{
    S_clust <- complete_rs(N = n_clust,
                           n = n,
                           prob = prob)
  }
  assignment <- rep(S_clust, n_per_clust)
  assignment <-
    assignment[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE))]
  return(assignment)
}