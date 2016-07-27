#' Cluster Random Sampling
#'
#' cluster_rs implements a random sampling procedure in which groups of units are sampled together (as a cluster). This function conducts complete random sampling at the cluster level, unless simple = TRUE, in which case \code{\link{simple_rs}} analogues are used.
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param m Use for a design in which m clusters are sampled. (optional)
#' @param prob Use for a design in which either floor(N_clusters*prob) or ceiling(N_clusters*prob) clusters are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N_clusters*prob) clusters will be sampled and with probability prob, ceiling(N_clusters*prob) clusters will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param simple logical, defaults to FALSE. If TRUE, simple random sampling of clusters. When simple = TRUE, please do not specify m.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#' @examples
#' clust_var <- rep(letters, times=1:26)
#'
#' S <- cluster_rs(clust_var = clust_var)
#' table(S, clust_var)
#'
#' S <- cluster_rs(clust_var = clust_var, m = 13)
#' table(S, clust_var)
#'
cluster_rs <- function(clust_var,
                       m = NULL,
                       prob = NULL,
                       simple = FALSE) {
  unique_clust <- unique(clust_var)
  n_clust <- length(unique_clust)
  if (simple) {
    if (!is.null(m)) {
      stop("Please do not specify m when simple = TRUE")
    }
    if (!is.null(m_each)) {
      stop("Please do not specify m_each when simple = TRUE")
    }
    S_clust <- simple_rs(
      N = n_clust,
      prob = prob)
    
  } else{
    S_clust <- complete_rs(
      N = n_clust,
      m = m,
      prob = prob)
  }
  merged <-
    merge(
      x = data.frame(clust_var, init_order = 1:length(clust_var)),
      y = data.frame(clust_var = unique_clust, S_clust),
      by = "clust_var"
    )
  merged <- merged[order(merged$init_order),]
  return(merged$S_clust)
}