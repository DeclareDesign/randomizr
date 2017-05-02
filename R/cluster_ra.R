#' Cluster Random Assignment
#'
#' cluster_ra implements a random assignment procedure in which groups of units are assigned together (as a cluster) to treatment conditions. This function conducts complete random assignment at the cluster level, unless simple = TRUE, in which case \code{\link{simple_ra}} analogues are used.
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param m Use for a two-arm design in which m clusters are assigned to treatment and N-m clusters are assigned to control. (optional)
#' @param m_each Use for a multi-arm design in which the values of m_each determine the number of clusters assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many clusters should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)
#' @param prob Use for a two-arm design in which either floor(N_clusters*prob) or ceiling(N_clusters*prob) clusters are assigned to treatment. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_clusters*prob) clusters will be assigned to treatment and with probability prob, ceiling(N_clusters*prob) clusters will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilties of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of clusters assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the length of m_each or condition_names.
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named T1, T2, T3, etc.
#' @param simple logical, defaults to FALSE. If TRUE, simple random assignment of clusters to conditions is used. When simple = TRUE, please do not specify m or m_each.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit.
#' @export
#' @examples
#' # Two Group Designs
#' clust_var <- rep(letters, times=1:26)
#'
#' Z <- cluster_ra(clust_var = clust_var)
#' table(Z, clust_var)
#'
#' Z <- cluster_ra(clust_var = clust_var, m = 13)
#' table(Z, clust_var)
#'
#' Z <- cluster_ra(clust_var = clust_var, m_each = c(10, 16),
#'                 condition_names = c("control", "treatment"))
#' table(Z, clust_var)
#'
#' # Multi-arm Designs
#' Z <- cluster_ra(clust_var = clust_var, num_arms = 3)
#' table(Z, clust_var)
#'
#' Z <- cluster_ra(clust_var = clust_var, m_each = c(7, 7, 12))
#' table(Z, clust_var)
#'
#' Z <- cluster_ra(clust_var = clust_var, m_each = c(7, 7, 12),
#'                 condition_names = c("control", "placebo", "treatment"))
#' table(Z, clust_var)
#'
#' Z <- cluster_ra(clust_var = clust_var,
#'                 condition_names = c("control", "placebo", "treatment"))
#' table(Z, clust_var)
cluster_ra <- function(clust_var,
                       m = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_each = NULL,
                       num_arms = NULL,
                       condition_names = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  if (check_inputs) {
    check_inputs <-
      check_randomizr_arguments(
        clust_var = clust_var,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
  }
  
  n_per_clust <- tapply(clust_var, clust_var, length)
  unique_clust <- names(n_per_clust)
  n_clust <- length(unique_clust)
  
  if (simple) {
    if (!is.null(m)) {
      stop("Please do not specify m when simple = TRUE")
    }
    if (!is.null(m_each)) {
      stop("Please do not specify m_each when simple = TRUE")
    }
    z_clust <- simple_ra(
      N = n_clust,
      prob = prob,
      prob_each = prob_each,
      num_arms = num_arms,
      condition_names = condition_names
    )
    
  } else{
    z_clust <- complete_ra(
      N = n_clust,
      m = m,
      m_each = m_each,
      prob = prob,
      prob_each = prob_each,
      num_arms = num_arms,
      condition_names = condition_names
    )
  }
  
  assignment <- rep(z_clust, n_per_clust)
  assignment <-
    assignment[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE))]
  return(assignment)
}