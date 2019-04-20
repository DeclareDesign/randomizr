#' Cluster Random Assignment
#'
#' cluster_ra implements a random assignment procedure in which groups of units are assigned together (as a cluster) to treatment conditions. This function conducts complete random assignment at the cluster level, unless simple = TRUE, in which case \code{\link{simple_ra}} analogues are used.
#'
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param m Use for a two-arm design in which m clusters are assigned to treatment and N-m clusters are assigned to control. (optional)
#' @param m_unit Use for a two-arm design in which exactly unique(m_unit) clusters are assigned to treatment and the remainder are assigned to control. m_unit must be of length N and must be the same for all units (optional)
#' @param m_each Use for a multi-arm design in which the values of m_each determine the number of clusters assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many clusters should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)
#' @param prob Use for a two-arm design in which either floor(N_clusters*prob) or ceiling(N_clusters*prob) clusters are assigned to treatment. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_clusters*prob) clusters will be assigned to treatment and with probability prob, ceiling(N_clusters*prob) clusters will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Use for a two-arm design. unique(prob_unit) will be passed to the prob argument and must be the same for all units.
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilities of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of clusters assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the length of m_each or conditions.
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named T1, T2, T3, etc.
#' @param simple logical, defaults to FALSE. If TRUE, simple random assignment of clusters to conditions is used. When simple = TRUE, please do not specify m or m_each.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit.
#' @export
#' @examples
#' # Two Group Designs
#' clusters <- rep(letters, times=1:26)
#'
#' Z <- cluster_ra(clusters = clusters)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m = 13)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(10, 16),
#'                 conditions = c("control", "treatment"))
#' table(Z, clusters)
#'
#' # Multi-arm Designs
#' Z <- cluster_ra(clusters = clusters, num_arms = 3)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(7, 7, 12))
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(7, 7, 12),
#'                 conditions = c("control", "placebo", "treatment"))
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters,
#'                 conditions = c("control", "placebo", "treatment"))
#' table(Z, clusters)
cluster_ra <- function(clusters = NULL,
                       m = NULL,
                       m_unit = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_unit = NULL,
                       prob_each = NULL,
                       num_arms = NULL,
                       conditions = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  if (check_inputs)
    .invoke_check(check_randomizr_arguments_new)
  
  n_per_clust <- tapply(clusters, clusters, length)
  n_clust <- length(n_per_clust)
  
  if(!is.null(m_unit)) {
    m_unit <- rep(unique(m_unit), n_clust)
  }
  
  delegate_args <- list(
    N = n_clust,
    prob = prob,
    prob_unit = rep(unique(prob_unit), n_clust),
    prob_each = prob_each,
    num_arms = num_arms,
    conditions = conditions,
    check_inputs = check_inputs
  )
  
  z_clust <- cluster_ra_helper("simple_ra", "complete_ra",
                               delegate_args, simple, m, m_unit, m_each)
  
  assignment <- rep(z_clust, n_per_clust)
  assignment <-
    assignment[order(unlist(split(seq_along(clusters), clusters), FALSE, FALSE))]
  return(assignment)
}
#' probabilities of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' # Two Group Designs
#' clusters <- rep(letters, times = 1:26)
#' prob_mat <- cluster_ra_probabilities(clusters = clusters)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m = 10)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      m_each = c(9, 17),
#'                                      conditions = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, num_arms = 3)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12),
#'                          conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                          conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      prob_each = c(.1, .2, .7))
#' head(prob_mat)
#'
#'
#'
#' @export
cluster_ra_probabilities <-
  function(clusters = NULL,
           m = NULL,
           m_unit = NULL,
           m_each = NULL,
           prob = NULL,
           prob_unit = NULL,
           prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           simple = FALSE,
           check_inputs = TRUE) {
    if (check_inputs)
      .invoke_check(check_randomizr_arguments_new)
    
    n_per_clust <- tapply(clusters, clusters, length)
    unique_clust <- names(n_per_clust)
    n_clust <- length(unique_clust)
    
    
    if(!is.null(m_unit)){m_unit <- rep(unique(m_unit), n_clust)}
    
    delegate_args <- list(
      N = n_clust,
      prob = prob,
      prob_unit = rep(unique(prob_unit), n_clust),
      prob_each = prob_each,
      num_arms = num_arms,
      conditions = conditions,
      check_inputs = check_inputs
    )
    
    
    probs_clust <-
      cluster_ra_helper(
        "simple_ra_probabilities",
        "complete_ra_probabilities",
        delegate_args,
        simple,
        m,
        m_unit,
        m_each
      )
    
    prob_mat <-
      probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <-
      prob_mat[order(unlist(split(seq_along(clusters), clusters),
                            FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }

# consolidated logic for simple vs complete,
cluster_ra_helper <-
  function(simple_delegate,
           complete_delegate,
           delegate_args,
           simple,
           m,
           m_unit,
           m_each) {
    if (simple) {
      delegate <- simple_delegate
    } else{
      delegate <- complete_delegate
      delegate_args$m <- m
      delegate_args$m_unit <- m_unit
      delegate_args$m_each <- m_each
    }
    
    do.call(delegate, delegate_args)
    
  }