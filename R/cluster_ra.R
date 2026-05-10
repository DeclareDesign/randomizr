#' Cluster Random Assignment
#'
#' \code{cluster_ra} assigns entire groups of units (clusters) to treatment conditions, so that all units within a cluster share the same treatment status. Cluster assignment is appropriate when the intervention can only be delivered at the group level (for example, a school-wide program that cannot be withheld from individual students), when spillovers within groups make individual-level assignment infeasible, or when the treatment is itself defined as a group-level condition. Because all units in a cluster move together, the effective sample size for estimating average effects is the number of clusters, not the number of units. Clustering therefore typically increases sampling variability relative to complete or block random assignment; the precision loss grows with the intra-cluster correlation in potential outcomes.
#'
#' By default, \code{cluster_ra} conducts complete random assignment at the cluster level: a fixed number of clusters are assigned to each condition on every draw. Setting \code{simple = TRUE} switches to independent Bernoulli assignment of clusters.
#'
#' @param clusters A vector of length N indicating which cluster each unit belongs to. (required)
#' @param m Use for a two-arm design in which exactly \code{m} clusters are assigned to treatment. (optional)
#' @param m_unit Use for a two-arm design. \code{unique(m_unit)} clusters are assigned to treatment; must be the same for all units and of length N. (optional)
#' @param m_each Use for a multi-arm design. A numeric vector giving the number of clusters assigned to each condition; must sum to the total number of clusters. (optional)
#' @param prob Use for a two-arm design in which either \code{floor(N_clusters*prob)} or \code{ceiling(N_clusters*prob)} clusters are assigned to treatment. The probability of assignment to treatment is exactly \code{prob}. Must be between 0 and 1. (optional)
#' @param prob_unit Use for a two-arm design. \code{unique(prob_unit)} will be passed to the \code{prob} argument and must be the same for all units. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition; entries must be nonnegative, sum to 1. Because of integer rounding, the exact number of clusters assigned to each condition may differ slightly from assignment to assignment, but the overall probability of assignment is exactly \code{prob_each}. (optional)
#' @param num_arms The total number of treatment arms. If unspecified, determined from \code{m_each} or \code{conditions}. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named T1, T2, T3, etc. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, clusters are assigned to conditions independently (simple random assignment). Do not specify \code{m} or \code{m_each} when \code{simple = TRUE}.
#' @param check_inputs Logical. Defaults to \code{TRUE}.
#'
#' @return A vector of length N indicating the treatment condition of each unit.
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