#' Cluster Random Assignment
#'
#' \code{cluster_ra} assigns entire groups of units (clusters) to treatment conditions, so that all units within a cluster share the same treatment status. Cluster assignment is appropriate when the intervention can only be delivered at the group level (for example, a school-wide program that cannot be withheld from individual students), when spillovers within groups make individual-level assignment infeasible, or when the treatment is itself defined as a group-level condition. Because all units in a cluster move together, the effective sample size for estimating average effects is the number of clusters, not the number of units. Clustering therefore typically increases sampling variability relative to complete or block random assignment; the precision loss grows with the intra-cluster correlation in potential outcomes.
#'
#' By default, \code{cluster_ra} conducts complete random assignment at the cluster level: a fixed number of clusters are assigned to each condition on every draw. Setting \code{simple = TRUE} switches to independent Bernoulli assignment of clusters.
#'
#' @seealso \code{\link{complete_ra}()}, \code{\link{block_and_cluster_ra}()}, \code{\link{cluster_rs}()}, \code{\link{cluster_ra_probabilities}()}
#'
#' @param clusters A vector of length N indicating which cluster each unit belongs to. (required)
#' @param m Use for a two-arm design in which exactly \code{m} clusters are assigned to treatment. (optional)
#' @param m_unit Use for a two-arm design. \code{unique(m_unit)} clusters are assigned to treatment; must be the same for all units and of length N. (optional)
#' @param m_each Use for a multi-arm design. A numeric vector giving the number of clusters assigned to each condition; must sum to the total number of clusters. (optional)
#' @param prob Use for a two-arm design in which either \code{floor(N_clusters*prob)} or \code{ceiling(N_clusters*prob)} clusters are assigned to treatment. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_clusters*prob} and the floor otherwise, so that each cluster's probability of assignment is exactly \code{prob}. When \code{N_clusters*prob} is a whole number the count is fixed. Must be between 0 and 1. (optional)
#' @param prob_unit Use for a two-arm design. \code{unique(prob_unit)} will be passed to the \code{prob} argument and must be the same for all units. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition; entries must be nonnegative, sum to 1. Because of integer rounding, the exact number of clusters assigned to each condition may differ slightly from assignment to assignment, but the overall probability of assignment is exactly \code{prob_each}. (optional)
#' @param num_arms The total number of treatment arms. If unspecified, determined from \code{m_each} or \code{conditions}. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named T1, T2, T3, etc. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, clusters are assigned to conditions independently (simple random assignment at the cluster level), so the number of treated clusters varies from draw to draw. Do not specify \code{m} or \code{m_each} when \code{simple = TRUE}. (optional)
#' @param check_inputs Logical. Whether to verify before assigning that the arguments are internally consistent: that counts sum to the number of clusters, that probabilities lie between 0 and 1 and sum to 1, and so on. Defaults to \code{TRUE}. \code{FALSE} skips the checking only: \code{num_arms} and \code{conditions} are still derived from the other arguments, so the same call draws the same assignment either way. What goes is the verification, and an impossible design is then no longer refused. \code{block_m} larger than a block, for instance, quietly treats the whole block. Declaring the design once with \code{\link{declare_ra}()} and drawing from it with \code{\link{conduct_ra}()} is the usual way to avoid re-checking the same arguments in a simulation. (optional)
#'
#' @return A vector of length N indicating the treatment condition of each unit. Every unit in a cluster receives the same value. Numeric in a two-arm trial; a factor (ordered by \code{conditions}) in a multi-arm trial.
#' @export
#' @examples
#' # Ten clusters, of sizes 1 through 10
#' clusters <- rep(letters[1:10], times = 1:10)
#'
#' # Two Group Designs
#'
#' Z <- cluster_ra(clusters = clusters)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m = 4)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(6, 4),
#'                 conditions = c("control", "treatment"))
#' table(Z, clusters)
#'
#' # Multi-arm Designs
#' Z <- cluster_ra(clusters = clusters, num_arms = 3)
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(3, 3, 4))
#' table(Z, clusters)
#'
#' Z <- cluster_ra(clusters = clusters, m_each = c(3, 3, 4),
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
  else
    .invoke_derive()

  # tapply drops NA silently, so an NA cluster would shorten the assignment
  # with no error; 1.x returned 5 values for 6 units here.
  if (anyNA(clusters)) stop("`clusters` must not contain NA.", call. = FALSE)

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
#' Probabilities of assignment: Cluster Random Assignment
#'
#' Returns the probability that each unit is assigned to each condition under
#' cluster random assignment. Every unit in a cluster shares its cluster's
#' probability, since clusters move together.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in,
#' which \code{\link{obtain_condition_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{cluster_ra}()}
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix with N rows and one column per treatment condition, with columns named \code{prob_<condition>}. Entry (i, j) is the probability that unit i is assigned to condition j, and every row sums to 1.
#'
#' @examples
#'
#' # Two Group Designs
#' clusters <- rep(letters[1:10], times = 1:10)
#' prob_mat <- cluster_ra_probabilities(clusters = clusters)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m = 4)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      m_each = c(6, 4),
#'                                      conditions = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, num_arms = 3)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(3, 3, 4))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(3, 3, 4),
#'                          conditions = c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                          conditions = c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      prob_each = c(0.1, 0.2, 0.7))
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
    else
      .invoke_derive()
    
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