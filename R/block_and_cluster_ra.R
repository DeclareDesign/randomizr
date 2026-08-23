#' Blocked and Clustered Random Assignment
#'
#' \code{block_and_cluster_ra} assigns whole clusters to conditions, conducting
#' the assignment separately within each block. Use it when treatment can only
#' be delivered to a group and the groups differ in ways worth balancing on.
#' Clustering costs precision, since the effective sample size is the number of
#' clusters rather than the number of units; blocking buys some of it back by
#' guaranteeing treated and control clusters within every block.
#'
#' Clusters must nest within blocks: every unit in a cluster has to belong to
#' the same block.
#'
#' @seealso \code{\link{cluster_ra}()}, \code{\link{block_ra}()},
#'   \code{\link{strata_and_cluster_rs}()}
#'
#' @param blocks A vector of length N indicating which block each unit belongs to. Every unit in a cluster must belong to the same block. (required)
#' @param clusters A vector of length N indicating which cluster each unit belongs to. (required)
#' @param prob Use for a two-arm design in which either \code{floor(N_clusters_block*prob)} or \code{ceiling(N_clusters_block*prob)} clusters are assigned to treatment within each block. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_clusters_block*prob} and the floor otherwise, which makes each cluster's probability of assignment exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Use for a two-arm design. Must be of length N. \code{tapply(prob_unit, blocks, unique)} will be passed to \code{block_prob}, so it must be constant within each block. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition. All entries must be between 0 and 1 inclusive and must sum to 1. Because of integer rounding, the exact number of clusters assigned to each condition may differ slightly from assignment to assignment, but the overall probability of assignment is exactly \code{prob_each}. (optional)
#' @param m Use for a two-arm design in which the scalar \code{m} gives the fixed number of clusters assigned to treatment within every block. This count does not vary across blocks. (optional)
#' @param m_unit Use for a two-arm design. Must be of length N. \code{tapply(m_unit, blocks, unique)} will be passed to \code{block_m}, so it must be constant within each block. (optional)
#' @param block_m Use for a two-arm design in which \code{block_m} gives the number of clusters to assign to treatment within each block. Must be a numeric vector as long as the number of blocks, in the same order as \code{sort(unique(blocks))}. (optional)
#' @param block_m_each Use for a multi-arm design in which \code{block_m_each} gives the number of clusters assigned to each condition within each block. Must be a matrix with one row per block and one column per treatment arm. Rows respect the ordering of blocks by \code{sort(unique(blocks))}; columns should be in the order of \code{conditions}, if specified. (optional)
#' @param block_prob Use for a two-arm design in which the probability of assignment to treatment varies across blocks. Must be in the same order as \code{sort(unique(blocks))}. Differs from \code{prob} in that the probability of assignment can vary across blocks. (optional)
#' @param block_prob_each Use for a multi-arm design in which assignment probabilities vary across blocks. Must be a matrix with one row per block and one column per treatment arm; each row must sum to 1. Rows respect the ordering of \code{sort(unique(blocks))}. Use only if the probabilities of assignment should vary by block, otherwise use \code{prob_each}. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param check_inputs Logical. Whether to verify before assigning that the arguments are internally consistent: that clusters nest within blocks, that counts sum to the number of clusters in each block, that probabilities lie between 0 and 1 and sum to 1, and so on. Defaults to \code{TRUE}. \code{FALSE} skips the checking only: \code{num_arms} and \code{conditions} are still derived from the other arguments, so the same call draws the same assignment either way. What goes is the verification, and an impossible design is then no longer refused. \code{block_m} larger than a block, for instance, quietly treats the whole block. Declaring the design once with \code{\link{declare_ra}()} and drawing from it with \code{\link{conduct_ra}()} is the usual way to avoid re-checking the same arguments in a simulation. (optional)
#'
#' @return A vector of length N indicating the treatment condition of each unit. Every unit in a cluster receives the same value. Numeric in a two-arm trial; a factor (ordered by \code{conditions}) in a multi-arm trial.
#'
#' @examples
#' # Twelve clusters, of sizes 1 through 12, nested in four blocks of three
#' clusters <- rep(letters[1:12], times = 1:12)
#'
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:3]] <- "block_1"
#' blocks[clusters %in% letters[4:6]] <- "block_2"
#' blocks[clusters %in% letters[7:9]] <- "block_3"
#' blocks[clusters %in% letters[10:12]] <- "block_4"
#'
#'
#' table(blocks, clusters)
#'
#' Z <- block_and_cluster_ra(blocks = blocks,
#'                           clusters = clusters)
#'
#' table(Z, blocks)
#' table(Z, clusters)
#'
#' Z <- block_and_cluster_ra(blocks = blocks,
#'                           clusters = clusters,
#'                           num_arms = 3)
#'
#' table(Z, blocks)
#' table(Z, clusters)
#'
#' Z <- block_and_cluster_ra(blocks = blocks,
#'                           clusters = clusters,
#'                           prob_each = c(0.2, 0.5, 0.3))
#'
#' # One row per block, one column per arm: how many clusters go where
#' block_m_each <- rbind(c(1, 2),
#'                       c(2, 1),
#'                       c(1, 2),
#'                       c(2, 1))
#'
#' Z <- block_and_cluster_ra(blocks = blocks,
#'                           clusters = clusters,
#'                           block_m_each = block_m_each)
#'
#' table(Z, blocks)
#' table(Z, clusters)
#'
#' @export
block_and_cluster_ra <-
  function(blocks = NULL,
           clusters = NULL,
           prob = NULL,
           prob_unit = NULL,
           prob_each = NULL,
           m = NULL,
           m_unit = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE) {
    
    if (check_inputs) .invoke_check(check_randomizr_arguments_new) else .invoke_derive()

    # tapply drops NA silently, so an NA would shorten the assignment with no
    # error. Unused factor levels are dropped so a subset of a factor behaves
    # like the same subset of a character vector.
    if (anyNA(blocks)) stop("`blocks` must not contain NA.", call. = FALSE)
    if (anyNA(clusters)) stop("`clusters` must not contain NA.", call. = FALSE)
    if (is.factor(blocks)) blocks <- droplevels(blocks)
    if (is.factor(clusters)) clusters <- droplevels(clusters)

    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)

    # get the block for each cluster
    clust_blocks <- tapply(blocks, clusters, unique)
    
    if(!is.null(prob_unit)){
      block_prob <- tapply(prob_unit, blocks, unique)
    }
    
    if(!is.null(m_unit)){
      block_m <- tapply(m_unit, blocks, unique)
    }

    # Conduct random assignment at cluster level
    z_clust <- block_ra(
      blocks = clust_blocks,
      num_arms = num_arms,
      prob = prob,
      prob_each = prob_each,
      m = m,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      conditions = conditions,
      check_inputs = check_inputs
    )

    # back up to the individual level, maintaining original ordering
    assignment <- rep(z_clust, n_per_clust)
    assignment <-
      assignment[order(unlist(split(seq_along(clusters), clusters), 
                              FALSE, FALSE))]
    return(assignment)
  }

#' Probabilities of assignment: Blocked and Clustered Random Assignment
#'
#' Returns the probability that each unit is assigned to each condition when
#' clusters are assigned within blocks. Probabilities vary across blocks and are
#' constant within a cluster.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in,
#' which \code{\link{obtain_condition_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{block_and_cluster_ra}()}
#'
#' @inheritParams block_and_cluster_ra
#'
#' @return A matrix with N rows and one column per treatment condition, with columns named \code{prob_<condition>}. Entry (i, j) is the probability that unit i is assigned to condition j, and every row sums to 1.
#'
#' @examples
#'
#' clusters <- rep(letters[1:12], times = 1:12)
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:3]] <- "block_1"
#' blocks[clusters %in% letters[4:6]] <- "block_2"
#' blocks[clusters %in% letters[7:9]] <- "block_3"
#' blocks[clusters %in% letters[10:12]] <- "block_4"
#'
#'
#' prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
#'                                                blocks = blocks)
#' head(prob_mat)
#'                                     
#' prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
#'                                                blocks = blocks,
#'                                                num_arms = 3)
#' head(prob_mat)
#'                                     
#' prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters,
#'                                                blocks = blocks,
#'                                                prob_each = c(0.2, 0.5, 0.3))
#' head(prob_mat)                                    
#'
#' # One row per block, one column per arm: how many clusters go where
#' block_m_each <- rbind(c(1, 2),
#'                       c(2, 1),
#'                       c(1, 2),
#'                       c(2, 1))
#'
#' prob_mat <- block_and_cluster_ra_probabilities(clusters = clusters, 
#'                                                blocks = blocks, 
#'                                                block_m_each = block_m_each)
#' head(prob_mat)                                    
#'
#'
#' @export
block_and_cluster_ra_probabilities <-
  function(blocks = NULL,
           clusters = NULL,
           prob = NULL,
           prob_unit = NULL,
           prob_each = NULL,
           m = NULL,
           m_unit = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE) {
    

    if (check_inputs) .invoke_check(check_randomizr_arguments_new) else .invoke_derive()
    
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)
    n_clust <- length(n_per_clust)
    
    # get the block for each cluster
    clust_blocks <- tapply(blocks, clusters, unique)
    
    if(!is.null(prob_unit)){
      block_prob <- tapply(prob_unit, blocks, unique)
    }
    
    if(!is.null(m_unit)){
      block_m <- tapply(m_unit, blocks, unique)
    }
    
    probs_clust <- block_ra_probabilities(
      blocks = clust_blocks,
      prob = prob,
      prob_each = prob_each,
      m = m,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      num_arms = num_arms,
      conditions = conditions,
      check_inputs = check_inputs
    )
    
    prob_mat <- probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <- prob_mat[order(unlist(split(seq_along(clusters),clusters),
                                      FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }
