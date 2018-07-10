#' Blocked and Clustered Random Assignment
#'
#' A random assignment procedure in which units are assigned as clusters and clusters are nested within blocks.
#'
#' @param blocks A vector of length N that indicates which block each unit belongs to.
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param prob Use for a two-arm design in which either floor(N_clusters_block*prob) or ceiling(N_clusters_block*prob) clusters are assigned to treatment within each block. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_clusters_block*prob) clusters will be assigned to treatment and with probability prob, ceiling(N_clusters_block*prob) clusters will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilities of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of clusters assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param m Use for a two-arm design in which the scalar m describes the fixed number of clusters assigned in each block. This number does not vary across blocks.
#' @param block_m Use for a two-arm design in which block_m describes the number of clusters to assign to treatment within each block. block_m must be a numeric vector that is as long as the number of blocks and is in the same order as sort(unique(blocks)).
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of clusters assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of clusters to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). The columns should be in the order of conditions, if specified.
#' @param block_prob Use for a two-arm design in which block_prob describes the probability of assignment to treatment within each block. Must be in the same order as sort(unique(blocks)). Differs from prob in that the probability of assignment can vary across blocks. 
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilities of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilities of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An exception is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit.
#'
#' @examples
#' clusters <- rep(letters, times=1:26)
#'
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:5]] <- "block_1"
#' blocks[clusters %in% letters[6:10]] <- "block_2"
#' blocks[clusters %in% letters[11:15]] <- "block_3"
#' blocks[clusters %in% letters[16:20]] <- "block_4"
#' blocks[clusters %in% letters[21:26]] <- "block_5"
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
#'                           prob_each = c(.2, .5, .3))
#'
#' block_m_each <- rbind(c(2, 3),
#'                       c(1, 4),
#'                       c(3, 2),
#'                       c(2, 3),
#'                       c(5, 1))
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
           prob_each = NULL,
           m = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE) {
    
    if (check_inputs) .invoke_check(check_randomizr_arguments_new)
    

    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)

    # get the block for each cluster
    clust_blocks <- tapply(blocks, clusters, unique)

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

#' probabilities of assignment: Blocked and Clustered Random Assignment
#'
#' @inheritParams block_and_cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' clusters <- rep(letters, times=1:26)
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:5]] <- "block_1"
#' blocks[clusters %in% letters[6:10]] <- "block_2"
#' blocks[clusters %in% letters[11:15]] <- "block_3"
#' blocks[clusters %in% letters[16:20]] <- "block_4"
#' blocks[clusters %in% letters[21:26]] <- "block_5"
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
#'                                                prob_each = c(.2, .5, .3))
#' head(prob_mat)                                    
#'
#' block_m_each <- rbind(c(2, 3),
#'                       c(1, 4),
#'                       c(3, 2),
#'                       c(2, 3),
#'                       c(5, 1))
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
           prob_each = NULL,
           m = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           check_inputs = TRUE) {
    

    if (check_inputs) .invoke_check(check_randomizr_arguments_new)
    
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)
    n_clust <- length(n_per_clust)
    
    # get the block for each cluster
    clust_blocks <- tapply(blocks, clusters, unique)
    
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
