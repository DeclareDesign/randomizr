#' Probabilties of assignment: Blocked and Clustered Random Assignment
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
  function(blocks = block_var,
           clusters = clust_var,
           prob = NULL,
           prob_each = NULL,
           m = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           check_inputs = TRUE,
           block_var = NULL,
           clust_var = NULL) {
    
    warn_deprecated_args(block_var, clust_var)
    
    
    if (check_inputs) {
      input_check <-
        check_randomizr_arguments(
          blocks = blocks,
          clusters = clusters,
          prob = prob,
          prob_each = prob_each,
          m = m,
          block_m = block_m,
          block_m_each = block_m_each,
          block_prob = block_prob,
          block_prob_each = block_prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    }
    
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
      condition_names = condition_names,
      check_inputs = check_inputs
    )
    
    prob_mat <- probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <- prob_mat[order(unlist(split(1:length(clusters),clusters), FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }
