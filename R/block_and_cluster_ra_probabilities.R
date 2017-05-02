#' Probabilties of assignment: Blocked and Clustered Random Assignment
#'
#' @inheritParams block_and_cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' clust_var <- rep(letters, times=1:26)
#' block_var <- rep(NA, length(clust_var))
#' block_var[clust_var %in% letters[1:5]] <- "block_1"
#' block_var[clust_var %in% letters[6:10]] <- "block_2"
#' block_var[clust_var %in% letters[11:15]] <- "block_3"
#' block_var[clust_var %in% letters[16:20]] <- "block_4"
#' block_var[clust_var %in% letters[21:26]] <- "block_5"
#'
#'
#' prob_mat <- block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                                block_var = block_var)
#' head(prob_mat)
#'                                     
#' prob_mat <- block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                                block_var = block_var,
#'                                                num_arms = 3)
#' head(prob_mat)
#'                                     
#' prob_mat <- block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                                block_var = block_var,
#'                                                prob_each = c(.2, .5, .3))
#' head(prob_mat)                                    
#'
#' block_m_each <- rbind(c(2, 3),
#'                       c(1, 4),
#'                       c(3, 2),
#'                       c(2, 3),
#'                       c(5, 1))
#'
#' prob_mat <- block_and_cluster_ra_probabilities(clust_var = clust_var, 
#'                                                block_var = block_var, 
#'                                                block_m_each = block_m_each)
#' head(prob_mat)                                    
#'
#'
#' @export
block_and_cluster_ra_probabilities <-
  function(block_var,
           clust_var,
           prob = NULL,
           prob_each = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           check_inputs = TRUE) {
    
    if (check_inputs) {
      check_inputs <-
        check_randomizr_arguments(
          block_var = block_var,
          clust_var = clust_var,
          prob = prob,
          prob_each = prob_each,
          block_m = block_m,
          block_m_each = block_m_each,
          block_prob = block_prob,
          block_prob_each = block_prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    }
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clust_var, clust_var, length)
    n_clust <- length(n_per_clust)
    
    # get the block for each cluster
    clust_blocks <- tapply(block_var, clust_var, unique)
    
    probs_clust <- block_ra_probabilities(
      block_var = clust_blocks,
      prob = prob,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      num_arms = num_arms,
      condition_names = condition_names
    )
    
    prob_mat <- probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <- prob_mat[order(unlist(split(1:length(clust_var),clust_var), FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }
