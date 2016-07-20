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
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var)
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var,
#'                                    num_arms = 3)
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var,
#'                                    prob_each = c(.2, .5, .3))
#'
#' block_m_each <- rbind(c(2, 3),
#'                      c(1, 4),
#'                      c(3, 2),
#'                      c(2, 3),
#'                      c(5, 1))
#'
#' block_and_cluster_ra_probabilities(clust_var = clust_var, 
#'                                    block_var = block_var, 
#'                                    block_m_each = block_m_each)
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
           block_prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           balance_load = FALSE) {
    unique_clus <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clus))
    for (i in 1:length(unique_clus)) {
      clust_blocks[i] <- unique(block_var[clust_var == unique_clus[i]])
    }
    
    probs_clus <- block_ra_probabilities(
      block_var = clust_blocks,
      block_m = block_m,
      block_m_each = block_m_each,
      num_arms = num_arms,
      prob_each = prob_each,
      block_prob_each = block_prob_each,
      condition_names = condition_names,
      balance_load = balance_load
    )
    
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    merged <- merged[order(merged$init_order),]
    prob_mat <- as.matrix(merged[, colnames(probs_clus)])
    return(prob_mat)
  }
