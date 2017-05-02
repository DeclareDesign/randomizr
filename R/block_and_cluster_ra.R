#' Blocked and Clustered Random Assignment
#'
#' A random assignment procedure in which units are assigned as clusters and clusters are nested within blocks.
#'
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param prob Use for a two-arm design in which either floor(N_clusters_block*prob) or ceiling(N_clusters_block*prob) clusters are assigned to treatment within each block. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_clusters_block*prob) clusters will be assigned to treatment and with probability prob, ceiling(N_clusters_block*prob) clusters will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilties of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of clusters assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param block_m Use for a two-arm design in which block_m describes the number of units to assign to treatment within each block. Note that in previous versions of randomizr, block_m behaved like block_m_each.
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of clusters assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of clusters to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_prob Use for a two-arm design in which block_prob describes the probability of assignment to treatment within each block. Differs from prob in that the probability of assignment can vary across blocks.
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilties of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An execption is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit.
#'
#' @examples
#' clust_var <- rep(letters, times=1:26)
#'
#' block_var <- rep(NA, length(clust_var))
#' block_var[clust_var %in% letters[1:5]] <- "block_1"
#' block_var[clust_var %in% letters[6:10]] <- "block_2"
#' block_var[clust_var %in% letters[11:15]] <- "block_3"
#' block_var[clust_var %in% letters[16:20]] <- "block_4"
#' block_var[clust_var %in% letters[21:26]] <- "block_5"
#'
#'
#' table(block_var, clust_var)
#'
#' Z <- block_and_cluster_ra(block_var = block_var,
#'                           clust_var = clust_var)
#'
#' table(Z, block_var)
#' table(Z, clust_var)
#'
#' Z <- block_and_cluster_ra(block_var = block_var,
#'                           clust_var = clust_var,
#'                           num_arms = 3)
#'
#' table(Z, block_var)
#' table(Z, clust_var)
#'
#' Z <- block_and_cluster_ra(block_var = block_var,
#'                           clust_var = clust_var,
#'                           prob_each = c(.2, .5, .3))
#'
#' block_m_each <- rbind(c(2, 3),
#'                       c(1, 4),
#'                       c(3, 2),
#'                       c(2, 3),
#'                       c(5, 1))
#'
#' Z <- block_and_cluster_ra(block_var = block_var,
#'                           clust_var = clust_var,
#'                           block_m_each = block_m_each)
#'
#' table(Z, block_var)
#' table(Z, clust_var)
#'
#' @export
block_and_cluster_ra <-
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
    
    # get the block for each cluster
    clust_blocks <- tapply(block_var, clust_var, unique)
    
    # Conduct random assignment at cluster level
    z_clust <- block_ra(
      block_var = clust_blocks,
      num_arms = num_arms,
      prob = prob,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      condition_names = condition_names
    )
    
    # back up to the individual level, maintaining original ordering
    assignment <- rep(z_clust, n_per_clust)
    assignment <-
      assignment[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE))]
    return(assignment)
  }
