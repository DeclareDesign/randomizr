
#' Blocked and Clustered Random Assignment.
#' 
#' If clusters are nested within blocks, blocked and clustered random assignment is possible. For example, imagine that villages are nested within regions. 
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m or the length of condition_names.
#' @param block_m A matrix of arm sizes whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified.
#' @param prob_each A vector whose length is equal to the number of treatment assignments. When specified, prob_each assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. prob_each must sum to 1.
#' @param condition_names A character vector giving the names of the treatment conditions. If unspecified, the treatment conditions. will be named T1, T2, T3, etc.
#'
#' @return A random assignment
#' @export
block_and_cluster_ra <- 
  function(clust_var, block_var, num_arms = NULL, block_m=NULL, prob_each=NULL, condition_names = NULL) {
    
    # confirm that all units within clusters are in the same block
    # is there a computationally faster way to confirm this (possible c++ loop?)
    
    if(!all(rowSums(table(clust_var, block_var) != 0)==1)){
      stop("All units within a cluster must be in the same block.")
    }
    
    # Setup: obtain unique clusters
    unique_clust <- unique(clust_var)
    
    # get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clust))
    for(i in 1:length(unique_clust)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clust[i]])  
    }
    
    # Conduct random assignment at cluster level
    z_clust <- block_ra(block_var = clust_blocks, 
                        num_arms = num_arms,
                        block_m = block_m, 
                        prob_each = prob_each,
                        condition_names = condition_names)
    
    # Merge back up to the individual level, maintaining original ordering
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    y = data.frame(clust_var=unique_clust, z_clust), by="clust_var")
    merged <- merged[order(merged$init_order),]
    return(merged$z_clust)
  }
