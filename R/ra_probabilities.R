#' Probabilties of assignment: Complete Random Assignment
#'
#' @param N the total number of units in the experimental sample (required).
#' @param m If specified, a two-group design is assumed. m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units are assigned to treatment. If not specified, half of the sample (N/2) will be assigned to treatment (if N is odd, m will be set to either floor(N/2) or ceiling(N/2) with equal probability. m is NULL by default. 
#' @param prob If specified, a two-group design is assumed. prob is the probability of assignment to treatment. Within rounding, N*prob subjects will be assigned to treatment.
#' @param num_arms The total number of treatment arms. If unspecified, num_arms will be determined from the length of m_each or condition_names.
#' @param m_each A numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed.
#' @param prob_each A numeric giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each.
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be names T1, T2, T3, etc. An execption is a two-group design in which N only or N and m are specified, in which the condition names are 0 and 1.
#'
#' @return A matrix of probabilities of assignment.
#' @export
complete_ra_probabilities <- function(N, m = NULL, prob = NULL, num_arms = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  
  # Setup: obtain number of arms and condition_names
  
  if(is.null(num_arms)){
    num_arms <- 2
    if(!is.null(m_each)){num_arms <- length(m_each)}
    if(!is.null(prob_each)){num_arms <- length(prob_each)}
    if(!is.null(condition_names)){num_arms <- length(condition_names)}
  }
  
  if(is.null(condition_names)){
    if(num_arms==2){
      condition_names = c(0,1)
    }else{
      condition_names <- paste0("T", 1:num_arms)    
      }
  }
  
  # Case 0: Two Arms and N = 1
  if(is.null(m_each) & is.null(prob_each) & num_arms ==2 & N ==1) {
    prob <- 0.5
    prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
  }
  
  # Case 1: Two Arms and N > 1
  if(is.null(m_each) & is.null(prob_each) & num_arms==2 & N > 1){
    m_floor <- m
    m_ceiling <- m
    
    if(is.null(m)){
      m_floor <- floor(N/2)
      m_ceiling <- ceiling(N/2)
    }
    
    if(is.null(m)){
      m_floor <- floor(N*prob)
      m_ceiling <- ceiling(N*prob)
    }
    
    prob <- 0.5*(m_floor/N) + 0.5*(m_ceiling/N)
    prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, 
                       dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
  }
  
  # Case 2: We need to obtain "condition_probabilities" then make a matrix.
  
  # 2a: If m_each is specified
  if(!is.null(m_each) & is.null(prob_each)){
    remainder <-  N%%num_arms
    condition_probabilities <- (m_each/N)
  }
  
  # 2b: if neither m_each nor prob_each is specified
  if(is.null(m_each) & is.null(prob_each)){
    m_each <- rep(N%/%num_arms, num_arms)
    remainder <-  N%%num_arms
    condition_probabilities <- 
      (1-(remainder/num_arms))*(m_each/N) +
      (remainder/num_arms)*((m_each +1)/N)
  }
  
  # 2c: if prob_each is specified
  if(!is.null(prob_each)){
    m_each <- floor(N*prob_each)
    remainder <- N - sum(m_each)
    condition_probabilities <- 
      (1-(remainder/length(prob_each)))* (m_each/N) +
      (remainder/length(prob_each))* ((m_each +1)/N)
  } 
  
  # 2d: if N is smaller than number of arms, we just flip coins
  if(N < num_arms){
    condition_probabilities <- rep(N/num_arms, num_arms)
  }
  
  # Build prob_mat
  prob_mat <- matrix(rep(condition_probabilities, N), 
                     byrow=TRUE, ncol=length(condition_probabilities), 
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  return(prob_mat)
  
}

#' Probabilties of assignment: Block Random Assignment
#'
#' @param block_var A vector of length N indicating which block each unit belongs to.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m or the length of condition_names.
#' @param block_m A matrix of arm sizes whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified.
#' @param prob_each A vector whose length is equal to the number of treatment conditions. When specified, prob_each assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. prob_each must sum to 1.
#' @param condition_names A character vector giving the names of the treatment conditions. If unspecified, the treatment conditions. will be named T1, T2, T3, etc.
#'
#' @return A matrix of probabilities of assignment.
#' @export
block_ra_probabilities <- function(block_var, num_arms = NULL, block_m=NULL, prob_each = NULL, condition_names = NULL){
  
  # Setup: obtain number of arms and condition_names
  
  if(is.null(num_arms)){
    num_arms <- 2
    if(!is.null(block_m)){num_arms <- dim(block_m)[2]}
    if(!is.null(prob_each)){num_arms <- length(prob_each)}
    if(!is.null(condition_names)){num_arms <- length(condition_names)}
  }
  
  if(is.null(condition_names)){
    if(num_arms==2){
      condition_names = c(0,1)
    }else{
      condition_names <- paste0("T", 1:num_arms)    
    }
  }
  
  blocks <- sort(unique(block_var))
  prob_mat <- matrix(NA, 
                     nrow = length(block_var), 
                     ncol = length(condition_names),
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  
  # Case 1: Assume (approximately) equal probabilities for all blocks and conditions.
  if(is.null(block_m) & is.null(prob_each) & is.null(prob_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probabilities(N = N_block, condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 2: block_m is specified
  if(!is.null(block_m)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probabilities(N = N_block, 
                                                                   m_each = block_m[i,], 
                                                                   condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 3: prob_each is specified
  if(!is.null(prob_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probabilities(N = N_block, 
                                                                   prob_each = prob_each, 
                                                                   condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 4: prob_each is specified
  if(!is.null(prob_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probabilities(N = N_block, 
                                                                   prob_each = prob_each[i,], 
                                                                   condition_names=condition_names)
    }
    return(prob_mat)
  }
}

#' Probabilties of assignment: Cluster Random Assignment
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param m The total number clusters to be treated. Should only be specified for a two group design in which exactly m of N clusters is assigned to treatment. If not specified, half of the clusters will be assigned to treatment. Is NULL by default. 
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the length of m_each or condition_names.
#' @param m_each A numeric vector giving the number of clusters to be assigned to each treatment group. Must sum to the total number of clusters. If unspecified, equally sized (rounded) groups will be assumed.
#' @param prob_each A numeric vector giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each.
#' @param condition_names A character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc. 
#'
#' @return A matrix of probabilities of assignment.
#' 
#' @export
cluster_ra_probabilities <- function(clust_var, m=NULL, num_arms = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  probs_clus <- complete_ra_probabilities(N = n_clus, m = m, num_arms = num_arms, m_each = m_each, prob_each = prob_each, condition_names = condition_names)
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
  merged <- merged[order(merged$init_order),]
  prob_mat <- as.matrix(merged[,colnames(probs_clus)])
  return(prob_mat)
}

#' Probabilties of assignment: Blocked and Clustered Random Assignment
#'
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m or the length of condition_names. 
#' @param block_m A matrix of arm sizes whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified.
#' @param prob_each A vector whose length is equal to the number of treatment assignments. When specified, prob_each assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. prob_each must sum to 1.
#' @param condition_names A character vector giving the names of the treatment conditions. If unspecified, the treatment conditions. will be named T1, T2, T3, etc.
#'
#' @return A matrix of probabilities of assignment.
#' @export
block_and_cluster_ra_probabilities <- 
  function(clust_var, block_var, num_arms = NULL, block_m=NULL, prob_each=NULL, condition_names = NULL){
    unique_clus <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clus))
    for(i in 1:length(unique_clus)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clus[i]])  
    }
    
    probs_clus <- block_ra_probabilities(block_var = clust_blocks, 
                                         block_m = block_m,
                                         num_arms = num_arms,
                                         prob_each = prob_each,
                                         condition_names = condition_names)
    
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
    merged <- merged[order(merged$init_order),]
    prob_mat <- as.matrix(merged[,colnames(probs_clus)])
    return(prob_mat)
  }




#' Calculate Probability of Observed Condition
#'
#' This function calculates the probability that each experimental unit is in the experimental condition that it is in.. You specify designs exactly as you do in `simple_ra()`, `complete_ra()`, `block_ra()` or `cluster_ra()`, adding only the `design` argument. 
#' Especially when units have different probabilities of assignment, this function can be useful for calculating inverse probability weights.
#' @param Z a random assignment generated by `randomizr`. Use the identical parameters when generating the assignment and calculating the probabilities.
#' @param N the total number of units in the experimental sample
#' @param prob if specified, a two-group design is assumed. prob is the probability of assignment to treatment.
#' @param m if specified, a two-group design is assumed.  m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units to treatment. If not specified, half of the sample (N/2) will be assigned to treatment. Is null by default. In clustered designs, exactly m of N clusters is assigned to treatment. If not specified, half of the clusters will be assigned to treatment.
#' @param m_each a numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed.
#' @param prob_each a numeric giving the probability of assignment to each treatment arm. Must sum to 1.  Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each.
#' @param block_var A vector of length N that includes the blocking variable
#' @param block_m A matrix whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified.
#' @param prob_each A vector whose length is equal to the number of treatment assignments. When specified, prob_each assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. prob_each must sum to 1.
#' @param clust_var a vector of length N that describes which cluster each unit belongs to.
#' @param num_arms the total number of treatment arms. If unspecified, will be determined from the length of m_each or condition_names.
#' @param condition_names a character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc.
#' @param design a string that specifies the design used.  Can only take the values "simple", "complete", "block", or "cluster".
#' @param return_design a logical value that specifies whether a dataframe with Z, the probabilities of each condition, and the observed condition probability should be returned.  Defaults to FALSE.
#' @keywords random assignment
#' @export
#' @examples
# Simple designs
#' N <- 100
#' Z <- simple_ra(N)
#' condition_probs(Z=Z, N=N, design = "simple")
#' 
#' Z <- simple_ra(N=N,prob_each = c(.1, .2, .7), condition_names = c("A", "B", "C"))
#' condition_probs(Z=Z, N=N,prob_each = c(.1, .2, .7), condition_names = c("A", "B", "C"), 
#'                design = "simple", return_design=TRUE)
#' 
#' # Complete designs
#' N <- 100 
#' Z <- complete_ra(N=N, m = 45)
#' condition_probs(Z=Z, N=N, m = 45, design = "complete")
#' 
#' Z <- complete_ra(N=N, m_each = c(10, 20, 70))
#' condition_probs(Z=Z, N=N, m_each = c(10, 20, 70), 
#'              design = "complete", return_design=TRUE)
#' 
#' # Block designs
#' 
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' block_m <- rbind(c(30, 20),
#'                  c(50, 50),
#'                  c(100, 100))
#' Z <- block_ra(block_var=block_var, block_m=block_m)                  
#' condition_probs(Z=Z, block_var=block_var, block_m=block_m, design="block")
#' 
#' Z <- block_ra(block_var=block_var, prob_each=c(.1, .1, .8),
#'                condition_names=c("control", "placebo", "treatment"))
#' condition_probs(Z=Z, block_var=block_var, prob_each=c(.1, .1, .8),
#'                condition_names=c("control", "placebo", "treatment"), 
#'                design="block")
#'                
#' # Cluster designs
#' 
#' clust_var <- rep(letters, times=1:26)
#' Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12))
#' condition_probs(Z=Z, clust_var=clust_var, m_each=c(7, 7, 12), 
#' design="cluster", return_design=TRUE)
condition_probs <- function(Z, N= NULL, prob=NULL, prob_each=NULL, 
                            m = NULL, m_each = NULL,
                            block_var = NULL, block_m = NULL,
                            clust_var = NULL, 
                            num_arms=NULL, condition_names = NULL, 
                            design,return_design=FALSE){
  probs_mat <- design_probs(N= N, prob=prob, prob_each=prob_each, 
                            m = m, m_each = m_each,
                            block_var = block_var, block_m = block_m,
                            clust_var = clust_var, 
                            num_arms = num_arms, condition_names = condition_names, design)
  cond_Z <- cond_Z <- paste0("prob_", Z)
  indicies <- sapply(colnames(probs_mat), FUN= x <- function(cond_name, cond_Z){cond_Z == cond_name}, cond_Z=cond_Z)
  cond_probs <- as.vector(t(probs_mat))[as.vector(t(indicies))]
  
  if(return_design == TRUE){
    return(data.frame(Z=Z, probs_mat, cond_probs=cond_probs))
  }
  return(cond_probs)
}


