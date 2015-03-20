# randomizr v 0.2

#' Complete Random Assignment
#'
#' This function conducts complete random assignment, a procedure in which a pre-specified number of units is assigned to each treatment condition.  This function can accomodate any number of treatment arms.
#' @param N the total number of units in the experimental sample.
#' @param m if specified, a two-group design is assumed.  m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units to treatment. If not specified, half of the sample (N/2) will be assigned to treatment. Is null by default.
#' @param num_arms the total number of treatment arms. If unspecified, will be determined from the length of m_each or condition_names.
#' @param m_each a numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed.
#' @param prob_each a numeric giving the probability of assignment to each treatment arm. Must sum to 1.  Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each.
#' @param condition_names a character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc.
#' @keywords random assignment
#' @export
#' @examples
#' # Two Group Designs
#'
#' Z <- complete_ra(N=100)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m=50)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each = c(30, 70), 
#'                  condition_names = c("control", "treatment"))
#' table(Z)
#' 
#' # Multi-arm Designs
#' Z <- complete_ra(N=100, num_arms=3)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each=c(30, 30, 40))
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each=c(30, 30, 40), 
#'                  condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
#' Z <- complete_ra(N=100, condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
complete_ra <- function(N,m=NULL, num_arms=NULL, m_each = NULL, prob_each=NULL, condition_names = NULL){
  
  if(!is.null(m) & !is.null(condition_names)){
    stop("Do not specify m and condition_names together. Use m_each and condition_names instead.")
  }
  if(!is.null(prob_each) & !is.null(m_each)){
    stop("Do not specify prob_each and m_each together. Use one or the other.")
  }
  # Simple 2 group design, returns zeros and ones
  if(is.null(m_each) & is.null(condition_names) & is.null(num_arms) & is.null(prob_each)){
    if(is.null(m)){
      coin_flip <- rbinom(1,1,.5)
      if(coin_flip==0) m <- floor(N/2)
      if(coin_flip==1) m <- ceiling(N/2)
    }
    if(m >= N){
      stop("The number of units assigned to treatment (m) must be smaller than the total number of units (N)")
    }

    assign <- ifelse(1:N %in% sample(1:N,m),1,0)
    return(assign)
  }
  
  # All other types
  
  if(all(!is.null(m_each),sum(m_each) != N)){
    stop("The sum of number assigned to each condition (m_each) must equal the total number of units (N)")
  }
  if(all(!is.null(condition_names), !is.null(m_each), length(m_each) != length(condition_names))){
    stop("The length of conditions_names must equal the length of m_each")
  }
  if(all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != length(condition_names))){
    stop("The length of conditions_names must equal the length of prob_each")
  }
  if(all(!is.null(m_each), !is.null(num_arms),length(m_each) != num_arms)){
    stop("The number of arms (n_arms) must equal the length of m_each")
  }
  if(all(!is.null(prob_each), !is.null(num_arms),length(prob_each) != num_arms)){
    stop("The number of arms (n_arms) must equal the length of prob_each")
  }
  if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
    stop("The length of conditions_names must equal the number of arms (n_arms)")
  }
  
  if(!is.null(prob_each)){
    if(sum(prob_each)!=1){
      stop("If specified, the sum of prob_each must equal 1")
    }
    m_each <- floor(N*prob_each)
    remainder <- N - sum(m_each)
    m_each <- m_each + complete_ra(N=length(prob_each), m= remainder)
  } 
  
  if(is.null(m_each)){
    if(is.null(num_arms)){
      num_arms <- length(condition_names)
    }
    m_each <- rep(N%/%num_arms, num_arms)
    remainder <-  N%%num_arms
    m_each <- m_each + ifelse(1:num_arms %in% sample(1:num_arms,remainder),1,0)
  }
  
  if(is.null(num_arms)){
    num_arms <- length(m_each)
  }
  
  if(is.null(condition_names)){
    condition_names <- paste0("T", 1:num_arms)
  }
  
  if(N < num_arms){
    assign <- sample(condition_names, N, replace=FALSE)
    return(assign)
  }
  
  rand_order <- sample(1:N,replace = FALSE)
  assign <- rep(NA, N)
  for (i in 1:num_arms){
    assign[rand_order[(sum(m_each[0:(i-1)]) +1):sum(m_each[0:i]) ]] <- condition_names[i]
  }
  return(assign)
}

#' Simple Random Assignment
#'
#' This function conducts simple random assignment, a procedure in which units are assigned to treatment conditions with a known probability, but the number of units assigned to any condition might vary from one randomization to the next.  This function can accomodate any number of treatment arms.
#' @param N the total number of units in the experimental sample
#' @param prob if specified, a two-group design is assumed. prob is the probability of assignment to treatment.
#' @param num_arms the total number of treatment arms. If unspecified, will be determined from the length of prob_each or condition_names.
#' @param prob_each a numeric vector giving probabilites of assignment to each treatment group. Must sum to 1. If unspecified, equal probabilities will be assumed.
#' @param condition_names a character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc.
#' @keywords random assignment
#' @export
#' @examples
#' # Two Group Designs
#'
#' Z <- simple_ra(N=100)
#' table(Z)
#' 
#' Z <- simple_ra(N=100, prob=0.5)
#' table(Z)
#' 
#' Z <- simple_ra(N=100, prob_each = c(0.3, 0.7), 
#'                condition_names = c("control", "treatment"))
#' table(Z)
#' 
#' # Multi-arm Designs
#' Z <- simple_ra(N=100, num_arms=3)
#' table(Z)
#' 
#' Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4))
#' table(Z)
#' 
#' Z <- simple_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
#'                condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
#' Z <- simple_ra(N=100, condition_names=c("control", "placebo", "treatment"))
#' table(Z)
simple_ra <- function(N, prob=NULL, num_arms=NULL, prob_each=NULL, condition_names = NULL){
  
  if(!is.null(prob) & !is.null(condition_names)){
    stop("Do not specify prob and condition_names together. Use prob_each and condition_names instead.")
  }
  # Simple 2 group design, returns zeros and ones
  if(is.null(prob_each) & is.null(condition_names) & is.null(num_arms)){
    if(is.null(prob)){
      prob <- 0.5
    }
    if(prob > 1 | prob < 0){
      stop("The probability of assignment to treatment must be between 0 and 1.")
    }
    assign <- rbinom(n = N, size = 1, prob = prob)
    return(assign)
  }
  
  # All other types
  
  if(all(!is.null(prob_each),sum(prob_each) != 1)){
    stop("The sum of the probabilities of assignment to each condition (prob_each) must equal 1.")
  }
  if(all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != length(condition_names))){
    stop("The length of conditions_names must equal the length of prob_each")
  }
  if(all(!is.null(prob_each), !is.null(num_arms),length(prob_each) != num_arms)){
    stop("The number of arms (n_arms) must equal the length of prob_each")
  }
  if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
    stop("The length of conditions_names must equal the number of arms (n_arms)")
  }
  
  if(is.null(prob_each)){
    if(is.null(num_arms)){
      num_arms <- length(condition_names)
    }
    prob_each <- rep(1/num_arms, num_arms)
  }
  
  if(is.null(num_arms)){
    num_arms <- length(prob_each)
  }
  
  if(is.null(condition_names)){
    condition_names <- paste0("T", 1:num_arms)
  }
  
  assign <- sample(x = condition_names, size = N, replace = TRUE, prob = prob_each)
  return(assign) 
}

#' Block Random Assignment
#'
#' This function assigns a fixed number of units within each block to treatment.
#' @param block_var A vector of length N that includes the blocking variable
#' @param num_arms the total number of treatment arms. If unspecified, will be determined from the number of columns of block_m or the length of condition_names.
#' @param block_m A matrix whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified.
#' @param block_prob A vector whose length is equal to the number of treatment assignments. When specified, block_prob assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. block_prob must sum to 1.
#' @param condition_names a character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc.
#' @keywords random assignment
#' @export
#' @examples
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' Z <- block_ra(block_var=block_var)
#' table(block_var, Z)
#' 
#' block_m <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#' 
#' Z <- block_ra(block_var=block_var, block_m=block_m)
#' table(block_var, Z)
#' 
#' block_m <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' 
#' Z <- block_ra(block_var=block_var, block_m=block_m, 
#'               condition_names=c("control", "treatment"))
#' table(block_var, Z)
#' 
#' # Multi-arm Designs
#' Z <- block_ra(block_var=block_var, num_arms=3)
#' table(block_var, Z)
#' 
#' block_m <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' Z <- block_ra(block_var=block_var, block_m=block_m )
#' table(block_var, Z)
#' 
#' Z <- block_ra(block_var=block_var, block_m=block_m, 
#'               condition_names=c("control", "placebo", "treatment"))
#' table(block_var, Z)
#' 
#' Z <- block_ra(block_var=block_var, block_prob=c(.1, .1, .8))
#' table(block_var, Z)
block_ra <- function(block_var, num_arms= NULL, block_m=NULL, block_prob=NULL, condition_names = NULL){
  
  if(!is.null(block_m) & !is.null(block_prob)){
    stop("Do not specify both block_m and block_prob at the same time.")      
  }
  
  blocks <- sort(unique(block_var))
  assign <- rep(NA, length(block_var))
  
  if(is.null(block_m) & is.null(block_prob) & is.null(num_arms)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, condition_names=condition_names)
    }
    return(assign)
  }
  
  if(is.null(block_m) & is.null(block_prob) & !is.null(num_arms)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, num_arms=num_arms, condition_names=condition_names)
    }
    return(assign)
  }
  
  if(all(!is.null(num_arms), !is.null(block_prob), num_arms != length(block_prob))){
    stop("If both num_arms and block_prob are specified, num_arms must be equal to the length of block_prob")
  }
  
  if(all(!is.null(num_arms), !is.null(block_m), num_arms != ncol(block_m))){
    stop("If both num_arms and block_m are specified, num_arms must be equal to the number of columns of block_m")
  }
  
  if(!is.null(block_m)){
    for(i in 1:length(blocks)){
      if(nrow(block_m)!=length(unique(blocks))){
        stop("block_m should have the same number of rows as there are unique blocks in block_var")
      }
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, m_each = block_m[i,], condition_names=condition_names)
    }
    return(assign)
  }
  
  if(!is.null(block_prob)){
    
    for(i in 1:length(blocks)){
      if(sum(block_prob)!=1){
        stop("block_prob must sum to 1.")
      }
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, prob_each = block_prob, condition_names=condition_names)
    }
    return(assign)
  }
}

#' Cluster Random Assignment
#'
#' This function conducts complete random assignment by cluster.Clusters are collections of units that are assigned to a treatment together.
#' @param clust_var a vector of length N that describes which cluster each unit belongs to.
#' @param m the total number treatments to be allocated. Should only be specified for a two group design in which exactly m of N clusters is assigned to treatment. If not specified, half of the clusters will be assigned to treatment. Is null by default.
#' @param num_arms the total number of treatment arms. If unspecified, will be determined from the length of m_each or condition_names.
#' @param m_each a numeric vector giving the number of clusters to be assigned to each treatment group. Must sum to the total number of clusters. If unspecified, equally sized (rounded) groups will be assumed.
#' @param condition_names a character vector giving the names of the treatment groups.  If unspecified, the treatment groups will be names T1, T2, T3, etc.
#' @keywords random assignment
#' @export
#' @examples
#' # Two Group Designs
#' clust_var <- rep(letters, times=1:26)
#'
#' Z <- cluster_ra(clust_var=clust_var)
#' table(Z, clust_var)
#' 
#' Z <- cluster_ra(clust_var=clust_var, m=13)
#' table(Z, clust_var)
#' 
#' Z <- cluster_ra(clust_var=clust_var, m_each = c(10, 16), 
#'                 condition_names = c("control", "treatment"))
#' table(Z, clust_var)
#' 
#' # Multi-arm Designs
#' Z <- cluster_ra(clust_var=clust_var, num_arms=3)
#' table(Z, clust_var)
#' 
#' Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12))
#' table(Z, clust_var)
#' 
#' Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12), 
#'                 condition_names=c("control", "placebo", "treatment"))
#' table(Z, clust_var)
#' 
#' Z <- cluster_ra(clust_var=clust_var, 
#'                 condition_names=c("control", "placebo", "treatment"))
#' table(Z, clust_var)
cluster_ra <- function(clust_var, m=NULL, num_arms=NULL, m_each = NULL, condition_names = NULL){
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  z_clus <- complete_ra(N = n_clus, m = m, num_arms = num_arms, m_each = m_each, 
                        condition_names = condition_names)
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  y = data.frame(clust_var=unique_clus, z_clus), by="clust_var")
  merged <- merged[order(merged$init_order),]
  return(merged$z_clus)
}



