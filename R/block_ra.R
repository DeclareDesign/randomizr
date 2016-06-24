#' Block Random Assignment
#'
#' Random assignment where complete random assignment is employed within experimental blocks. For example, imagine that 50 of 100 men are assigned to treatment and 75 of 200 women are assigned to treatment.
#' 
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m_each or block_prob_each, the length of prob_each, or the length of condition_names.
#' @param block_m_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units to be assigned to each treatment arm. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_m Deprecated. Use block_m_each instead.
#' @param prob_each A numeric vector giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use block_m_each.
#' @param block_prob_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within block. Use only if the probabilities of assignment should vary by block. Each row of block_prob_each must sum to 1.
#' @param condition_names A character vector giving the names of the treatment conditions. If unspecified, the treatment conditions will be named T1, T2, T3, etc.
#' @param remainder_draws Number of random remainder contigency tables to draw. Defaults to 100. You may need to increase in some cases.
#' @return A vector of length N that indicates the treatment condition of each unit.
#' @export
#' @examples
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' Z <- block_ra(block_var=block_var)
#' table(block_var, Z)
#' 
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#' 
#' Z <- block_ra(block_var=block_var, block_m_each=block_m_each)
#' table(block_var, Z)
#' 
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' 
#' Z <- block_ra(block_var=block_var, block_m_each=block_m_each, 
#'               condition_names=c("control", "treatment"))
#' table(block_var, Z)
#' 
#' # Multi-arm Designs
#' Z <- block_ra(block_var=block_var, num_arms=3)
#' table(block_var, Z)
#' 
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' Z <- block_ra(block_var=block_var, block_m_each=block_m_each )
#' table(block_var, Z)
#' 
#' Z <- block_ra(block_var=block_var, block_m_each=block_m_each, 
#'               condition_names=c("control", "placebo", "treatment"))
#' table(block_var, Z)
#' 
#' Z <- block_ra(block_var=block_var, prob_each=c(.1, .1, .8))
#' table(block_var, Z)
block_ra <- function(block_var, num_arms= NULL, 
                     block_m=NULL, block_m_each = NULL, 
                     prob_each=NULL, block_prob_each = NULL, condition_names = NULL,remainder_draws = 100){
  
  if(!is.null(block_m)){
    warning("Use of block_m is deprecated. Use block_m_each instead.")
    if(!is.null(block_m_each)){
      block_m_each <- block_m
    }
  }
  
  if(sum(!is.null(block_m_each), !is.null(prob_each), !is.null(block_prob_each)) >1){
    stop("Please specify only one of block_m_each, prob_each, block_prob_each.")      
  }
  
  if(all(!is.null(num_arms), !is.null(prob_each), num_arms != length(prob_each))){
    stop("If both num_arms and prob_each are specified, num_arms must be equal to the length of prob_each.")
  }
  
  if(all(!is.null(num_arms), !is.null(block_m_each), num_arms != ncol(block_m_each))){
    stop("If both num_arms and block_m_each are specified, num_arms must be equal to the number of columns of block_m_each.")
  }
  
  if(all(!is.null(num_arms), !is.null(block_prob_each), num_arms != ncol(block_prob_each))){
    stop("If both num_arms and block_prob_each are specified, num_arms must be equal to the number of columns of block_prob_each.")
  }
  
  if(all(!is.null(condition_names), !is.null(prob_each), length(condition_names) != length(prob_each))){
    stop("If both condition_names and prob_each are specified, the length of condition_names must be equal to the length of prob_each.")
  }
  
  if(all(!is.null(condition_names), !is.null(block_m_each), length(condition_names) != ncol(block_m_each))){
    stop("If both condition_names and block_m_each are specified, the length of condition_names must be equal to the number of columns of block_m_each.")
  }
  
  if(all(!is.null(condition_names), !is.null(block_prob_each), length(condition_names) != ncol(block_prob_each))){
    stop("If both condition_names and block_prob_each are specified, the length of condition_names must be equal to the number of columns of block_prob_each.")
  }
  
  if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
    stop("If both condition_names and num_arms are specified, the length of condition_names must be equal to num_arms.")
  }
  
  blocks <- sort(unique(block_var))
  assign <- rep(NA, length(block_var))
  
  if(!is.null(block_m_each)){
    if(nrow(block_m_each) != length(unique(blocks))){
      stop("block_m_each should have the same number of rows as there are unique blocks in block_var")
    }
  }
  
  if(!is.null(block_prob_each)){
    if(nrow(block_prob_each) != length(unique(blocks))){
      stop("block_prob_each should have the same number of rows as there are unique blocks in block_var")
    }
    if(any(apply(block_prob_each, 1, sum) !=1)){
      stop("All rows of block_prob_each must sum to 1.")
    }
  }
  
  if(!is.null(prob_each)){
    if(sum(prob_each)!=1){
      stop("prob_each must sum to 1.")
    }
  }
  # Setup: obtain number of arms and condition_names
  
  if(is.null(num_arms)){
    num_arms <- 2
    if(!is.null(block_m_each)){num_arms <- ncol(block_m_each)}
    if(!is.null(prob_each)){num_arms <- length(prob_each)}
    if(!is.null(block_prob_each)){num_arms <- ncol(block_prob_each)}
    if(!is.null(condition_names)){num_arms <- length(condition_names)}
  }
  
  if(is.null(condition_names)){
    if(num_arms==2){
      condition_names = c(0,1)
    }else{
      condition_names <- paste0("T", 1:num_arms)    
    }
  }
  
  
  if(is.null(block_m_each) & is.null(block_prob_each)){
    
    if(is.null(prob_each)){
      prob_each <- rep(1/num_arms, num_arms)
    }
    
    # Get the baseline allocation
    block_m_each <- floor(table(block_var) %*% t(prob_each))
    
    # Figure out the marginals
    m_each <- floor(length(block_var)*prob_each)
    m_remainder <- length(block_var) - sum(m_each)
    m_each <- m_each + complete_ra(N=length(prob_each), m = m_remainder)
    
    fixed_column_margin <- m_each - colSums(block_m_each) 
    fixed_row_margin <- table(block_var) - rowSums(block_m_each)
    
    if(sum(fixed_column_margin)>0  & sum(fixed_row_margin) >0){
    
      # Gather some unique possibilities that only have 1's and 0's
      possibilities <- r2dtable(n = remainder_draws, r = fixed_row_margin, c = fixed_column_margin)
      possibilities <- unique(possibilities)
      possibilities <- possibilities[sapply(possibilities, FUN = function(x) all(x %in% c(0,1)))]
      which_possibility <- sample(1:length(possibilities), 1)
      
      #
      block_m_each <- block_m_each + possibilities[[which_possibility]]
    }
    
    assign <- block_ra(block_var = block_var, block_m_each = block_m_each)
    return(assign)
  }
  
  if(!is.null(block_m_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, m_each = block_m_each[i,], condition_names=condition_names)
    }
    assign <- condition_names[assign]
    if(!identical(condition_names, c(0,1))){
      assign <- factor(assign, levels = condition_names)
    }
    return(assign)
  }
  
  if(!is.null(block_prob_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, prob_each = block_prob_each[i,], condition_names=condition_names)
    }
    assign <- condition_names[assign]
    assign <- factor(assign, levels = condition_names)
    return(assign)
  }
  
  # Could Cut  
  # if(!is.null(prob_each)){
  #   for(i in 1:length(blocks)){
  #     N_block <- sum(block_var==blocks[i])
  #     assign[block_var==blocks[i]] <- complete_ra(N = N_block, prob_each = prob_each, condition_names=condition_names)
  #   }
  #   assign <- condition_names[assign]
  #   assign <- factor(assign, levels = condition_names)
  #   return(assign)
  # }
  
}
