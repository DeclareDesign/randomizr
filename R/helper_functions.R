# randomizr input cleaning and helper functions

#'@export
check_randomizr_arguments <- 
  function(N = NULL, 
           prob = NULL, 
           m = NULL, 
           m_each = NULL,
           prob_each = NULL, 
           block_var = NULL, 
           block_m = NULL,
           block_m_each = NULL,
           block_prob_each = NULL,
           clust_var = NULL, 
           num_arms = NULL, 
           condition_names = NULL){
    
    # Checks
    if(!is.null(block_m)){
      warning("Use of block_m is deprecated. Use block_m_each instead.")
      if(!is.null(block_m_each)){
        block_m_each <- block_m
      }
    }
    
    if(!is.null(N)){
      if(!(length(N) == 1 & (all.equal(N, as.integer(N))) & N > 0)){
        stop("N must be an integer greater than 0")
      }
    }
    
    if(!is.null(prob)){
      if(prob > 1 | prob < 0){
        stop("The probability of assignment to treatment must be between 0 and 1.")
      }
      if(!is.null(condition_names)){
        stop("Do not specify prob and condition_names together. Use prob_each and condition_names instead.")
      }
    }
    
    if(!is.null(m)){
      if(m < 0){
        stop("If specified, the number of units assigned to treatment (m) must be nonnegative.")
      }
      if(m > N){
        stop("If specified, the number of units assigned to treatment (m) must not be greater than the total number of units (N).")
      }
      if(!is.null(condition_names)){
        stop("Do not specify m and condition_names together. Use m_each and condition_names instead.")
      }
    }
    
    if(!is.null(prob_each)){
      if(any(prob_each > 1 | prob_each < 0)){
        stop("The probabiltiies of assignment to any condition may not be greater than 1 or less than zero.")
      }
      
      if(sum(prob_each) != 1){
        stop("The sum of the probabilities of assignment to each condition (prob_each) must equal 1.")
      }
    }
    if(!is.null(m_each)){
      if(any(m_each < 0)){
        stop("The number of units assigned to all conditions must be nonnegative.")
      }
      if(sum(m_each) != N){
        stop("The sum of the number assigned to each condition (m_each) must equal the total number of units (N)")
      }
    }
    
    # should we allow 1 arm trials?  
    #if(!is.null(num_arms)){
    #  if(!num_arms > 1){
    #    stop("The number of arms (specified with num_arms) must be greater than one.")
    #  }
    #}
    # if(!is.null(condition_names) & length(condition_names) ==1){
    #   stop("The number of arms (as inferred from the length of condition_names) must be greater than one.")
    # }
    # if(!is.null(m_each) & length(m_each) ==1){
    #   stop("The number of arms (as inferred from the length of m_each) must be greater than one.")
    # }
    # if(!is.null(prob_each) & length(prob_each) ==1){
    #   stop("The number of arms (as inferred from the length of prob_each) must be greater than one.")
    # }
    
    if(all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != length(condition_names))){
      stop("The length of conditions_names must equal the length of prob_each")
    }
    if(all(!is.null(prob_each), !is.null(num_arms), length(prob_each) != num_arms)){
      stop("The number of arms (num_arms) must equal the length of prob_each")
    }
    if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
      stop("The length of conditions_names must equal the number of arms (num_arms)")
    }
    if(all(!is.null(condition_names), !is.null(m_each), length(m_each) != length(condition_names))){
      stop("If both condition_names and m_each are specified, the length of condition_names must be equal to the length of m_each")
    }
    if(all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != length(condition_names))){
      stop("If both condition_names and prob_each are specified, the length of condition_names must be equal to the length of prob_each.")
    }
    if(all(!is.null(m_each), !is.null(num_arms),length(m_each) != num_arms)){
      stop("The number of arms (num_arms) must equal the length of m_each")
    }
    if(all(!is.null(prob_each), !is.null(num_arms),length(prob_each) != num_arms)){
      stop("The number of arms (num_arms) must equal the length of prob_each")
    }
    if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
      stop("If both condition_names and num_arms are specified, the length of condition_names must be equal to num_arms.")
    }
    
    if(all(!is.null(num_arms), !is.null(block_m_each), num_arms != ncol(block_m_each))){
      stop("If both num_arms and block_m_each are specified, num_arms must be equal to the number of columns of block_m_each.")
    }
    if(all(!is.null(num_arms), !is.null(block_prob_each), num_arms != ncol(block_prob_each))){
      stop("If both num_arms and block_prob_each are specified, num_arms must be equal to the number of columns of block_prob_each.")
    }
    
    if(all(!is.null(condition_names), !is.null(block_m_each), length(condition_names) != ncol(block_m_each))){
      stop("If both condition_names and block_m_each are specified, the length of condition_names must be equal to the number of columns of block_m_each.")
    }
    
    if(all(!is.null(condition_names), !is.null(block_prob_each), length(condition_names) != ncol(block_prob_each))){
      stop("If both condition_names and block_prob_each are specified, the length of condition_names must be equal to the number of columns of block_prob_each.")
    }
    
    
    
    
    if(!is.null(prob_each) & !is.null(m_each)){
      stop("Do not specify prob_each and m_each together. Use one or the other.")
    }
    
    if(sum(!is.null(block_m_each), !is.null(prob_each), !is.null(block_prob_each)) >1){
      stop("Please specify only one of block_m_each, prob_each, block_prob_each.")      
    }
    
    if(!is.null(block_var)){
      N_blocks = length(unique(block_var))
      
      if(!is.null(clust_var)){
        if(!all(rowSums(table(clust_var, block_var) != 0)==1)){
          stop("All units within a cluster must be in the same block.")
        }
      }
    }
    
    if(!is.null(block_m_each)){
      if(nrow(block_m_each) != N_blocks){
        stop("If specified, block_m_each should have the same number of rows as there are unique blocks in block_var")
      }
    }
    
    if(!is.null(block_prob_each)){
      if(nrow(block_prob_each) != N_blocks){
        stop("If specified, block_prob_each should have the same number of rows as there are unique blocks in block_var")
      }
      if(any(apply(block_prob_each, 1, sum) !=1)){
        stop("All rows of block_prob_each must sum to 1.")
      }
    }
    
    
    # learn about design
    
    # obtain num_arms
    
    if(is.null(num_arms)){
      num_arms <- 2
      if(!is.null(m_each)){num_arms <- length(m_each)}
      if(!is.null(block_m_each)){num_arms <- ncol(block_m_each)}
      if(!is.null(prob_each)){num_arms <- length(prob_each)}
      if(!is.null(block_prob_each)){num_arms <- ncol(block_prob_each)}
      if(!is.null(condition_names)){num_arms <- length(condition_names)}
      num_arms_was_null <- TRUE
    }else{
      num_arms_was_null <- FALSE
    }
    
    # obtain condition_names
    if(is.null(condition_names)){
      if(num_arms==2 & num_arms_was_null){
        condition_names = c(0,1)
      }else{
        condition_names <- paste0("T", 1:num_arms)    
      }
    }
    
    return(list(num_arms = num_arms, condition_names = condition_names))
    
  }


clean_condition_names <- function(assign, condition_names){
  if(all(assign %in% c(0,1))){
    return(as.numeric(assign))
  }else{
    #assign <- condition_names[assign]
    #if(!identical(condition_names, c(0,1))){
    #  assign <- factor(assign, levels = condition_names)
    #}
    
    return(factor(assign, levels = condition_names))
  }
}

