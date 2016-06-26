#' Block Random Assignment
#'
#' Random assignment where complete random assignment is employed within experimental blocks. For example, imagine that 50 of 100 men are assigned to treatment and 75 of 200 women are assigned to treatment.
#' 
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m_each or block_prob_each, the length of prob_each, or the length of condition_names.
#' @param prob If specified, a two-group design is assumed. prob is the probability of assignment to treatment. Within rounding, N*prob subjects within each block will be assigned to treatment.
#' @param block_m_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units to be assigned to each treatment arm. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_m Deprecated. Use block_m_each instead.
#' @param prob_each A numeric vector giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use block_m_each.
#' @param block_prob_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within block. Use only if the probabilities of assignment should vary by block. Each row of block_prob_each must sum to 1.
#' @param condition_names A character vector giving the names of the treatment conditions. If unspecified, the treatment conditions will be named T1, T2, T3, etc.
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
block_ra <- function(block_var, num_arms= NULL, prob = NULL,
                     block_m=NULL, block_m_each = NULL, 
                     prob_each=NULL, block_prob_each = NULL, condition_names = NULL){
  
  check_inputs <- check_randomizr_arguments(block_var = block_var, num_arms = num_arms, prob = prob,
                                            block_m = block_m, block_m_each = block_m_each,
                                            prob_each = prob_each, block_prob_each = block_prob_each,
                                            condition_names = condition_names)
  
  blocks <- sort(unique(block_var))
  assign <- rep(NA, length(block_var))
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  # Setup: obtain number of arms and condition_names
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  
  # Case 1 use or infer prob_each
  if(is.null(block_m_each) & is.null(block_prob_each)){
    
    if(is.null(prob_each)){
      prob_each <- rep(1/num_arms, num_arms)
    }
    
    # Get the baseline allocation
    block_m_each <- floor(table(block_var) %*% t(prob_each))
    
    # Figure out the marginals
    n_unassigned <- length(block_var) - sum(block_m_each)
    
    if(sum(n_unassigned) >0){
      fixed_column_margin <- as.numeric(table(complete_ra(n_unassigned, num_arms = num_arms)))
      fixed_row_margin <- table(block_var) - rowSums(block_m_each)
      block_m_each <- update_block_m_each(block_m_each, fixed_row_margin, fixed_column_margin)
    }    
    
    assign <- block_ra(block_var = block_var, block_m_each = block_m_each, condition_names=condition_names)
    return(assign)
  }
  
  # Case 2 use block_m_each
  
  if(!is.null(block_m_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, m_each = block_m_each[i,], condition_names=condition_names)
    }
    if(!identical(condition_names, c(0,1))){
      assign <- condition_names[assign]
    }
    assign <- clean_condition_names(assign, condition_names)
    return(assign)
  }
  
  # Case 3 use block_prob_each
  
  if(!is.null(block_prob_each)){
    
    # Get the baseline allocation
    block_m_each <- floor(sweep(block_prob_each, 1, table(block_var), `*`))
    
    # Figure out the marginals
    n_unassigned <- length(block_var) - sum(block_m_each)
    
    if(sum(n_unassigned) >0){
      fixed_column_margin <- as.numeric(table(complete_ra(n_unassigned, num_arms = num_arms)))
      fixed_row_margin <- table(block_var) - rowSums(block_m_each)
      block_m_each <- update_block_m_each(block_m_each, fixed_row_margin, fixed_column_margin)
    }  
    
    assign <- block_ra(block_var = block_var, block_m_each = block_m_each, condition_names=condition_names)
    return(assign)
  }
}


update_block_m_each <- 
  function(block_m_each, fixed_row_margin, fixed_column_margin){
    # Gather some unique possibilities that only have 1's and 0's
    condition <- FALSE
    while(!condition){
      draw <- r2dtable(n = 1, r = fixed_row_margin, c = fixed_column_margin)[[1]]
      condition <- all(draw %in% c(0,1))
    }
    return(block_m_each + draw)
  }