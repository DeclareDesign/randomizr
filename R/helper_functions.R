# randomizr input cleaning and helper functions

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
           condition_names = NULL) {
    conflict_args <- list(
      prob = prob,
      m = m,
      m_each = m_each,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob_each = block_prob_each
    )
    
    
    if(!is.null(clust_var)){
      N <- length(unique(clust_var))
    }
      
      
    specified_args <- !sapply(conflict_args, is.null)
    
    if (sum(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(conflict_args)[specified_args], collapse = " and "),
           ".")
    }
    
    
    
    if (!is.null(N)) {
      if (!(length(N) == 1 & (all.equal(N, as.integer(N))) & N > 0)) {
        stop("N must be an integer greater than 0")
      }
    }
    
    if (!is.null(prob)) {
      if (prob > 1 | prob < 0) {
        stop("The probability of assignment to treatment must be between 0 and 1.")
      }
    }
    
    if (!is.null(m)) {
      if (m < 0) {
        stop("If specified, the number of units assigned to treatment (m) must be nonnegative.")
      }
      if (m > N) {
        stop(
          "If specified, the number of units assigned to treatment (m) must not be greater than the total number of units (N)."
        )
      }
    }
    
    if (!is.null(prob_each)) {
      if (any(prob_each > 1 | prob_each < 0)) {
        stop(
          "The probabiltiies of assignment to any condition may not be greater than 1 or less than zero."
        )
      }
      if (sum(prob_each) != 1) {
        stop(
          "The sum of the probabilities of assignment to each condition (prob_each) must equal 1."
        )
      }
    }
    
    if (!is.null(m_each)) {
      if (any(m_each < 0)) {
        stop("The number of units assigned to all conditions must be nonnegative.")
      }
      if (sum(m_each) != N) {
        stop(
          "The sum of the number assigned to each condition (m_each) must equal the total number of units (N)"
        )
      }
    }
    
    # Lengths
    
    if (!is.null(condition_names)) {
      if (!is.null(m)) {
        if (length(condition_names) != 2) {
          stop(
            "If m and condition_names are specified together, condition_names must be of length 2."
          )
        }
      }
      if (!is.null(prob)) {
        if (length(condition_names) != 2) {
          stop(
            "If prob and condition_names are specified together, condition_names must be of length 2."
          )
        }
      }
      if (!is.null(prob_each)) {
        if (length(prob_each) != length(condition_names)) {
          stop(
            "If prob_each and condition_names are specified together, they must be of the same length."
          )
        }
      }
      if (!is.null(m_each)) {
        if (length(m_each) != length(condition_names)) {
          stop(
            "If m_each and condition_names are specified together, they must be of the same length."
          )
        }
      }
      if (!is.null(block_m_each)) {
        if (ncol(block_m_each) != length(condition_names)) {
          stop(
            "If both condition_names and block_m_each are specified, the length of condition_names must be equal to the number of columns of block_m_each."
          )
        }
      }
      if (!is.null(block_prob_each)) {
        if (ncol(block_prob_each) != length(condition_names)) {
          stop(
            "If both condition_names and block_prob_each are specified, the length of condition_names must be equal to the number of columns of block_prob_each"
          )
        }
      }
      if (!is.null(num_arms)) {
        if (num_arms != length(condition_names)) {
          stop(
            "If both condition_names and num_arms are specified, the length of condition_names must be equal to num_arms."
          )
        }
      }
    }
    
    if (!is.null(num_arms)) {
      if (!is.null(m)) {
        if (num_arms != 2) {
          stop("If m and num_arms are specified together, num_arms must be 2.")
        }
      }
      if (!is.null(prob)) {
        if (num_arms != 2) {
          stop("If prob and num_arms are specified together, num_arms must be 2.")
        }
      }
      if (!is.null(prob_each)) {
        if (length(prob_each) != num_arms) {
          stop(
            "If prob_each and num_arms are specified together, the length of prob_each must be equal to num_arms."
          )
        }
      }
      if (!is.null(m_each)) {
        if (length(m_each) != num_arms) {
          stop(
            "If m_each and num_arms are specified together, the length of m_each must be equal to num_arms."
          )
        }
      }
      if (!is.null(block_m_each)) {
        if (ncol(block_m_each) != num_arms) {
          stop(
            "If both num_arms and block_m_each are specified, num_arms must be equal to the number of columns of block_m_each."
          )
        }
      }
      if (!is.null(block_prob_each)) {
        if (ncol(block_prob_each) != num_arms) {
          stop(
            "If both num_arms and block_prob_each are specified, num_arms must be equal to the number of columns of block_prob_each"
          )
        }
      }
    }
    
    
    
    # Blocked Design Checks
    N_per_block <- NULL
    if (!is.null(block_var)) {
      N_per_block <- as.numeric(table(block_var))
      N_blocks <- length(N_per_block)
      
      if (!is.null(block_m)) {
        if (length(block_m) != N_blocks) {
          stop(
            "If specified, block_m should have the same lengths as there are unique blocks in block_var."
          )
        }
      }
      
      if (!is.null(block_m_each)) {
        if (nrow(block_m_each) != N_blocks) {
          stop(
            "If specified, block_m_each should have the same number of rows as there are unique blocks in block_var."
          )
        }
        if (is.null(clust_var) &
            !all(apply(block_m_each, 1, sum) == N_per_block)) {
          stop(
            "If specified, each row of block_m_each must sum to the number of units in the corresponding block."
          )
        }
        
      }
      
      if (!is.null(block_prob_each)) {
        if (nrow(block_prob_each) != N_blocks) {
          stop(
            "If specified, block_prob_each should have the same number of rows as there are unique blocks in block_var"
          )
        }
        if (is.null(clust_var) &
            any(apply(block_prob_each, 1, sum) != 1)) {
          stop("If specified, each row of block_prob_each must sum to 1.")
        }
      }
      
      if (!is.null(clust_var)) {
        if (!all(rowSums(table(clust_var, block_var) != 0) == 1)) {
          stop("All units within a cluster must be in the same block.")
        }
      }
      
    }
    
    # learn about design
    
    # obtain num_arms
    
    if (is.null(num_arms)) {
      num_arms <- 2
      if (!is.null(m_each)) {
        num_arms <- length(m_each)
      }
      if (!is.null(block_m)) {
        num_arms <- 2
      }
      if (!is.null(block_m_each)) {
        num_arms <- ncol(block_m_each)
      }
      if (!is.null(prob_each)) {
        num_arms <- length(prob_each)
      }
      if (!is.null(block_prob_each)) {
        num_arms <- ncol(block_prob_each)
      }
      if (!is.null(condition_names)) {
        num_arms <- length(condition_names)
      }
      num_arms_was_null <- TRUE
    } else{
      num_arms_was_null <- FALSE
    }
    
    # obtain condition_names
    if (is.null(condition_names)) {
      if (num_arms == 2 & num_arms_was_null) {
        condition_names = c(0, 1)
      } else{
        condition_names <- paste0("T", 1:num_arms)
      }
    }
    
    return(
      list(
        num_arms = num_arms,
        condition_names = condition_names,
        N_per_block = N_per_block
      )
    )
    
  }


clean_condition_names <- function(assign, condition_names) {
  if (all(assign %in% c(0, 1))) {
    return(as.numeric(assign))
  } else{
    #assign <- condition_names[assign]
    #if(!identical(condition_names, c(0,1))){
    #  assign <- factor(assign, levels = condition_names)
    #}
    
    return(factor(assign, levels = condition_names))
  }
}
