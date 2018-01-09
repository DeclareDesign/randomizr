# randomizr input cleaning and helper functions

warn_deprecated_args <- function(block_var=NULL, clust_var=NULL, strata_var=NULL) {
  if(!is.null(block_var)){warning(simpleCondition("block_var is deprecated, use blocks instead", call=sys.call(1)))}
  if(!is.null(clust_var)){warning(simpleCondition("clust_var is deprecated, use clusters instead", call=sys.call(1)))}
  if(!is.null(strata_var)){warning(simpleCondition("strata_var is deprecated, use strata instead", call=sys.call(1)))}
}

check_randomizr_arguments <-
  function(N = NULL,
           prob = NULL,
           m = NULL,
           m_each = NULL,
           prob_each = NULL,
           blocks = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           clusters = NULL,
           num_arms = NULL,
           condition_names = NULL) {
    conflict_args <- list(
      prob = prob,
      m = m,
      m_each = m_each,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each
    )
    
    
    if (!is.null(clusters)) {
      N <- length(unique(clusters))
    }
    
    
    specified_args <- !sapply(conflict_args, is.null)
    
    if (sum(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(conflict_args)[specified_args], collapse = " and "),
           ".")
    }
    
    
    
    if (!is.null(N)) {
      if (!(length(N) == 1 &
            (isTRUE(all.equal(
              N, as.integer(N)
            ))) & N > 0)) {
        stop("N must be an integer greater than 0")
      }
    }
    
    if (!is.null(prob)) {
      if (prob > 1 | prob < 0) {
        stop("The probability of assignment to treatment must be between 0 and 1.")
      }
    }
    
    if (!is.null(block_prob)) {
      if (any(block_prob > 1 | block_prob < 0)) {
        stop("The probabilities of assignment to treatment must be between 0 and 1 for all blocks.")
      }
    }
    
    if (!is.null(m) & is.null(blocks)) {
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
      if (!isTRUE(all.equal(sum(prob_each), 1))) {
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
      if (length(unique(condition_names)) != length(condition_names)) {
        stop("You must supply unique values to condition_names.")
      }
      if (!is.null(m)) {
        if (length(condition_names) != 2) {
          stop(
            "If m and condition_names are specified together, condition_names must be of length 2."
          )
        }
      }
      if (!is.null(block_m)) {
        if (length(condition_names) != 2) {
          stop(
            "If block_m and condition_names are specified together, condition_names must be of length 2."
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
      if (!is.null(block_prob)) {
        if (length(condition_names) != 2) {
          stop(
            "If block_prob and condition_names are specified together, condition_names must be of length 2."
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
      if (!is.null(block_m)) {
        if (num_arms != 2) {
          stop("If block_m and num_arms are specified together, num_arms must be 2.")
        }
      }
      if (!is.null(prob)) {
        if (num_arms != 2) {
          stop("If prob and num_arms are specified together, num_arms must be 2.")
        }
      }
      
      if (!is.null(block_prob)) {
        if (num_arms != 2) {
          stop("If block_prob and num_arms are specified together, num_arms must be 2.")
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
    if (!is.null(blocks)) {
      N_per_block <- tapply(blocks, blocks, length)
      attributes(N_per_block) <- NULL
      N_blocks <- length(N_per_block)
      
      if (!is.null(block_m)) {
        if (length(block_m) != N_blocks) {
          stop(
            "If specified, block_m should have the same length as there are unique blocks in blocks."
          )
        }
      }
      if (!is.null(block_prob)) {
        if (length(block_prob) != N_blocks) {
          stop(
            "If specified, block_prob should have the same length as there are unique blocks in blocks."
          )
        }
      }
      
      if(!is.null(m) & !is.null(blocks)){
        block_m <- rep(m, length(N_per_block))
      }
      
      if (!is.null(block_m)) {
        if (any(block_m > N_per_block | block_m < 0)) {
          stop(
            "The number of units assigned to treatment within a block must be nonnegative and not exceed the total number units within the block."
          )
        }
      }
      
      
      if (!is.null(block_m_each)) {
        if (nrow(block_m_each) != N_blocks) {
          stop(
            "If specified, block_m_each should have the same number of rows as there are unique blocks in blocks."
          )
        }
        if (is.null(clusters) &
            !all(apply(block_m_each, 1, sum) == N_per_block)) {
          stop(
            "If specified, each row of block_m_each must sum to the number of units in the corresponding block."
          )
        }
        
      }
      
      if (!is.null(block_prob_each)) {
        if (nrow(block_prob_each) != N_blocks) {
          stop(
            "If specified, block_prob_each should have the same number of rows as there are unique blocks in blocks"
          )
        }
        if (is.null(clusters) &
            !isTRUE(all.equal(apply(block_prob_each, 1, sum), rep(1, N_blocks), check.attributes = FALSE))) {
          stop("If specified, each row of block_prob_each must sum to 1.")
        }
      }
      
      if (!is.null(clusters)) {
        if (!all(tapply(blocks, clusters, function(x)
          all(x == x[1])))) {
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

check_samplr_arguments <-
  function(N = NULL,
           prob = NULL,
           n = NULL,
           strata = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           clusters = NULL) {
    conflict_args <- list(
      prob = prob,
      n = n,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
    
    if (!is.null(clusters)) {
      N <- length(unique(clusters))
    }
    
    specified_args <- !sapply(conflict_args, is.null)
    
    if (sum(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(conflict_args)[specified_args], collapse = " and "),
           ".")
    }
    
    
    if (!is.null(N)) {
      if (!(length(N) == 1 &
            (isTRUE(all.equal(
              N, as.integer(N)
            ))) & N > 0)) {
        stop("N must be an integer greater than 0")
      }
    }
    
    if (!is.null(prob)) {
      if (prob > 1 | prob < 0) {
        stop("The probability of being sampled must be between 0 and 1.")
      }
    }
    
    if (!is.null(strata_prob)) {
      if (any(strata_prob > 1 | strata_prob < 0)) {
        stop("The probabilities of being sampled must be between 0 and 1 for all strata.")
      }
    }
    
    if (!is.null(n) & is.null(strata)) {
      if (n < 0) {
        stop("If specified, the number of units sampled (n) must be nonnegative.")
      }
      if (n > N) {
        stop(
          "If specified, the number of units sampled (n) must not be greater than the total number of units (N)."
        )
      }
    }
    
    # stratified Design Checks
    N_per_stratum <- NULL
    if (!is.null(strata)) {
      N_per_stratum <- tapply(strata, strata, length)
      attributes(N_per_stratum) <- NULL
      N_strata <- length(N_per_stratum)
      
      if (!is.null(strata_n)) {
        if (length(strata_n) != N_strata) {
          stop(
            "If specified, strata_n should have the same length as there are unique strata in strata."
          )
        }
      }
      if (!is.null(strata_prob)) {
        if (length(strata_prob) != N_strata) {
          stop(
            "If specified, strata_prob should have the same length as there are unique strata in strata."
          )
        }
      }
      
      if (!is.null(strata_n)) {
        if (any(strata_n > N_per_stratum | strata_n < 0)) {
          stop(
            "The number of units sampled within a stratum must be nonnegative and not exceed the total number units within the strata."
          )
        }
      }
      
      if (!is.null(clusters)) {
        if (!all(tapply(strata, clusters, function(x)
          all(x == x[1])))) {
          stop("All units within a cluster must be in the same stratum.")
        }
      }
      
    }
    
    return(list(
      num_arms = 2,
      condition_names =  c(0, 1),
      N_per_stratum = N_per_stratum
    ))
    
  }



clean_condition_names <- function(assignment, condition_names) {
  
  if(is.factor(condition_names)){
    return(factor(assignment, levels = levels(condition_names)))
  }
  
  if (is.numeric(assignment)) {
    return(assignment)
  }
  
  if (is.logical(assignment)) {
    return(as.numeric(assignment))
  }    
  
  return(factor(assignment, levels = condition_names))
}
