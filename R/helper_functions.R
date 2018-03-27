
.invoke_check <- function(check){
  definition <- sys.function(sys.parent())
  envir <- parent.frame(1)

  all_args <- mget(names(formals(definition)), envir)  
  ret <- check(all_args)
  list2env(ret, envir)
  invisible(NULL)
}

#f <- function(N,n) {.invoke_check(function(a) list(n = a[["n"]] + 1)); n}



check_randomizr_arguments_new <- function(all_args) do.call("check_randomizr_arguments", all_args)

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
           conditions = NULL, ...) {

    # N, blocks, clusters, num_arms, conditions are used generally, check them first
    
    if (!is.null(clusters)) {
      N <- length(unique(clusters))
    }

    if (!is.null(blocks)) {
      N_per_block <- tapply(blocks, blocks, length)
      attributes(N_per_block) <- NULL
      N_blocks <- length(N_per_block)

      attributes(blocks) <- list(N_per_block=N_per_block, N_blocks=N_blocks)
      
      if (!is.null(clusters)) {
        # N would be set by clusters check abovew
        if (any(colSums(table(blocks, clusters) > 0) > 1)) {
          stop("All units within a cluster must be in the same block.")
        }
      } else {
        if(is.null(N)) {
          N <- sum(N_per_block) 
        } else if (N != sum(N_per_block)) {
          stop("N should equal the length of blocks")
        }
      }
    }
    
    if(is.null(N)){
      stop("N, blocks or clusters must be specified.")
    }
    
    if (length(N) != 1 || N != floor(N) || N <= 0) {
      stop("N must be a positive integer scalar.")
    }

    if (anyDuplicated(conditions)) {
      stop("You must supply unique values to conditions.")
    }
    
    if (!is.null(num_arms) && !is.null(conditions) &&  num_arms != length(conditions)) {
      stop(
        "If both conditions and num_arms are specified, the length of conditions must be equal to num_arms."
      )
    }
    
    # Each of these should be a unique specifier, consistent with above general args
    conflict_args <- c("prob",             "prob_each",        "m",       "m_each",
                       "block_prob", "block_prob_each",  "block_m", "block_m_each")
    specified_args <- Filter(Negate(is.null), mget(conflict_args))
    
    if (length(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(specified_args), collapse = " and "),
           ".")
    } else if (length(specified_args) == 1){
      arg <- names(specified_args)
      arg_block <- grepl("^block_", arg)
      arg_each  <- grepl("_each$", arg)
      
      if(arg_block && is.null(blocks)) stop("Specified ", arg, "but blocks is NULL.")

      .check_ra_arg_num_arms_conditions(arg, arg_block, arg_each, specified_args[[1]], num_arms, conditions)  

      .check_ra[[arg]](N, blocks, clusters, num_arms, conditions, specified_args[[1]])
    }


    
    # learn about design
    
    # obtain num_arms
    
    if (is.null(num_arms)) {
      
      num_arms <- if(!is.null(conditions)) length(conditions) 
        else if (length(specified_args) == 0) 2
        else if (!arg_each) 2
        else if (!arg_block) length(specified_args[[1]]) 
        else ncol(specified_args[[1]]) 

      if(num_arms == 2 && is.null(conditions))
        conditions <- 0:1
    } 
    
    # obtain conditions, wasn't set by num_arms guess
    if (is.null(conditions)) {
        conditions <- paste0("T", 1:num_arms)
    }
    
    return(
      list(
        num_arms = num_arms,
        conditions = conditions,
        condition_names = conditions,
        N_per_block = get0("N_per_block")
      )
    )
    
  }

.check_ra <- new.env(parent=emptyenv())

.check_ra_arg_num_arms_conditions <- function(arg, arg_block, arg_each, value, num_arms, conditions){
  
  if (!arg_each) {
    w <- 2
    num_arms_fmt <- "If %s and num_arms are both specified, num_arms must be 2."
    conditions_fmt <- "If %s and conditions are both specified, conditions must be length 2."
  } else if (!arg_block) {
    w <- length(value)
    num_arms_fmt <- "If %s and num_arms are both specified, the length of %s must be equal to num_arms."
    conditions_fmt <- "If %s and conditions are both specified, they must be the same length."
  } else {
    w <- ncol(value)
    num_arms_fmt <- "If %s and num_arms are both specified, the number of columns of %s must be equal to num_arms."
    conditions_fmt <- "If %s and conditions are both specified, the length of conditions must be equal to the number of columns of %s."
  }
  
  if (!is.null(num_arms) && num_arms != w) {
    stop(sprintf(num_arms_fmt, arg, arg))
  }
  
  if (!is.null(conditions) && length(conditions) != w) {
    stop(sprintf(conditions_fmt, arg, arg))
  }
}

.check_ra$prob <- function(N, blocks, clusters, num_arms, conditions, prob) {
  if (prob > 1 | prob < 0) {
    stop("The probability of assignment to treatment must be between 0 and 1.")
  }
}

.check_ra$prob_each <- function(N, blocks, clusters, num_arms, conditions, prob_each) {
    
  if (any(prob_each > 1 | prob_each < 0)) {
    stop(
      "The probabilties of assignment to any condition may not be greater than 1 or less than zero."
    )
  }
  if (sum(prob_each) != 1) {
    stop(
      "The sum of the probabilities of assignment to each condition (prob_each) must equal 1."
    )
  }
}

.check_ra$m <- function(N, blocks, clusters, num_arms, conditions, m) {
  if (m < 0) {
    stop("If specified, the number of units assigned to treatment (m) must be nonnegative.")
  }
  if(is.null(blocks)) {
    if (m > N) {
      stop(
        "If specified, the number of units assigned to treatment (m) must not be greater than the total number of units (N)."
      )
    }
  } else if(m > min(attr(blocks, "N_per_block"))){
    stop(
      "The number of units assigned to treatment within a block must not exceed the total number units within the block."
    )
  }


}

.check_ra$m_each <- function(N, blocks, clusters, num_arms, conditions, m_each) {
  if (any(m_each < 0)) {
    stop("The number of units assigned to all conditions must be nonnegative.")
  }
  if (sum(m_each) != N) {
    stop(
      "The sum of the number assigned to each condition (m_each) must equal the total number of units (N)"
    )
  }
}

.check_ra$block_prob <- function(N, blocks, clusters, num_arms, conditions, block_prob) {

  if (any(block_prob > 1 | block_prob < 0)) {
    stop("The probabilities of assignment to treatment must be between 0 and 1 for all blocks.")
  }

  if (length(block_prob) != attr(blocks, "N_blocks")) {
    stop(
      "If specified, block_prob should have the same length as there are unique blocks in blocks."
    )
  }
  
}

.check_ra$block_prob_each <- function(N, blocks, clusters, num_arms, conditions, block_prob_each) {

  if(any(block_prob_each < 0 | block_prob_each > 1)){
    stop("The probabilities of assignment to treatment must be between 0 and 1 for all blocks.")
  }
  if (nrow(block_prob_each) != attr(blocks, "N_blocks")) {
    stop(
      "If specified, block_prob_each should have the same number of rows as there are unique blocks in blocks"
    )
  }
  if (is.null(clusters) && any(rowSums(block_prob_each) != 1)) {
    stop("If specified, each row of block_prob_each must sum to 1.")
  }

}

.check_ra$block_m <- function(N, blocks, clusters, num_arms, conditions, block_m) {

  if (length(block_m) != attr(blocks, "N_blocks")) {
    stop(
      "If specified, block_m should have the same length as there are unique blocks in blocks."
    )
  }
  if (any(block_m > attr(blocks, "N_per_block") | block_m < 0)) {
    stop(
      "The number of units assigned to treatment within a block must be nonnegative and not exceed the total number units within the block."
    )
  }
}

.check_ra$block_m_each <- function(N, blocks, clusters, num_arms, conditions, block_m_each) {

  if (nrow(block_m_each) != attr(blocks, "N_blocks")) {
    stop(
      "If specified, block_m_each should have the same number of rows as there are unique blocks in blocks."
    )
  }
  if (is.null(clusters) && any(rowSums(block_m_each) != attr(blocks, "N_per_block"))) {
    stop(
      "If specified, each row of block_m_each must sum to the number of units in the corresponding block."
    )
  }
}



check_samplr_arguments_new <- function(all_args){
  do.call("check_samplr_arguments", all_args)
}

check_samplr_arguments <-
  function(N = NULL,
           prob = NULL,
           n = NULL,
           strata = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           clusters = NULL, ...) {
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
      conditions =  c(0, 1),
      condition_names = c(0, 1),
      N_per_stratum = N_per_stratum
    ))
    
  }



clean_condition_names <- function(assignment, conditions) {
  
  if(is.factor(conditions)){
    return(factor(assignment, levels = levels(conditions)))
  }
  
  if (is.numeric(assignment)) {
    return(assignment)
  }
  
  if (is.logical(assignment)) {
    return(as.numeric(assignment))
  }    
  
  return(factor(assignment, levels = conditions))
}
