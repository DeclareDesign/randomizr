





# Call check() with all formal arguments of the invoking function
# reinsert results into that environment
.invoke_check <- function(check) {
  definition <- sys.function(sys.parent())
  envir <- parent.frame(1)
  all_args <- mget(names(formals(definition)), envir)
  ret <- check(all_args)
  list2env(ret, envir)
  invisible(NULL)
}

#f <- function(N,n) {.invoke_check(function(a) list(n = a[["n"]] + 1)); n}

check_randomizr_arguments_new <-
  function(all_args)
    do.call("check_randomizr_arguments", all_args)

check_randomizr_arguments <-
  function(N = NULL,
           prob = NULL,
           prob_unit = NULL,
           m = NULL,
           m_unit = NULL,
           m_each = NULL,
           prob_each = NULL,
           blocks = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           clusters = NULL,
           num_arms = NULL,
           simple = NULL,
           conditions = NULL,
           ...) {
    # N, blocks, clusters, num_arms, conditions are used generally, check them first
    if (!is.null(clusters)) {
      N <- length(unique(clusters))
    }
    
    if (!is.null(blocks)) {
      if (isTRUE(simple))
        stop("You can't specify `simple` when using blocked assignment.",
             call. = FALSE)
      
      if (!is.null(clusters)) {
        N_per_block <-
          tapply(clusters, blocks, function(x)
            length(unique(x)))
        attributes(N_per_block) <- NULL
        
        # N would be set by clusters check abovew
        if (any(colSums(table(blocks, clusters) > 0) > 1)) {
          stop("All units within a cluster must be in the same block.",
               call. = FALSE)
        }
      } else {
        N_per_block <- tapply(blocks, blocks, length)
        attributes(N_per_block) <- NULL
      }
      if (is.null(N)) {
        N <- sum(N_per_block)
      } else if (N != sum(N_per_block)) {
        stop("N should equal the length of blocks.", call. = FALSE)
      }
      
      N_blocks <- length(N_per_block)
      
      attributes(blocks) <-
        list(N_per_block = N_per_block, N_blocks = N_blocks)
      
    }
    
    if (is.null(N)) {
      stop("N, blocks or clusters must be specified.", call. = FALSE)
    }
    
    if (length(N) != 1 || N != floor(N) || N <= 0) {
      stop("N must be a positive integer scalar.", call. = FALSE)
    }
    
    if (anyDuplicated(conditions)) {
      stop("You must supply unique values to conditions.", call. = FALSE)
    }
    
    if (!is.null(num_arms) &&
        !is.null(conditions) &&  num_arms != length(conditions)) {
      stop(
        "If both conditions and num_arms are specified, the length of conditions must be equal to num_arms.",
        call. = FALSE
      )
    }
    
    # Each of these should be a unique specifier, consistent with above general args
    conflict_args <-
      c(
        "prob",
        "prob_each",
        "prob_unit",
        "m",
        "m_unit",
        "m_each",
        "block_prob",
        "block_prob_each",
        "block_m",
        "block_m_each"
      )
    specified_args <- Filter(Negate(is.null), mget(conflict_args))
    
    if (length(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(specified_args), collapse = " and "),
           ".",
           call. = FALSE)
    } else if (length(specified_args) == 1) {
      arg <- names(specified_args)
      arg_block <- grepl("^block_", arg)
      arg_each  <- grepl("_each$", arg)
      
      if (arg_block &&
          is.null(blocks))
        stop("Specified `", arg, "` but blocks is NULL.", call. = FALSE)
      
      if (isTRUE(simple) &&
          !grepl("prob", arg))
        stop("You can't specify `", arg, "`` when simple = TRUE.", call. = FALSE)
      
      # checking num_arms and conditions consistency
      .check_ra_arg_num_arms_conditions(arg,
                                        arg_block,
                                        arg_each,
                                        specified_args[[1]],
                                        num_arms,
                                        conditions)
      
      .check_ra[[arg]](N,
                       blocks,
                       clusters,
                       num_arms,
                       conditions,
                       simple,
                       specified_args[[1]])
      
    }
    
    
    # learn about design
    
    # obtain num_arms
    
    if (is.null(num_arms)) {
      num_arms <- if (!is.null(conditions))
        length(conditions)
      else if (length(specified_args) == 0)
        2
      else if (!arg_each)
        2
      else if (!arg_block &&
               !is.matrix(specified_args[[1]]))
        length(specified_args[[1]])
      else
        ncol(specified_args[[1]])
      
      if (num_arms == 2 && is.null(conditions))
        conditions <- 0:1
    }
    
    # obtain conditions, wasn't set by num_arms guess
    if (is.null(conditions)) {
      conditions <- paste0("T", 1:num_arms)
    }
    
    
    ret <- list(
      num_arms = num_arms,
      conditions = conditions,
      # condition_names = conditions,
      N_per_block = get0("N_per_block")
    )
    
    ret
  }

# Arg-specific checks
.check_ra <- new.env(parent = emptyenv())


# Generic checks that block, each num_arms, conditions are consistent.
.check_ra_arg_num_arms_conditions <-
  function(arg,
           arg_block,
           arg_each,
           value,
           num_arms,
           conditions) {
    if (!arg_each) {
      w <- 2
      num_arms_fmt <-
        "If %s and num_arms are both specified, num_arms must be 2."
      conditions_fmt <-
        "If %s and conditions are both specified, conditions must be length 2."
    } else if (!arg_block) {
      w <- length(value)
      num_arms_fmt <-
        "If %s and num_arms are both specified, the length of %s must be equal to num_arms."
      conditions_fmt <-
        "If %s and conditions are both specified, they must be the same length."
    } else {
      w <- ncol(value)
      num_arms_fmt <-
        "If %s and num_arms are both specified, the number of columns of %s must be equal to num_arms."
      conditions_fmt <-
        "If %s and conditions are both specified, the length of conditions must be equal to the number of columns of %s."
    }
    
    if (!is.null(num_arms) && num_arms != w) {
      stop(sprintf(num_arms_fmt, arg, arg), call. = FALSE)
    }
    
    if (!is.null(conditions) && length(conditions) != w) {
      stop(sprintf(conditions_fmt, arg, arg), call. = FALSE)
    }
  }

.check_ra$prob <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           prob) {
    if (any(prob > 1 | prob < 0)) {
      stop("The probability of assignment to treatment must be between 0 and 1.",
           call. = FALSE)
    }
    if (!length(prob) %in% c(1)) {
      stop("`prob` must be of length 1.", call. = FALSE)
    }
  }

.check_ra$prob_unit <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           prob_unit) {
    if (any(prob_unit > 1 | prob_unit < 0)) {
      stop("The probability of assignment to treatment must be between 0 and 1.",
           call. = FALSE)
    }
    
    # if it's complete
    if (is.null(blocks)) {
      if (!isTRUE(simple)) {
        if (!is_constant(prob_unit)) {
          stop(
            "In a complete random assignment design, `prob_unit` must be the same for all units.",
            call. = FALSE
          )
        }
      }
    } else{
      # if it's blocked
      if (!all(tapply(
        X = prob_unit,
        INDEX = blocks,
        FUN = is_constant
      ))) {
        stop(
          "In a block random assignment design, `prob_unit` must be the same for all units within the same block.",
          call. = FALSE
        )
      }
    }
    
    # if it's clustered:
    if (!is.null(clusters)) {
      if (!length(prob_unit) %in% length(clusters)) {
        stop("`prob_unit` must be of length N.", call. = FALSE)
      }
      
    } else{
      if (!length(prob_unit) %in% c(N)) {
        stop("`prob_unit` must be of length N.", call. = FALSE)
      }
    }
  }

.check_ra$prob_each <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           prob_each) {
    if (any(prob_each > 1 | prob_each < 0)) {
      stop(
        "The probabilties of assignment to any condition may not be greater than 1 or less than zero.",
        call. = FALSE
      )
    }
    if (is.vector(prob_each)) {
      if (all.equal(sum(prob_each), 1) != TRUE) {
        stop(
          "The sum of the probabilities of assignment to each condition (prob_each) must equal 1 for each obs.",
          call. = FALSE
        )
      }
    }
    else if (is.matrix(prob_each)) {
      if (any(sapply(rowSums(prob_each), function(x)
        all.equal(x, 1) != TRUE))) {
        stop(
          "The sum of the probabilities of assignment to each condition (prob_each) must equal 1 for each obs.",
          call. = FALSE
        )
      }
      if (!nrow(prob_each) %in% c(1, N)) {
        stop("`prob_each` must have either 1 or N rows.", call. = FALSE)
      }
    }
    else
      stop("`prob_each` must be a vector or matrix.", call. = FALSE)
    
    
  }

.check_ra$m <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           m) {
    if (length(m) != 1) {
      stop("If specified, the number of units assigned to treatment (m) must be a scalar.",
           call. = FALSE)
    }
    
    if (m < 0) {
      stop(
        "If specified, the number of units assigned to treatment (m) must be nonnegative.",
        call. = FALSE
      )
    }
    if (m != floor(m)) {
      stop("`m` must be an integer.")
    }
    if (is.null(blocks)) {
      if (m > N) {
        stop(
          "If specified, the number of units assigned to treatment (m) must not be greater than the total number of units (N).",
          call. = FALSE
        )
      }
    } else if (m > min(attr(blocks, "N_per_block"))) {
      stop(
        "The number of units assigned to treatment within a block must not exceed the total number units within the block.",
        call. = FALSE
      )
    }
  }

.check_ra$m_unit <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           m_unit) {
    # if it's complete
    if (is.null(blocks)) {
      if (!is_constant(m_unit)) {
        stop(
          "In a complete random assignment design, `m_unit` must be the same for all units.",
          call. = FALSE
        )
      }
    } else{
      # if it's blocked
      if (!all(tapply(X = m_unit, INDEX = blocks, FUN = is_constant))) {
        stop(
          "In a block random assignment design, `m_unit` must be the same for all units within the same block.",
          call. = FALSE
        )
      }
    }
    
    # if it's clustered:
    if (!is.null(clusters)) {
      if (!length(m_unit) %in% length(clusters)) {
        stop("`m_unit` must be of length N.", call. = FALSE)
      }
      
    } else{
      if (!length(m_unit) %in% c(N)) {
        stop("`m_unit` must be of length N", call. = FALSE)
      }
    }
  }


.check_ra$m_each <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           m_each) {
    if (any(m_each < 0)) {
      stop("The number of units assigned to all conditions must be nonnegative.",
           call. = FALSE)
    }
    if (sum(m_each) != N) {
      stop(
        "The sum of the number assigned to each condition (m_each) must equal the total number of units (N).",
        call. = FALSE
      )
    }
  }

.check_ra$block_prob <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           block_prob) {
    if (any(block_prob > 1 | block_prob < 0)) {
      stop(
        "The probabilities of assignment to treatment must be between 0 and 1 for all blocks.",
        call. = FALSE
      )
    }
    
    if (length(block_prob) != attr(blocks, "N_blocks")) {
      stop(
        "If specified, block_prob should have the same length as there are unique blocks in blocks.",
        call. = FALSE
      )
    }
    
  }

.check_ra$block_prob_each <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           block_prob_each) {
    if (any(block_prob_each < 0 | block_prob_each > 1)) {
      stop(
        "The probabilities of assignment to treatment must be between 0 and 1 for all blocks.",
        call. = FALSE
      )
    }
    if (nrow(block_prob_each) != attr(blocks, "N_blocks")) {
      stop(
        "If specified, block_prob_each should have the same number of rows as there are unique blocks in blocks.",
        call. = FALSE
      )
    }
    if (is.null(clusters) &&
        any(sapply(rowSums(block_prob_each), function(x)
          all.equal(x, 1) != TRUE))) {
      stop("If specified, each row of block_prob_each must sum to 1.",
           call. = FALSE)
    }
    
  }

.check_ra$block_m <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           block_m) {
    if (length(block_m) != attr(blocks, "N_blocks")) {
      stop(
        "If specified, block_m should have the same length as there are unique blocks in blocks.",
        call. = FALSE
      )
    }
    if (any(block_m > attr(blocks, "N_per_block") | block_m < 0)) {
      stop(
        "The number of units assigned to treatment within a block must be nonnegative and not exceed the total number units within the block.",
        call. = FALSE
      )
    }
  }

.check_ra$block_m_each <-
  function(N,
           blocks,
           clusters,
           num_arms,
           conditions,
           simple,
           block_m_each) {
    if (nrow(block_m_each) != attr(blocks, "N_blocks")) {
      stop(
        "If specified, block_m_each should have the same number of rows as there are unique blocks in blocks.",
        call. = FALSE
      )
    }
    if (is.null(clusters) &&
        any(rowSums(block_m_each) != attr(blocks, "N_per_block"))) {
      stop(
        "If specified, each row of block_m_each must sum to the number of units in the corresponding block.",
        call. = FALSE
      )
    }
  }



check_samplr_arguments_new <- function(all_args) {
  do.call("check_samplr_arguments", all_args)
}

check_samplr_arguments <- 
  function(N = NULL,
           prob = NULL,
           prob_unit = NULL,
           n = NULL,
           n_unit = NULL,
           strata = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           clusters = NULL,
           simple = NULL,
           ...) {
    
    if (!is.null(clusters)) {
      N <- length(unique(clusters))
    }
    
    if (!is.null(strata)) {
      if (isTRUE(simple))
        stop("You can't specify 'simple' with strata.", call. = FALSE)
      
      if (!is.null(clusters)) {
        N_per_stratum <-
          tapply(clusters, strata, function(x)
            length(unique(x)))
        attributes(N_per_stratum) <- NULL
        
        # N would be set by clusters check abovew
        if (any(colSums(table(strata, clusters) > 0) > 1)) {
          stop("All units within a cluster must be in the same stratum.",
               call. = FALSE)
        }
      } else {
        N_per_stratum <- tapply(strata, strata, length)
        attributes(N_per_stratum) <- NULL
      }
      
      N_strata <- length(N_per_stratum)
      
      if (is.null(N)) {
        N <- sum(N_per_stratum)
      } else if (N != sum(N_per_stratum)) {
        stop("N should equal the length of strata.", call. = FALSE)
      }
      attributes(strata) <-
        list(N_strata = N_strata, N_per_stratum = N_per_stratum)
    }
    
    if (length(N) != 1 || N != floor(N) || N <= 0) {
      stop("N must be an integer greater than 0.", call. = FALSE)
    }
    
    conflict_args <- c("prob", "prob_unit", "n", "n_unit", "strata_prob", "strata_n")
    specified_args <- Filter(Negate(is.null), mget(conflict_args))
    
    if (length(specified_args) > 1) {
      stop("Please specify only one of ",
           paste(names(specified_args), collapse = " and "),
           ".",
           call. = FALSE)
    } else if (length(specified_args) == 1) {
      arg <- names(specified_args)
      
      if (isTRUE(simple) &&
          !grepl("prob", arg))
        stop("You can't specify `", arg, "` when simple = TRUE.", call. = FALSE)
      
      
      .check_rs[[arg]](N, strata, clusters, simple, specified_args[[1]])
    }
    
    list(
      num_arms = 2,
      conditions =  0:1,
      condition_names = 0:1,
      N_per_stratum = get0("N_per_stratum")
    )
    
  }

# rs specific arg checks
.check_rs <- new.env(parent = emptyenv())

.check_rs$prob <-
  function(N,
           strata,
           clusters,
           simple,
           prob) {
    if (any(prob > 1 | prob < 0)) {
      stop("The probability of assignment to treatment must be between 0 and 1.",
           call. = FALSE)
    }
    if (!length(prob) %in% c(1)) {
      stop("`prob` must be of length 1.", call. = FALSE)
    }
  }

.check_rs$prob_unit <-
  function(N,
           strata,
           clusters,
           simple,
           prob_unit) {
    if (any(prob_unit > 1 | prob_unit < 0)) {
      stop("The probability of being sampled must be between 0 and 1.",
           call. = FALSE)
    }
    # if it's complete
    if (is.null(strata)) {
      if (!isTRUE(simple)) {
        if (!is_constant(prob_unit)) {
          stop(
            "In a complete random sampling design, `prob_unit` must be the same for all units.",
            call. = FALSE
          )
        }
      }
    } else{
      # if it's strata
      if (!all(tapply(X = prob_unit, INDEX = strata, FUN = is_constant))) {
        stop(
          "In a stratified random assignment design, `prob_unit` must be the same for all units within the same stratum.",
          call. = FALSE
        )
      }
    }
    # if it's clustered:
    if (!is.null(clusters)) {
      if (!length(prob_unit) %in% length(clusters)) {
        stop("`prob_unit` must be of length N.", call. = FALSE)
      }
      
    } else{
      if (!length(prob_unit) %in% c(N)) {
        stop("`prob_unit` must be of length N.", call. = FALSE)
      }
    }
  }

.check_rs$strata_prob <-
  function(N, 
           strata, 
           clusters, 
           simple,
           strata_prob) {
    if (any(strata_prob > 1 | strata_prob < 0)) {
      stop("The probabilities of being sampled must be between 0 and 1 for all strata.",
           call. = FALSE)
    }
    if (length(strata_prob) != attr(strata, "N_strata")) {
      stop(
        "If specified, strata_prob should have the same length as there are unique strata in strata.",
        call. = FALSE
      )
    }
  }


.check_rs$n <- 
  function(N, 
           strata, 
           clusters, 
           simple,
           n) {
    if (!length(n) %in% c(1)) {
      stop("If specified, the number of units sampled (n) must be of length 1.", call. = FALSE)
    }
    
    if (n < 0) {
      stop("If specified, the number of units sampled (n) must be nonnegative.",
           call. = FALSE)
    }
    if (n != floor(n)) {
      stop("If specified, the number of units sampled (n) must be an integer.",
           call. = FALSE)
    }
    if (is.null(strata)) {
      if (n > N) {
        stop(
          "If specified, the number of units sampled (n) must not be greater than the total number of units (N).",
          call. = FALSE
        )
      }
    } else {
      if (n > min(attr(strata, "N_per_stratum"))) {
        stop(
          "If specified, the number of units sampled (n) must not be greater than the number of units in a strata (N).",
          call. = FALSE
        )
      }
    }
  }

.check_rs$n_unit <-
  function(N,
           strata,
           clusters,
           simple,
           n_unit) {
    # if it's complete
    if (is.null(strata)) {
      if (!is_constant(n_unit)) {
        stop(
          "In a complete random sampling design, `n_unit` must be the same for all units.",
          call. = FALSE
        )
      }
    } else{
      # if it's stratified
      if (!all(tapply(X = n_unit, INDEX = strata, FUN = is_constant))) {
        stop(
          "In a stratified random sampling design, `n_unit` must be the same for all units within the same stratum.",
          call. = FALSE
        )
      }
    }
    
    # if it's clustered:
    if (!is.null(clusters)) {
      if (!length(n_unit) %in% length(clusters)) {
        stop("`n_unit` must be of length N.", call. = FALSE)
      }
      
    } else{
      if (!length(n_unit) %in% c(N)) {
        stop("`n_unit` must be of length N.", call. = FALSE)
      }
    }
  }


.check_rs$strata_n <- 
  function(N, 
           strata, 
           clusters, 
           simple,
           strata_n) {
    if (length(strata_n) != attr(strata, "N_strata")) {
      stop(
        "If specified, strata_n should have the same length as there are unique strata in strata.",
        call. = FALSE
      )
    }
    if (any(strata_n > attr(strata, "N_per_stratum") |
            strata_n < 0)) {
      stop(
        "The number of units sampled within a stratum must be nonnegative and not exceed the total number units within the strata.",
        call. = FALSE
      )
    }
  }


clean_condition_names <- 
  function(assignment, conditions) {
    if (is.factor(conditions)) {
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

is_constant <- function(x)
  all(x[1] == x)
