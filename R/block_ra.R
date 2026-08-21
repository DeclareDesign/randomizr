#' Block Random Assignment
#'
#' \code{block_ra} assigns units to treatment conditions within pre-defined groups called blocks (or strata). Within each block, complete random assignment determines which units are treated. Blocking typically reduces the sampling variability of an experiment relative to simple or complete random assignment: by guaranteeing that treated and control units are drawn from every covariate-defined subgroup, it rules out the unlucky assignments that would otherwise pull estimates far from the true average treatment effect. The precision gain is largest when the blocking variable is strongly correlated with potential outcomes; if the blocking variable is uncorrelated with outcomes, blocking neither helps nor hurts.
#'
#' In the simplest two-arm case with no arguments beyond \code{blocks}, the function assigns approximately half the units in each block to treatment. Researchers can specify exact counts (via \code{block_m}) or target probabilities that are held constant (via \code{prob}) or allowed to vary (via \code{block_prob}) across blocks.
#'
#' @seealso \code{\link{complete_ra}()}, \code{\link{block_and_cluster_ra}()}, \code{\link{strata_rs}()}, \code{\link{block_ra_probabilities}()}
#'
#' @param blocks A vector of length N indicating which block each unit belongs to. Can be character, factor, or numeric. (required)
#' @param prob Use for a two-arm design in which either \code{floor(N_block*prob)} or \code{ceiling(N_block*prob)} units are assigned to treatment within each block. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_block*prob} and the floor otherwise, which makes each unit's probability of assignment exactly \code{prob}. When \code{N_block*prob} is a whole number the count is fixed. Must be a real number between 0 and 1. (optional)
#' @param prob_unit Use for a two-arm design. Must be of length N. \code{tapply(prob_unit, blocks, unique)} will be passed to \code{block_prob}. (optional)
#' @param prob_each Use for a multi-arm design in which the values of \code{prob_each} determine the probabilities of assignment to each treatment condition. Must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 and the total must sum to 1. Because of integer rounding, the exact number of units assigned to each condition may differ slightly from assignment to assignment, but the overall probability of assignment is exactly \code{prob_each}. (optional)
#' @param m Use for a two-arm design in which the scalar \code{m} gives the fixed number of units to assign to treatment within every block. This count does not vary across blocks. (optional)
#' @param m_unit Use for a two-arm design. Must be of length N. \code{tapply(m_unit, blocks, unique)} will be passed to \code{block_m}. (optional)
#' @param block_m Use for a two-arm design in which \code{block_m} gives the number of units to assign to treatment within each block. Must be a numeric vector as long as the number of blocks, in the same order as \code{sort(unique(blocks))}. (optional)
#' @param block_m_each Use for a multi-arm design in which \code{block_m_each} gives the number of units assigned to each condition within each block. Must be a matrix with one row per block and one column per treatment arm. Rows should respect the ordering of blocks by \code{sort(unique(blocks))}; columns should be in the order of \code{conditions}, if specified. (optional)
#' @param block_prob Use for a two-arm design in which the probability of assignment to treatment varies across blocks. Must be in the same order as \code{sort(unique(blocks))}. (optional)
#' @param block_prob_each Use for a multi-arm design in which assignment probabilities vary across blocks. Must be a matrix with one row per block and one column per treatment arm. Each row must sum to 1. Rows respect the ordering of \code{sort(unique(blocks))}. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param check_inputs Logical. Whether to verify before assigning that the arguments are internally consistent: that counts sum to the block sizes, that probabilities lie between 0 and 1 and sum to 1, that matrices have one row per block, and so on. Defaults to \code{TRUE}. The check also fills in arguments that were left implicit, notably \code{conditions}, so with \code{FALSE} every argument the assignment needs must be supplied explicitly. Declaring the design once with \code{\link{declare_ra}()} and drawing from it with \code{\link{conduct_ra}()} is the usual way to avoid re-checking the same arguments in a simulation. (optional)
#' @param .block_int Internal use only. Pre-computed integer encoding of \code{blocks}, passed by \code{\link{conduct_ra}()} when a declaration was created with \code{\link{declare_ra}()}. Users should never set this argument. (optional)
#' @param .N_per_block Internal use only. Pre-computed block sizes corresponding to \code{.block_int}, passed by \code{\link{conduct_ra}()}. Users should never set this argument. (optional)
#'
#' @return A vector of length N indicating the treatment condition of each unit. Numeric in a two-arm trial; a factor (ordered by \code{conditions}) in a multi-arm trial.
#' @export
#'
#'
#' @examples
#'
#' # Two-arm Designs
#'
#' blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' Z <- block_ra(blocks = blocks)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, prob = 0.3)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, block_prob = c(0.1, 0.2, 0.3))
#' table(blocks, Z)
#' 
#' Z <- block_ra(blocks = blocks, 
#'               prob_unit = rep(c(0.1, 0.2, 0.3), 
#'                               times = c(50, 100, 200)))
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, m = 20)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, block_m = c(20, 30, 40))
#' table(blocks, Z)
#' 
#' Z <- block_ra(blocks = blocks, 
#'               m_unit = rep(c(20, 30, 40),
#'                            times = c(50, 100, 200)))
#' table(blocks, Z)
#'
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#'
#' Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
#' table(blocks, Z)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#'
#' Z <- block_ra(blocks = blocks, block_m_each = block_m_each,
#'               conditions = c("control", "treatment"))
#' table(blocks, Z)
#'
#' # Multi-arm Designs
#' Z <- block_ra(blocks = blocks, num_arms = 3)
#' table(blocks, Z)
#'
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' Z <- block_ra(blocks = blocks, block_m_each = block_m_each)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, block_m_each = block_m_each,
#'               conditions = c("control", "placebo", "treatment"))
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, prob_each = c(0.1, 0.1, 0.8))
#' table(blocks, Z)
#'
#'
#'
block_ra <- function(blocks = NULL,
                     prob = NULL,
                     prob_unit = NULL,
                     prob_each = NULL,
                     m = NULL,
                     m_unit = NULL,
                     block_m = NULL,
                     block_m_each = NULL,
                     block_prob = NULL,
                     block_prob_each = NULL,
                     num_arms = NULL,
                     conditions = NULL,
                     check_inputs = TRUE,
                     .block_int = NULL,    # pre-computed from declaration cache
                     .N_per_block = NULL) {
  if (!is.null(.block_int)) {
    # Cached path: declaration pre-computed the factor encoding at declare_ra()
    # time. No as.factor or tabulate needed — saves ~21 us per simulation draw.
    block_int   <- .block_int
    N_per_block <- .N_per_block
  } else {
    # Encode blocks as a factor once; reused by validation (via .N_per_block
    # hint) and fast path, avoiding a duplicate as.factor call.
    block_fac <- as.factor(blocks)
    block_int <- as.integer(block_fac)
    N_per_block <- tabulate(block_int)
  }

  if (check_inputs) {
    # Pass N_per_block as a hint so check_randomizr_arguments skips its own
    # tabulate call (the .N_per_block argument is accepted via ...).
    .invoke_check(check_randomizr_arguments_new)
  }

  # Two-arm fast path: block_assign_cpp() sorts N units by (block, runif) in
  # one C++ call and thresholds within each block — no per-block R overhead.
  if (!is.null(num_arms) && num_arms == 2L &&
      is.null(prob_each) && is.null(block_m_each) && is.null(block_prob_each)) {
    if (!is.null(prob_unit)) block_prob <- tapply(prob_unit, blocks, unique)
    if (!is.null(m_unit))    block_m    <- tapply(m_unit,    blocks, unique)

    # Which of randomizr 1.x's three complete_ra() branches this call maps to.
    # The kernel has to consume the RNG the same way that branch did, and in the
    # same per-block order, or seeds set under 1.x stop reproducing.
    G <- length(N_per_block)
    if (!is.null(m) || !is.null(block_m)) {
      mode    <- 0L
      m_per_b <- if (!is.null(block_m)) as.integer(block_m) else rep(as.integer(m), G)
      prob_v  <- numeric(G)
    } else if (!is.null(block_prob)) {
      mode    <- 2L
      prob_v  <- as.numeric(block_prob)
      m_per_b <- integer(G)
    } else {
      # block_ra_helper() converts a scalar prob, and the default, into
      # prob_each before calling complete_ra, which is a different branch and a
      # different stream from block_prob.
      mode    <- 1L
      prob_v  <- rep(if (!is.null(prob)) prob else 0.5, G)
      m_per_b <- integer(G)
    }
    raw        <- block_assign_cpp(block_int, m_per_b, prob_v, mode)
    cond       <- if (!is.null(conditions)) conditions else c(0L, 1L)
    assignment <- cond[raw + 1L]
    return(clean_condition_names(assignment, conditions))
  }

  # Multi-arm fast path: the three cases block_ra_helper() routes to
  # complete_ra()'s prob_each and m_each branches, done in one C++ call instead
  # of one R call per block. m, block_m and block_prob are excluded because they
  # are the two-arm kernel's cases above; prob_unit and m_unit are excluded
  # because block_ra_helper() turns them into block_prob and block_m, which are
  # likewise two-arm. conditions has to be known, which the argument check
  # guarantees whenever it runs.
  if (!is.null(conditions) &&
      is.null(m) && is.null(m_unit) && is.null(block_m) &&
      is.null(prob_unit) && is.null(block_prob)) {
    G <- length(N_per_block)
    if (!is.null(block_m_each)) {
      param <- matrix(as.numeric(as.matrix(block_m_each)), nrow = G)
      multi_mode <- 0L
    } else if (!is.null(block_prob_each)) {
      param <- matrix(as.numeric(as.matrix(block_prob_each)), nrow = G)
      multi_mode <- 1L
    } else {
      # block_ra_helper() infers prob_each from prob, or from num_arms, and
      # hands the same vector to every block.
      pe <- if (!is.null(prob_each)) {
        prob_each
      } else if (!is.null(prob)) {
        c(1 - prob, prob)
      } else {
        rep(1 / num_arms, num_arms)
      }
      param <- matrix(rep(as.numeric(pe), each = G), nrow = G)
      multi_mode <- 1L
    }

    if (ncol(param) == length(conditions)) {
      raw <- block_assign_multi_cpp(block_int, param, multi_mode)
      return(clean_condition_names(conditions[raw + 1L], conditions))
    }
  }

  block_spots <-
    unlist(split(seq_along(blocks), blocks), FALSE, FALSE)

  mapply_args <- list(
    FUN = "complete_ra",
    SIMPLIFY = FALSE,
    N = N_per_block,
    MoreArgs = list(
      conditions = conditions,
      num_arms = num_arms,
      check_inputs = FALSE
    )
  )

  assign_list <-
    block_ra_helper(
      blocks,
      prob,
      prob_unit,
      prob_each,
      m,
      m_unit,
      block_m,
      block_m_each,
      block_prob,
      block_prob_each,
      num_arms,
      N_per_block,
      mapply_args
    )

  assignment <-
    unlist(assign_list, FALSE, FALSE)[order(block_spots)]
  assignment <- clean_condition_names(assignment, conditions)
  return(assignment)
}


#' Probabilities of assignment: Block Random Assignment
#'
#' Returns the probability that each unit is assigned to each condition under
#' block random assignment. Units in different blocks routinely have different
#' probabilities, which is exactly when these numbers are needed.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in,
#' which \code{\link{obtain_condition_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{block_ra}()}
#'
#' @inheritParams block_ra
#' @return A matrix with N rows and one column per treatment condition, with columns named \code{prob_<condition>}. Entry (i, j) is the probability that unit i is assigned to condition j, and every row sums to 1.
#'
#' @examples
#'
#' blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' prob_mat <- block_ra_probabilities(blocks = blocks)
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks, m = 20)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks,
#'                                    block_m_each = block_m_each,
#'                                    conditions = c("control", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks, num_arms = 3)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each)
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks, block_m_each = block_m_each,
#'                        conditions = c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(blocks = blocks, prob_each = c(0.1, 0.1, 0.8))
#' head(prob_mat)
#'
#' @export
block_ra_probabilities <- function(blocks = NULL,
                                   prob = NULL,
                                   prob_unit = NULL,
                                   prob_each = NULL,
                                   m = NULL,
                                   m_unit = NULL,
                                   block_m = NULL,
                                   block_m_each = NULL,
                                   block_prob = NULL,
                                   block_prob_each = NULL,
                                   num_arms = NULL,
                                   conditions = NULL,
                                   check_inputs = TRUE) {
  if (check_inputs) {
    .invoke_check(check_randomizr_arguments_new)
  } else {
    N_per_block <- tapply(blocks, blocks, length)
    attributes(N_per_block) <- NULL
  }
  
  block_spots <-
    unlist(split(seq_along(blocks), blocks), FALSE, FALSE)
  
  # blocks <- sort(unique(blocks))

  mapply_args <- list(
    FUN = "complete_ra_probabilities",
    N = N_per_block,
    MoreArgs = list(
      conditions = conditions,
      num_arms = num_arms,
      check_inputs = FALSE
    ),
    SIMPLIFY = FALSE
  )

  prob_mat <-  block_ra_helper(
    blocks,
    prob,
    prob_unit,
    prob_each,
    m,
    m_unit,
    block_m,
    block_m_each,
    block_prob,
    block_prob_each,
    num_arms,
    N_per_block,
    mapply_args
  )
  
  prob_mat <- do.call(rbind, prob_mat)
  prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
  
  return(prob_mat)
  
}

# consolidates the default argument fillin for block ra / block ra probs

block_ra_helper <- function(blocks = NULL,
                     prob = NULL,
                     prob_unit = NULL,
                     prob_each = NULL,
                     m = NULL,
                     m_unit = NULL, 
                     block_m = NULL,
                     block_m_each = NULL,
                     block_prob = NULL,
                     block_prob_each = NULL,
                     num_arms = NULL,
                     N_per_block, 
                     mapply_args) {
  
  if(!is.null(prob_unit)){
    block_prob <- tapply(prob_unit, blocks, unique)
  }
  
  if(!is.null(m_unit)){
    block_m <- tapply(m_unit, blocks, unique)
  }
  
  
  # Case 0: m is specified
  
  if (!is.null(m)) {
    ret <- list(m=rep(m, length(N_per_block)))
  }
  
  # Case 1 use block_m
  
  else if (!is.null(block_m)) {
    ret <- list(m=block_m)
  }
  
  # Case 1.5 use block_prob
  
  else if (!is.null(block_prob)) {
    ret <- list(prob=block_prob)
  }
  
  # Case 2 use or infer prob_each
  else if (is.null(block_m_each) & is.null(block_prob_each)) {
    if (!is.null(prob)) {
      prob_each <- c(1 - prob, prob)
    }
    
    if (is.null(prob_each)) {
      prob_each <- rep(1 / num_arms, num_arms)
    }
    
    ret <- list(prob_each=list(prob_each))
  }
  
  # Case 2 use block_m_each
  
  else if (!is.null(block_m_each)) {
    block_m_each_list <-
      split(block_m_each, seq_len(nrow(block_m_each)))
    
    ret <- list(m_each=block_m_each_list)
  }
  
  
  # Case 3 use block_prob_each
  
  else if (!is.null(block_prob_each)) {
    block_prob_each_list <-
      split(block_prob_each, seq_len(nrow(block_prob_each)))
    
    ret <- list(prob_each=block_prob_each_list)
  }
  
  do.call(mapply, append(mapply_args, ret))
}