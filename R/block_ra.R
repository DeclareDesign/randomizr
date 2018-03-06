#' Block Random Assignment
#'
#' block_ra implements a random assignment procedure in which units that are grouped into blocks defined by pre-treatment covariates are assigned using complete random assignment within block. For example, imagine that 50 of 100 men are assigned to treatment and 75 of 200 women are assigned to treatment.
#'
#' @param blocks A vector of length N that indicates which block each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a two-arm design in which either floor(N_block*prob) or ceiling(N_block*prob) units are assigned to treatment within each block. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_block*prob) units will be assigned to treatment and with probability prob, ceiling(N_block*prob) units will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilities of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of units assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param m Use for a two-arm design in which the scalar m describes the fixed number of units to assign in each block. This number does not vary across blocks.
#' @param block_m Use for a two-arm design in which the vector block_m describes the number of units to assign to treatment within each block. block_m must be a numeric vector that is as long as the number of blocks. Note that in previous versions of randomizr, block_m behaved like block_m_each.
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of units assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). The columns should be in the order of conditions, if specified.
#' @param block_prob Use for a two-arm design in which block_prob describes the probability of assignment to treatment within each block. Differs from prob in that the probability of assignment can vary across blocks.
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilities of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilities of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An exception is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#' @param block_var deprecated
#' @param condition_names deprecated
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by conditions) in a multi-arm trial.
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
#' Z <- block_ra(blocks = blocks, prob = .3)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, block_prob = c(.1, .2, .3))
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, m = 20)
#' table(blocks, Z)
#'
#' Z <- block_ra(blocks = blocks, block_m = c(20, 30, 40))
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
#' Z <- block_ra(blocks = blocks, prob_each = c(.1, .1, .8))
#' table(blocks, Z)
#'
#'
#'
block_ra <- function(blocks = block_var,
                     prob = NULL,
                     prob_each = NULL,
                     m = NULL,
                     block_m = NULL,
                     block_m_each = NULL,
                     block_prob = NULL,
                     block_prob_each = NULL,
                     num_arms = NULL,
                     conditions = condition_names,
                     check_inputs = TRUE,
                     block_var = NULL,
                     condition_names = NULL) {
  warn_deprecated_args(block_var)
  input_check <- NULL
  if (check_inputs) {
    input_check <- check_randomizr_arguments(
      blocks = blocks,
      prob = prob,
      prob_each = prob_each,
      m = m,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      num_arms = num_arms,
      conditions = conditions
    )
    num_arms <- input_check$num_arms
    conditions <- input_check$conditions
    N_per_block <- input_check$N_per_block
    
  } else {
    N_per_block <- tapply(blocks, blocks, length)
    attributes(N_per_block) <- NULL
  }
  
  block_spots <-
    unlist(split(1:length(blocks), blocks), FALSE, FALSE)
  
  if (!is.null(prob)) {
    prob_each <- c(1 - prob, prob)
  }
  
  # Setup: obtain number of arms and conditions
  
  # Case 0: m is specified
  
  if (!is.null(m)) {
    block_m <- rep(m, length(N_per_block))
  }
  
  # Case 1: block_m is specified
  if (!is.null(block_m)) {
    assign_list <-
      mapply(
        FUN = complete_ra,
        N = N_per_block,
        m = block_m,
        MoreArgs = list(
          conditions = conditions,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(block_spots)]
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Case 1.5: block_prob is specified
  if (!is.null(block_prob)) {
    assign_list <-
      mapply(
        FUN = complete_ra,
        N = N_per_block,
        prob = block_prob,
        MoreArgs = list(
          conditions = conditions,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(block_spots)]
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Case 2 use or infer prob_each
  if (is.null(block_m_each) & is.null(block_prob_each)) {
    if (is.null(prob_each)) {
      prob_each <- rep(1 / num_arms, num_arms)
    }
    assign_list <-
      mapply(
        FUN = complete_ra,
        N = N_per_block,
        MoreArgs = list(
          prob_each = prob_each,
          conditions = conditions,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(block_spots)]
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Case 3 use block_m_each
  
  if (!is.null(block_m_each)) {
    block_m_each_list <-
      #split(block_m_each, rep(1:nrow(block_m_each), times = ncol(block_m_each)))
      split(block_m_each, 1:nrow(block_m_each))
    
    assign_list <-
      mapply(
        FUN = complete_ra,
        N = N_per_block,
        m_each = block_m_each_list,
        MoreArgs = list(
          conditions = conditions,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(block_spots)]
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Case 4 use block_prob_each
  
  if (!is.null(block_prob_each)) {
    block_prob_each_list <-
      #split(block_prob_each, rep(1:nrow(block_prob_each), times = ncol(block_prob_each)))
      split(block_prob_each, 1:nrow(block_prob_each))
    
    
    assign_list <-
      mapply(
        FUN = complete_ra,
        N = N_per_block,
        prob_each = block_prob_each_list,
        MoreArgs = list(
          conditions = conditions,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    assignment <-
      unlist(assign_list, FALSE, FALSE)[order(block_spots)]
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
}
