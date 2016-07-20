#' Block Random Assignment
#'
#' block_ra implements a random assignment in which units that are grouped into blocks defined by pre-treatment covariates are assiged using complete random assignment withing block. For example, imagine that 50 of 100 men are assigned to treatment and 75 of 200 women are assigned to treatment.
#'
#' @param block_var A vector of length N that indicates which block each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a two-arm design in which either floor(N_block*prob) or ceiling(N_block*prob) units are assigned to treatment within each block. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_block*prob) units will be assigned to treatment and with probability prob, ceiling(N_block*prob) units will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilties of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of units assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param block_m Use for a two-arm design in which block_m describes the number of units to assign to treatment within each block. Note that in previous versions of randomizr, block_m behaved like block_m_each.
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of units assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilties of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An execption is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param balance_load logical, defaults to FALSE. This feature is experimental. If set to TRUE, the function will resolve rounding problems by randomly assigning "remainder" units to each possible treatment condition with equal probability, while ensuring that the total number of units assigned to each condition does not vary greatly from assignment to assignment. However, the true probabiltiies of assignment may be different from the nominal probabilities specified in prob_each or block_prob_each. Please use with caution and perform many tests before using in a real research scenario.
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by condition_names) in a multi-arm trial.
#' @export
#'
#' @importFrom stats r2dtable
#'
#' @examples
#'
#' # Two-arm Designs
#'
#' block_var <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' Z <- block_ra(block_var = block_var)
#' table(block_var, Z)
#'
#' Z <- block_ra(block_var = block_var, prob = .3)
#' table(block_var, Z)

#' Z <- block_ra(block_var = block_var, block_m = c(20, 30, 40))
#' table(block_var, Z)
#'
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#'
#' Z <- block_ra(block_var = block_var, block_m_each = block_m_each)
#' table(block_var, Z)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#'
#' Z <- block_ra(block_var = block_var, block_m_each = block_m_each,
#'               condition_names = c("control", "treatment"))
#' table(block_var, Z)
#'
#' # Multi-arm Designs
#' Z <- block_ra(block_var = block_var, num_arms = 3)
#' table(block_var, Z)
#'
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' Z <- block_ra(block_var = block_var, block_m_each = block_m_each)
#' table(block_var, Z)
#'
#' Z <- block_ra(block_var = block_var, block_m_each = block_m_each,
#'               condition_names = c("control", "placebo", "treatment"))
#' table(block_var, Z)
#'
#' Z <- block_ra(block_var = block_var, prob_each = c(.1, .1, .8))
#' table(block_var, Z)
#'
#'
#' # Experimental feature: load balancing
#' # This procedure constrains the total number of units in each arm.  
#' # This will never exceed 5 treated units total.
#'
#' block_var <- rep(c("A", "B","C"), times=c(3, 3, 3))
#' Z <- block_ra(block_var = block_var, balance_load = TRUE)
#' table(block_var, Z)
#'
#' # compare to block_ra without load balancing
#' # Sometimes this procedure assigns 6 total units to treatment
#' Z <- block_ra(block_var = block_var, balance_load = FALSE)
#' table(block_var, Z)
#'
block_ra <- function(block_var,
                     prob = NULL,
                     prob_each = NULL,
                     block_m = NULL,
                     block_m_each = NULL,
                     block_prob_each = NULL,
                     num_arms = NULL,
                     condition_names = NULL,
                     balance_load = FALSE) {
  
  check_inputs <- check_randomizr_arguments(
    block_var = block_var,
    prob = prob,
    prob_each = prob_each,
    block_m = block_m,
    block_m_each = block_m_each,
    block_prob_each = block_prob_each,
    num_arms = num_arms,
    condition_names = condition_names
  )
  
  blocks <- sort(unique(block_var))
  assign <- rep(NA, length(block_var))
  
  if (!is.null(prob)) {
    prob_each <- c(1 - prob, prob)
  }
  
  # Setup: obtain number of arms and condition_names
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  N_per_block <- check_inputs$N_per_block
  
  # Case 1: block_m is specified
  if (!is.null(block_m)) {
    for (i in 1:length(blocks)) {
      assign[block_var == blocks[i]] <-
        complete_ra(N = N_per_block[i],
                    m = block_m[i],
                    condition_names = condition_names)
    }
    if (!identical(condition_names, c(0, 1))) {
      assign <- condition_names[assign]
    }
    assign <- clean_condition_names(assign, condition_names)
    return(assign)
  }
  
  # Case 2 use or infer prob_each
  if (is.null(block_m_each) & is.null(block_prob_each)) {
    if (is.null(prob_each)) {
      prob_each <- rep(1 / num_arms, num_arms)
    }
    
    if (balance_load) {
      # Get the baseline allocation
      block_m_each <- floor(table(block_var) %*% t(prob_each))
      
      # Figure out the marginals
      n_unassigned <- length(block_var) - sum(block_m_each)
      
      if (sum(n_unassigned) > 0) {
        fixed_column_margin <-
          as.numeric(table(complete_ra(n_unassigned, num_arms = num_arms)))
        fixed_row_margin <- table(block_var) - rowSums(block_m_each)
        block_m_each <-
          update_block_m_each(block_m_each, fixed_row_margin, fixed_column_margin)
      }
      
      assign <-
        block_ra(
          block_var = block_var,
          block_m_each = block_m_each,
          condition_names = condition_names
        )
      return(assign)
    } else{
      for (i in 1:length(blocks)) {
        assign[block_var == blocks[i]] <-
          complete_ra(
            N = N_per_block[i],
            prob_each = prob_each,
            condition_names = condition_names
          )
      }
      if (!identical(condition_names, c(0, 1))) {
        assign <- condition_names[assign]
      }
      assign <- clean_condition_names(assign, condition_names)
      return(assign)
    }
  }
  
  # Case 3 use block_m_each
  
  if (!is.null(block_m_each)) {
    for (i in 1:length(blocks)) {
      assign[block_var == blocks[i]] <-
        complete_ra(N = N_per_block[i],
                    m_each = block_m_each[i, ],
                    condition_names = condition_names)
    }
    if (!identical(condition_names, c(0, 1))) {
      assign <- condition_names[assign]
    }
    assign <- clean_condition_names(assign, condition_names)
    return(assign)
  }
  
  # Case 4 use block_prob_each
  
  if (!is.null(block_prob_each)) {
    if (balance_load) {
      # Get the baseline allocation
      block_m_each <-
        floor(sweep(block_prob_each, 1, table(block_var), `*`))
      
      # Figure out the marginals
      n_unassigned <- length(block_var) - sum(block_m_each)
      
      if (sum(n_unassigned) > 0) {
        fixed_column_margin <-
          as.numeric(table(complete_ra(n_unassigned, num_arms = num_arms)))
        fixed_row_margin <- table(block_var) - rowSums(block_m_each)
        block_m_each <-
          update_block_m_each(block_m_each, fixed_row_margin, fixed_column_margin)
      }
      
      assign <-
        block_ra(
          block_var = block_var,
          block_m_each = block_m_each,
          condition_names = condition_names
        )
      return(assign)
    } else{
      for (i in 1:length(blocks)) {
        assign[block_var == blocks[i]] <-
          complete_ra(
            N = N_per_block[i],
            prob_each = block_prob_each[i, ],
            condition_names = condition_names
          )
      }
      if (!identical(condition_names, c(0, 1))) {
        assign <- condition_names[assign]
      }
      assign <- clean_condition_names(assign, condition_names)
      return(assign)
    }
  }
}


update_block_m_each <-
  function(block_m_each,
           fixed_row_margin,
           fixed_column_margin) {
    # Gather some unique possibilities that only have 1's and 0's
    condition <- FALSE
    while (!condition) {
      draw <-
        r2dtable(n = 1, r = fixed_row_margin, c = fixed_column_margin)[[1]]
      condition <- all(draw %in% c(0, 1))
    }
    return(block_m_each + draw)
  }