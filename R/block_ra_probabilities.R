

#' Probabilties of assignment: Block Random Assignment
#'
#' @inheritParams block_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' block_var <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' prob_mat <- block_ra_probabilities(block_var = block_var)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#'
#' prob_mat <- block_ra_probabilities(block_var = block_var, block_m_each = block_m_each)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#'
#' prob_mat <- block_ra_probabilities(block_var = block_var,
#'                                    block_m_each = block_m_each,
#'                                    condition_names = c("control", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(block_var = block_var, num_arms = 3)
#' head(prob_mat)
#'
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' prob_mat <- block_ra_probabilities(block_var = block_var, block_m_each = block_m_each)
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(block_var=block_var, block_m_each=block_m_each,
#'                        condition_names=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- block_ra_probabilities(block_var=block_var, prob_each=c(.1, .1, .8))
#' head(prob_mat)
#'
#' @export
block_ra_probabilities <- function(block_var,
                                   prob = NULL,
                                   prob_each = NULL,
                                   block_m = NULL,
                                   block_m_each = NULL,
                                   block_prob = NULL,
                                   block_prob_each = NULL,
                                   num_arms = NULL,
                                   condition_names = NULL,
                                   check_inputs = TRUE) {
  if (check_inputs) {
    check_inputs <- check_randomizr_arguments(
      block_var = block_var,
      prob = prob,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      num_arms = num_arms,
      condition_names = condition_names
    )
    
  }
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  N_per_block <- check_inputs$N_per_block
  
  block_spots <-
    unlist(split(1:length(block_var), block_var), FALSE, FALSE)
  
  blocks <- sort(unique(block_var))
  prob_mat <- matrix(
    NA,
    nrow = length(block_var),
    ncol = length(condition_names),
    dimnames = list(NULL,  paste0("prob_", condition_names))
  )
  
  
  # Case 1 use block_m
  
  if (!is.null(block_m)) {
    prob_mat_list <-
      mapply(
        FUN = complete_ra_probabilities,
        N = N_per_block,
        m = block_m,
        MoreArgs = list(
          condition_names = condition_names,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    prob_mat <- do.call(rbind, prob_mat_list)
    prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
    
    return(prob_mat)
  }
  
  # Case 1.5 use block_prob
  
  if (!is.null(block_prob)) {
    prob_mat_list <-
      mapply(
        FUN = complete_ra_probabilities,
        N = N_per_block,
        prob = block_prob,
        MoreArgs = list(
          condition_names = condition_names,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    prob_mat <- do.call(rbind, prob_mat_list)
    prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
    
    return(prob_mat)
    return(prob_mat)
  }
  
  # Case 2 use or infer prob_each
  if (is.null(block_m_each) & is.null(block_prob_each)) {
    if (!is.null(prob)) {
      prob_each <- c(1 - prob, prob)
    }
    
    if (is.null(prob_each)) {
      prob_each <- rep(1 / num_arms, num_arms)
    }
    
    prob_mat_list <-
      mapply(
        FUN = complete_ra_probabilities,
        N = N_per_block,
        MoreArgs = list(
          prob_each = prob_each,
          condition_names = condition_names,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    prob_mat <- do.call(rbind, prob_mat_list)
    prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
    
    return(prob_mat)
    
  }
  
  # Case 2 use block_m_each
  
  if (!is.null(block_m_each)) {
    block_m_each_list <-
      split(block_m_each, rep(1:nrow(block_m_each), times = ncol(block_m_each)))
    
    prob_mat_list <-
      mapply(
        FUN = complete_ra_probabilities,
        N = N_per_block,
        m_each = block_m_each_list,
        MoreArgs = list(
          condition_names = condition_names,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    prob_mat <- do.call(rbind, prob_mat_list)
    prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
    
    return(prob_mat)
  }
  
  
  # Case 3 use block_prob_each
  
  if (!is.null(block_prob_each)) {
    block_prob_each_list <-
      split(block_prob_each, rep(1:nrow(block_prob_each), times = ncol(block_prob_each)))
    
    prob_mat_list <-
      mapply(
        FUN = complete_ra_probabilities,
        N = N_per_block,
        prob_each = block_prob_each_list,
        MoreArgs = list(
          condition_names = condition_names,
          num_arms = num_arms,
          check_inputs = FALSE
        ),
        SIMPLIFY = FALSE
      )
    
    prob_mat <- do.call(rbind, prob_mat_list)
    prob_mat <- prob_mat[order(block_spots), , drop = FALSE]
    
    return(prob_mat)
  }
  
  
}
