
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
                                   block_prob_each = NULL,
                                   num_arms = NULL,
                                   condition_names = NULL,
                                   balance_load = FALSE) {
  #if(all(balance_load & is.null(prob) & is.null(prob_each) & is.null(block_prob_each))){
  #  stop("If you use the experimental feature 'balance_load', then you must specify one of prob, prob_each, or block_prob_each.")
  #}
  
  
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
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  N_per_block <- check_inputs$N_per_block
  
  blocks <- sort(unique(block_var))
  prob_mat <- matrix(
    NA,
    nrow = length(block_var),
    ncol = length(condition_names),
    dimnames = list(NULL,  paste0("prob_", condition_names))
  )
  
  
  # Case 1 use block_m
  
  if (!is.null(block_m)) {
    for (i in 1:length(blocks)) {
      prob_mat[block_var == blocks[i],] <-
        complete_ra_probabilities(N = N_per_block[i],
                                  m = block_m[i],
                                  condition_names =
                                    condition_names)
    }
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
    
    if (balance_load) {
      for_sure_allocations <- floor(N_per_block %*% t(prob_each))
      possible_allocations <-
        matrix(1 / num_arms,
               nrow = length(N_per_block),
               ncol = num_arms)
      
      for_sure_probs <-
        sweep(for_sure_allocations, 1, N_per_block, `/`)
      possible_probs <-
        sweep(possible_allocations,
              1,
              (N_per_block - rowSums(for_sure_allocations)) / N_per_block,
              `*`)
      final_probs <- for_sure_probs + possible_probs
      
      for (i in 1:length(blocks)) {
        dimensions <- dim(prob_mat[block_var == blocks[i], , drop = FALSE])
        prob_mat[block_var == blocks[i],] <-
          matrix(
            final_probs[i,],
            nrow = dimensions[1],
            ncol = dimensions[2],
            byrow = TRUE
          )
      }
      return(prob_mat)
    } else{
      for (i in 1:length(blocks)) {
        prob_mat[block_var == blocks[i],] <-
          complete_ra_probabilities(
            N = N_per_block[i],
            prob_each = prob_each,
            condition_names =
              condition_names
          )
      }
      return(prob_mat)
    }
    
  }
  
  # Case 2 use block_m_each
  
  if (!is.null(block_m_each)) {
    for (i in 1:length(blocks)) {
      prob_mat[block_var == blocks[i],] <-
        complete_ra_probabilities(N = N_per_block[i],
                                  m_each = block_m_each[i,],
                                  condition_names =
                                    condition_names)
    }
    return(prob_mat)
  }
  
  
  # Case 3 use block_prob_each
  
  if (!is.null(block_prob_each)) {
    if (balance_load) {
      for_sure_allocations <-
        floor(sweep(block_prob_each, 1, table(block_var), `*`))
      possible_allocations <-
        matrix(1 / num_arms,
               nrow = length(N_per_block),
               ncol = num_arms)
      
      for_sure_probs <-
        sweep(for_sure_allocations, 1, N_per_block, `/`)
      possible_probs <-
        sweep(possible_allocations,
              1,
              (N_per_block - rowSums(for_sure_allocations)) / N_per_block,
              `*`)
      final_probs <- for_sure_probs + possible_probs
      
      for (i in 1:length(blocks)) {
        dimensions <- dim(prob_mat[block_var == blocks[i],])
        prob_mat[block_var == blocks[i],] <-
          matrix(
            final_probs[i,],
            nrow = dimensions[1],
            ncol = dimensions[2],
            byrow = TRUE
          )
      }
      return(prob_mat)
      
    } else{
      for (i in 1:length(blocks)) {
        prob_mat[block_var == blocks[i],] <-
          complete_ra_probabilities(
            N = N_per_block[i],
            prob_each = block_prob_each[i,],
            condition_names =
              condition_names
          )
      }
      return(prob_mat)
    }
  }
  
  
}
