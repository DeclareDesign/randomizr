#' Obtain the Number of Possible Permutations from a Random Assignment Declaration
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#'
#' @return a scalar
#' @export
#'
#' @examples
#' 
#' # complete
#' 
#' declaration <- declare_ra(N = 4)
#' perms <- obtain_permutation_matrix(declaration)
#' dim(perms)
#' obtain_num_permutations(declaration)
#' 
#' # blocked 
#' 
#' blocks <- c("A", "A", "B", "B", "C", "C", "C")
#' declaration <- declare_ra(blocks = blocks)
#' perms <- obtain_permutation_matrix(declaration)
#' dim(perms)
#' obtain_num_permutations(declaration)
#' 
#' # clustered
#' 
#' clusters <- c("A", "B", "A", "B", "C", "C", "C")
#' declaration <- declare_ra(clusters = clusters)
#' perms <- obtain_permutation_matrix(declaration)
#' dim(perms)
#' obtain_num_permutations(declaration)
#' 
#' # large
#' 
#' declaration <- declare_ra(20)
#' choose(20, 10)
#' perms <- obtain_permutation_matrix(declaration)
#' dim(perms)
#' 
obtain_num_permutations <- function(declaration) {
  (function(declaration) UseMethod("obtain_num_permutations", declaration))(declaration)
}

obtain_num_permutations.ra_simple <- function(declaration) {
  declaration$num_arms ^ (nrow(declaration$probabilities_matrix))
}

obtain_num_permutations.ra_complete <- function(declaration){
  complete_ra_num_permutations(
    N = nrow(declaration$probabilities_matrix),
    prob_each = declaration$probabilities_matrix[1, ],
    conditions = declaration$conditions
  )
}
    
obtain_num_permutations.ra_blocked <- function(declaration){

  block_prob_each_local <-
    by(
      declaration$probabilities_matrix,
      INDICES = declaration$blocks,
      FUN = function(x) {
        x[1, ]
      }
    )
  block_prob_each_local <-
    lapply(block_prob_each_local, as.vector, mode = "numeric")
  
  ns_per_block_list <-
    lapply(split(declaration$blocks,
                 declaration$blocks),
           length)
  
  condition_names_list <- lapply(1:length(ns_per_block_list),
                                 function(x)
                                   declaration$conditions)
  
  num_permutations_by_block <- mapply(FUN = complete_ra_num_permutations,
                                      ns_per_block_list,
                                      block_prob_each_local,
                                      condition_names_list)
  
  num_permutations <-
    prod(unlist(num_permutations_by_block))

  return(num_permutations)      
}
  
obtain_num_permutations.ra_clustered <- function(declaration){
  
  prob_each_local <-
    declaration$probabilities_matrix[1, ]
  
  n_per_clust <-
    tapply(declaration$clusters, declaration$clusters, length)
  n_clust <- length(n_per_clust)
  
    complete_ra_num_permutations(
      N = n_clust,
      prob_each = declaration$probabilities_matrix[1, ],
      conditions = declaration$conditions
    )
  
}
    
obtain_num_permutations.ra_blocked_and_clustered <- function(declaration){

  # Setup: obtain unique clusters
  n_per_clust <-
    tapply(declaration$clusters, declaration$clusters, length)
  n_clust <- length(n_per_clust)
  
  # get the block for each cluster
  clust_blocks <-
    tapply(declaration$blocks, declaration$clusters, unique)
  
  block_prob_each_local <-
    by(
      declaration$probabilities_matrix,
      INDICES = declaration$blocks,
      FUN = function(x) {
        x[1, ]
      }
    )
  block_prob_each_local <-
    lapply(block_prob_each_local, as.vector, mode = "numeric")
  
  ns_per_block_list <-
    lapply(split(clust_blocks,
                 clust_blocks),
           length)
  
  condition_names_list <- lapply(1:length(ns_per_block_list),
                                 function(x)
                                   declaration$conditions)
  
  num_permutations_by_block <- mapply(FUN = complete_ra_num_permutations,
                                      ns_per_block_list,
                                      block_prob_each_local,
                                      condition_names_list)
  
  num_permutations <-
    prod(unlist(num_permutations_by_block))
  
  return(num_permutations)
}
    
obtain_num_permutations.ra_custom <- function(declaration){
    ncol(declaration$permutation_matrix)
}


# Helper functions --------------------------------------------------------


multinomial_coefficient <-
  function(N, m_each) {
    # https://www.statlect.com/mathematical-tools/partitions
    if (N != sum(m_each)) {
      stop("the sum of m_each must equal N.")
    } 
    if (lfactorial(N) > 300){
      return(Inf) # this is a hack!
    }
    factorial(N) / prod(factorial(m_each))
  }

complete_ra_num_permutations <-
  function(N, prob_each, conditions) {
    m_each_floor <- floor(N * prob_each)
    N_floor <- sum(m_each_floor)
    N_remainder <- N - N_floor
    
    if (N_remainder == 0) {
      num_possibilities <-
        multinomial_coefficient(N = N, m_each = m_each_floor)
      
    } else {
      prob_each_fix_up <- ((prob_each * N) - m_each_floor) / N_remainder
      
      fix_ups <-
        expand.grid(replicate(N_remainder, conditions, simplify = FALSE),
                    stringsAsFactors = FALSE)
      fix_ups_probs <-
        c(prob_each_fix_up %*% t(prob_each_fix_up))
      
      fix_up_conditions <- apply(fix_ups, 1, as.character)
      
      if (is.null(dim(fix_up_conditions))) {
        fix_up_conditions <-
          matrix(fix_up_conditions, nrow = 1, byrow = TRUE)
      }
      
      m_eaches <-
        apply(fix_up_conditions, 2, function(x) {
          sapply(conditions, function(i)
            sum(x %in% i)) + m_each_floor
        })
      
      num_possibilities <-
        sum(apply(m_eaches, 2, function(x)
          multinomial_coefficient(N, m_each = x)))
      
    }
    return(num_possibilities)
  }
