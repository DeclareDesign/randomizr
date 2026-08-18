

#' Obtain the probabilities of permutations
#'
#' Returns how likely each assignment in the permutation matrix was. Most
#' designs make every possible assignment equally likely, in which case these
#' are all the same and can be ignored. Blocked and clustered designs of unequal
#' size do not, and there the probabilities are needed to weight the
#' randomization distribution correctly.
#'
#' @seealso \code{\link{obtain_permutation_matrix}()},
#'   \code{\link{obtain_num_permutations}()}
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}()}. (required)
#'
#' @return A vector with one entry per possible assignment, giving the probability that the design produces that assignment. The entries sum to 1 and are in the same order as the columns of \code{\link{obtain_permutation_matrix}()}, so the two can be used together.
#' @export
#'
#' @references
#' Andrews, G. E. (1976). \emph{The Theory of Partitions}. Encyclopedia of
#' Mathematics and its Applications, Volume 2. Reading, MA: Addison-Wesley.
#'
#' @examples
#'
#' # A design in which the possible assignments are *not* equally likely: with
#' # N = 5 and prob = 0.51, either 2 or 3 units are treated, and those two cases
#' # do not come up equally often.
#' declaration <- declare_ra(N = 5, prob_each = c(0.49, 0.51))
#'
#' obtain_num_permutations(declaration)
#'
#' perms <- obtain_permutation_matrix(declaration)
#' perm_probs <- obtain_permutation_probabilities(declaration)
#'
#' # perms has one column per possible assignment and perm_probs has one entry
#' # per column, in the same order
#' dim(perms)
#' length(perm_probs)
#'
#' # Each unit's probability of assignment to treatment, according to the
#' # declaration. Recovering these from perms is the check that the two objects
#' # line up.
#' true_probabilities <- declaration$probabilities_matrix[, 2]
#' true_probabilities
#'
#' # The unweighted average across columns is WRONG here: it treats every
#' # assignment as equally likely, which this design does not.
#' rowMeans(perms)
#'
#' # Weighting each column by how likely it is recovers the true probabilities.
#' # The matrix product does the weighted average: row i of perms times
#' # perm_probs sums unit i's treatment indicators weighted by column
#' # probability, which is exactly Pr(unit i treated).
#' perms %*% perm_probs
#'
obtain_permutation_probabilities <- function(declaration) {
  (function(declaration)
    UseMethod("obtain_permutation_probabilities", declaration))(declaration)
}

obtain_permutation_probabilities.ra_simple <-
  function(declaration) {
    N <- nrow(declaration$probabilities_matrix)
    prob_each <- declaration$probabilities_matrix[1,]
    r_parts <- restrictedparts(N, length(prob_each))
    perms <- t(permutations(length(prob_each)))
    
    r_parts_perms3 <- vapply(r_parts, `[`, perms, perms)
    dim(r_parts_perms3) <- local({
      d <- dim(r_parts_perms3)
      c(d[1], prod(d[-1])) # pivot third dimension to columns inplace
    })
    
    m_eaches <- unique(r_parts_perms3, MARGIN = 2)
    probs <-
      sapply(seq_len(ncol(m_eaches)), function(j) {
        prod(prob_each ^ m_eaches[, j])
      })
    
    reps <-
      sapply(seq_len(ncol(m_eaches)), function(j) {
        multinomial_coefficient(N = N, m_each = m_eaches[, j])
      })
    permutation_probabilities <-
      rep(probs, reps)
    permutation_probabilities
    
  }

obtain_permutation_probabilities.ra_complete <-
  function(declaration) {
    complete_ra_permutation_probabilities(
      N = nrow(declaration$probabilities_matrix),
      prob_each = declaration$probabilities_matrix[1,],
      conditions = declaration$conditions
    )
    
  }


obtain_permutation_probabilities.ra_blocked <-
  function(declaration) {
    block_prob_each_local <-
      by(
        declaration$probabilities_matrix,
        INDICES = declaration$blocks,
        FUN = function(x) {
          x[1,]
        }
      )
    block_prob_each_local <-
      lapply(block_prob_each_local, as.vector, mode = "numeric")
    
    ns_per_block_list <-
      lapply(split(declaration$blocks,
                   declaration$blocks),
             length)
    
    condition_names_list <- lapply(seq_along(ns_per_block_list),
                                   function(x)
                                     declaration$conditions)
    
    permutation_probabilities_by_block <-
      mapply(
        FUN = complete_ra_permutation_probabilities,
        ns_per_block_list,
        block_prob_each_local,
        condition_names_list,
        SIMPLIFY = FALSE
      )
    
    permutation_probabilities <-
      Reduce(f = expand_vector, x = permutation_probabilities_by_block)
    permutation_probabilities
    
  }


obtain_permutation_probabilities.ra_clustered <-
  function(declaration) {
    prob_each_local <-
      declaration$probabilities_matrix[1,]
    
    n_per_clust <-
      tapply(declaration$clusters, declaration$clusters, length)
    n_clust <- length(n_per_clust)
    
    permutation_probabilities <-
      complete_ra_permutation_probabilities(
        N = n_clust,
        prob_each = declaration$probabilities_matrix[1,],
        conditions = declaration$conditions
      )
    names(permutation_probabilities) <- NULL
    permutation_probabilities
  }

obtain_permutation_probabilities.ra_blocked_and_clustered <-
  function(declaration) {
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
          x[1,]
        }
      )
    block_prob_each_local <-
      lapply(block_prob_each_local, as.vector, mode = "numeric")
    
    ns_per_block_list <-
      lapply(split(clust_blocks,
                   clust_blocks),
             length)
    
    condition_names_list <- lapply(seq_along(ns_per_block_list),
                                   function(x)
                                     declaration$conditions)
    
    permutation_probabilities_by_block <-
      mapply(
        FUN = complete_ra_permutation_probabilities,
        ns_per_block_list,
        block_prob_each_local,
        condition_names_list,
        SIMPLIFY = FALSE
      )
    
    permutation_probabilities <-
      Reduce(f = expand_vector, x = permutation_probabilities_by_block)
    
    permutation_probabilities
  }



# Helper functions --------------------------------------------------------

expand_vector <-
  function(vec_1, vec_2) {
    return(c(vec_1 %*% t(vec_2)))
  }

exponentiate_vector <- function(vec, power) {
  if (power == 1) {
    return(vec)
  }
  c(tcrossprod(Recall(vec, power - 1), vec))
}

complete_ra_permutation_probabilities <-
  function(N, prob_each, conditions) {
    m_each_floor <- floor(N * prob_each)
    N_floor <- sum(m_each_floor)
    N_remainder <- N - N_floor
    
    if (N_remainder == 0) {
      num_permutations <-
        multinomial_coefficient(N, m_each = m_each_floor)
      
      permutation_probabilities <-
        rep(1 / num_permutations, num_permutations)
      
    } else {
      prob_each_fix_up <- ((prob_each * N) - m_each_floor) / N_remainder
      
      fix_ups <-
        expand.grid(replicate(N_remainder, conditions, simplify = FALSE),
                    stringsAsFactors = FALSE)
      
      fix_ups_probs <-
        exponentiate_vector(prob_each_fix_up, power = ncol(fix_ups))
      
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
        apply(m_eaches, 2, multinomial_coefficient, N = N)
      
      permutation_probabilities <-
        rep(fix_ups_probs / num_possibilities, num_possibilities)
      
    }
    return(permutation_probabilities)
  }
