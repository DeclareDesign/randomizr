#' Obtain Permutation Matrix from a Random Assignment Declaration
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @param maximum_permutations If the number of possible random assignments exceeds maximum_permutations, obtain_permutation_matrix will return a random sample of maximum_permutations permutations. Defaults to 10,000.
#'
#' @return a matrix of all possible (or a random sample of all possible) random assignments consistent with a declaration.
#' @importFrom utils combn
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
#'
obtain_permutation_matrix <-
  function(declaration, maximum_permutations = 10000) {
    if (inherits(declaration,  "ra_custom") || inherits(declaration,  "rs_custom")) {
      return(declaration$permutation_matrix)
    }
    
    num_permutations <- obtain_num_permutations(declaration)
    
    if (num_permutations > maximum_permutations && inherits(declaration, "ra_declaration")) {
      return(replicate(maximum_permutations, conduct_ra(declaration)))
    }
    
    if (num_permutations > maximum_permutations && inherits(declaration, "rs_declaration")) {
      return(replicate(maximum_permutations, draw_rs(declaration)))
    }
    
    (function(declaration)
      UseMethod("obtain_permutation_matrix", declaration))(declaration)
    
  }

obtain_permutation_matrix.ra_simple <- function(declaration) {
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
  
  perms_list <- sapply(seq_len(ncol(m_eaches)), function(j) {
    permutations_m_each(m_each = m_eaches[, j], declaration$conditions)
  })
  
  perms <- do.call(cbind, perms_list)
  perms
}

obtain_permutation_matrix.ra_complete <- function(declaration) {
  complete_ra_permutations(
    N = nrow(declaration$probabilities_matrix),
    prob_each = declaration$probabilities_matrix[1, ],
    conditions = declaration$conditions
  )
}

obtain_permutation_matrix.ra_blocked <- function(declaration) {
  block_spots <-
    unlist(split(seq_along(declaration$blocks), declaration$blocks), 
           FALSE, FALSE)
  
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
  
  condition_names_list <- lapply(seq_along(ns_per_block_list),
                                 function(x)
                                   declaration$conditions)
  
  perms_by_block <- mapply(
    FUN = complete_ra_permutations,
    ns_per_block_list,
    block_prob_each_local,
    condition_names_list,
    SIMPLIFY = FALSE
  )
  
  perms <-
    Reduce(expand_matrix, x = perms_by_block)
  
  perms <- perms[order(block_spots),]
  perms
}

obtain_permutation_matrix.ra_clustered <- function(declaration) {
  prob_each_local <-
    declaration$probabilities_matrix[1, ]
  
  n_per_clust <-
    tapply(declaration$clusters, declaration$clusters, length)
  n_clust <- length(n_per_clust)
  
  perms <-
    complete_ra_permutations(
      N = n_clust,
      prob_each = declaration$probabilities_matrix[1, ],
      conditions = declaration$conditions
    )
  
  # expand
  perms <- perms[rep(1:n_clust, n_per_clust), ]
  # arrange
  perms <-
    perms[order(unlist(split(
      seq_along(declaration$clusters), declaration$clusters
    ), FALSE, FALSE)),]
  perms
}

obtain_permutation_matrix.ra_blocked_and_clustered <-
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
          x[1, ]
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
    
    perms_by_block <- mapply(
      FUN = complete_ra_permutations,
      ns_per_block_list,
      block_prob_each_local,
      condition_names_list,
      SIMPLIFY = FALSE
    )
    
    perms <-
      Reduce(expand_matrix, x = perms_by_block)
    
    # arrange by blocks
    block_spots <-
      unlist(split(seq_along(clust_blocks), clust_blocks), FALSE, FALSE)
    
    perms <- perms[order(block_spots),]
    
    # expand
    perms <- perms[rep(1:n_clust, n_per_clust),]
    
    # arrange
    perms <-
      perms[order(unlist(split(
        seq_along(declaration$clusters), declaration$clusters
      ), FALSE, FALSE)),]
    perms
  }


obtain_permutation_matrix.rs_simple <- function(declaration) {
  
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
  
  perms_list <- sapply(seq_len(ncol(m_eaches)), function(j) {
    permutations_m_each(m_each = m_eaches[, j], declaration$conditions)
  })
  
  perms <- do.call(cbind, perms_list)
  perms
}

obtain_permutation_matrix.rs_complete <- function(declaration) {
  complete_ra_permutations(
    N = nrow(declaration$probabilities_matrix),
    prob_each = declaration$probabilities_matrix[1, ],
    conditions = declaration$conditions
  )
}

obtain_permutation_matrix.rs_stratified <- function(declaration) {
  
  stratum_spots <-
    unlist(split(seq_along(declaration$strata), declaration$strata), 
           FALSE, FALSE)
  
  stratum_prob_each_local <-
    by(
      declaration$probabilities_matrix,
      INDICES = declaration$strata,
      FUN = function(x) {
        x[1, ]
      }
    )
  stratum_prob_each_local <-
    lapply(stratum_prob_each_local, as.vector, mode = "numeric")
  
  ns_per_stratum_list <-
    lapply(split(declaration$strata,
                 declaration$strata),
           length)
  
  condition_names_list <- lapply(seq_along(ns_per_stratum_list),
                                 function(x)
                                   declaration$conditions)
  
  perms_by_stratum <- mapply(
    FUN = complete_ra_permutations,
    ns_per_stratum_list,
    stratum_prob_each_local,
    condition_names_list,
    SIMPLIFY = FALSE
  )
  
  perms <-
    Reduce(expand_matrix, x = perms_by_stratum)
  
  perms <- perms[order(stratum_spots),]
  perms
}

obtain_permutation_matrix.rs_clustered <- function(declaration) {
  
  prob_each_local <-
    declaration$probabilities_matrix[1, ]
  
  n_per_clust <-
    tapply(declaration$clusters, declaration$clusters, length)
  n_clust <- length(n_per_clust)
  
  perms <-
    complete_ra_permutations(
      N = n_clust,
      prob_each = declaration$probabilities_matrix[1, ],
      conditions = declaration$conditions
    )
  
  # expand
  perms <- perms[rep(1:n_clust, n_per_clust), ]
  # arrange
  perms <-
    perms[order(unlist(split(
      seq_along(declaration$clusters), declaration$clusters
    ), FALSE, FALSE)),]
  perms
}

obtain_permutation_matrix.rs_stratified_and_clustered <-
  function(declaration) {
    # Setup: obtain unique clusters
    n_per_clust <-
      tapply(declaration$clusters, declaration$clusters, length)
    n_clust <- length(n_per_clust)
    
    # get the stratum for each cluster
    clust_strata <-
      tapply(declaration$strata, declaration$clusters, unique)
    
    stratum_prob_each_local <-
      by(
        declaration$probabilities_matrix,
        INDICES = declaration$strata,
        FUN = function(x) {
          x[1, ]
        }
      )
    stratum_prob_each_local <-
      lapply(stratum_prob_each_local, as.vector, mode = "numeric")
    
    ns_per_stratum_list <-
      lapply(split(clust_strata,
                   clust_strata),
             length)
    
    condition_names_list <- lapply(seq_along(ns_per_stratum_list),
                                   function(x)
                                     declaration$conditions)
    
    perms_by_stratum <- mapply(
      FUN = complete_ra_permutations,
      ns_per_stratum_list,
      stratum_prob_each_local,
      condition_names_list,
      SIMPLIFY = FALSE
    )
    
    perms <-
      Reduce(expand_matrix, x = perms_by_stratum)
    
    # arrange by strata
    stratum_spots <-
      unlist(split(seq_along(clust_strata), clust_strata), FALSE, FALSE)
    
    perms <- perms[order(stratum_spots),]
    
    # expand
    perms <- perms[rep(1:n_clust, n_per_clust),]
    
    # arrange
    perms <-
      perms[order(unlist(split(
        seq_along(declaration$clusters), declaration$clusters
      ), FALSE, FALSE)),]
    perms
  }



# Helper functions --------------------------------------------------------


# https://stackoverflow.com/a/20199902/4172083
permutations <- function(n) {
  if (n == 1) {
    return(matrix(1))
  } else {
    sp <- permutations(n - 1)
    p <- nrow(sp)
    A <- matrix(nrow = n * p, ncol = n)
    for (i in 1:n) {
      A[(i - 1) * p + 1:p,] <- cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}

complete_ra_permutations <- function(N, prob_each, conditions) {
  m_each_floor <- floor(N * prob_each)
  N_floor <- sum(m_each_floor)
  N_remainder <- N - N_floor
  
  if (N_remainder == 0) {
    return(permutations_m_each(m_each_floor, conditions))
  }
  
  prob_each_fix_up <- ((prob_each * N) - m_each_floor) / N_remainder
  
  # Matrix of each combn of expand.grid(1:k, 1:k, 1:k, ...)
  k <- length(conditions)
  fix_ups <- matrix(0, nrow = k ^ N_remainder, ncol = N_remainder)
  for (i in 1:N_remainder) {
    fix_ups[, i] <- rep(1:k, each = k ^ (i - 1))
  }
  
  m_each_es <- apply(fix_ups, 1, tabulate, nbins = k) + m_each_floor
  
  perms <-
    lapply(seq_len(ncol(m_each_es)), function(i)
      permutations_m_each(m_each_es[, i], conditions))
  
  perms <- do.call(cbind, perms)
  
  perms
}

permutations_m_each <- function(m_each, conditions) {
  conditions <- conditions[m_each > 0]
  m_each <- m_each[m_each > 0]
  
  N <- sum(m_each)
  
  k <- length(m_each)
  
  
  if (k == 0)
    return(matrix(NA, 0, 0))
  if (k == 1)
    return(matrix(conditions[1], N, 1))
  
  # initialize matrix output to NA of appropriate class (int, double, character, etc)
  my_na <- local({
    conditions[1]  <- NA
    conditions[1]
  })
  
  # Preinitialize output matrix to correct size
  n_combs <- prod(choose(rev(cumsum(rev(
    m_each
  ))), m_each))
  out <- matrix(my_na, N, n_combs)
  
  # First pass is easy - use combn callback directly
  j <- 0
  
  combn(N, m_each[1], function(i) {
    j <<- j + 1
    out[i, j] <<- conditions[1]
    NULL
  }, FALSE)
  
  for (x in seq(k - 1)[-1]) {
    local_positions <-
      combn(sum(m_each[x:k]), m_each[x], simplify = FALSE)
    
    J <- j # Last column touched
    K <- length(local_positions)
    
    # each combination of out / local_pos using rep(each) rep(times) idiom
    out[, seq(J * K)] <- out[, rep(1:J, each = K), drop = FALSE]
    local_positions <- local_positions[rep(1:K, times = J)]
    
    for (j in seq(J * K)) {
      available <- which(is.na(out[, j]))
      which_row <- available[local_positions[[j]]]
      out[which_row, j] <- conditions[x]
    }
  }
  
  # Final pass is easy
  out[is.na(out)] <- conditions[k]
  
  out
}



expand_matrix <- function(mat_1, mat_2) {
  # by column:
  #  out = [  M1_1 M1_2 M1_3 ... M1_k M1_1 M1_2 M1_3 ... M1_k
  #           M2_1 M2_1 M2_1 ... M2_1 M2_2 M2_2 M2_2 ... M2_k ]
  
  out <-
    matrix(0L, nrow(mat_1) + nrow(mat_2), ncol(mat_1) * ncol(mat_2))
  
  out[seq_len(nrow(mat_1)),] <- mat_1
  out[nrow(mat_1) + seq_len(nrow(mat_2)), ] <-
    mat_2[, rep(seq_len(ncol(mat_2)), each = ncol(mat_1))]
  
  out
}

#' @useDynLib randomizr
restrictedparts <-
  function(n, m) {
    .Call("randomizr_restrictedparts",
          as.integer(n),
          as.integer(m),
          PACKAGE = "randomizr")
  }


vsample <-
  function(pmat) {
    .Call("randomizr_vsample", pmat, PACKAGE = "randomizr")
  }
