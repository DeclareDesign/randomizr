#' Custom Random Assignment
#'
#' Draws one column of a permutation matrix at random, with equal probability.
#' A permutation matrix enumerates the assignments a design can produce, one
#' column per possible assignment, so choosing a column uniformly conducts the
#' design. Used to back \code{declare_ra(permutation_matrix = )}, which is how
#' randomizr supports designs that no other function describes.
#'
#' @param permutation_matrix A matrix with one row per unit and one column per
#'   possible assignment, as produced by
#'   \code{\link{obtain_permutation_matrix}()}.
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by conditions) in a multi-arm trial.
#'
#' @noRd
custom_ra <- function(permutation_matrix) {
  
  permutation_matrix[ , sample.int(ncol(permutation_matrix), 1)]
}

#' Probabilities of assignment: Custom Random Assignment
#'
#' Reads the assignment probabilities straight off a permutation matrix by
#' counting, for each unit, the share of columns placing it in each condition.
#' Exact rather than simulated, since the matrix enumerates every assignment
#' the design can produce.
#'
#' @inheritParams custom_ra
#' @return A matrix of probabilities of assignment, one row per unit and one
#'   column per condition, with columns named \code{prob_<condition>}.
#'
#' @noRd
custom_ra_probabilities <- function(permutation_matrix) {
  P <- as.factor(permutation_matrix)
  dim(P) <- dim(permutation_matrix)
  
  lvl <- levels(P)
  
  P <- apply(P, 1, tabulate, nlevels(P))  
  
  rownames(P) <- paste0("prob_", lvl)
  P <- prop.table(P, 2)
  t(P)  
}
