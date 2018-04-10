#' Custom Random Assignment
#'
#' TODO
#'
#' @param permutation_matrix A permutation matrix
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by conditions) in a multi-arm trial.
#'
#' @examples
#' # TODO
custom_ra <- function(permutation_matrix) {
  
  permutation_matrix[ , sample.int(ncol(permutation_matrix), 1)]
}

#' probabilities of assignment: Custom Random Assignment
#'
#' @inheritParams custom_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # TODO
custom_ra_probabilities <- function(permutation_matrix) {
  P <- as.factor(permutation_matrix)
  dim(P) <- dim(permutation_matrix)
  
  lvl <- levels(P)
  
  P <- apply(P, 1, tabulate, nlevels(P))  
  
  rownames(P) <- paste0("prob_", lvl)
  P <- prop.table(P, 2)
  t(P)  
}
