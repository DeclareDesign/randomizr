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
