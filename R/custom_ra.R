#' Custom Random Assignment
#'
#' TODO
#'
#' @param permutation_matrix deprecated
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by conditions) in a multi-arm trial.
#'
#' @examples
#' # TODO
custom_ra <- function(permutation_matrix) {
  
  permutation_matrix[ , sample.int(ncol(permutation_matrix), 1)]
}

