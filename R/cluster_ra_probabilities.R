#' Probabilties of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' # Two Group Designs
#' clust_var <- rep(letters, times = 1:26)
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var, m = 10)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var,
#'                                      m_each = c(9, 17),
#'                                      condition_names = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var, num_arms = 3)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var, m_each = c(7, 7, 12))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var, m_each = c(7, 7, 12),
#'                          condition_names=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var,
#'                          condition_names=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clust_var = clust_var,
#'                                      prob_each = c(.1, .2, .7))
#' head(prob_mat)
#'
#'
#'
#' @export
cluster_ra_probabilities <-
  function(clust_var,
           m = NULL,
           m_each = NULL,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           simple = FALSE,
           check_inputs = TRUE) {
    if (check_inputs) {
      check_inputs <-
        check_randomizr_arguments(
          clust_var = clust_var,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    }
    n_per_clust <- tapply(clust_var, clust_var, length)
    unique_clust <- names(n_per_clust)
    n_clust <- length(unique_clust)
    
    if (simple) {
      probs_clust <-
        simple_ra_probabilities(
          N = n_clust,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    } else{
      probs_clust <-
        complete_ra_probabilities(
          N = n_clust,
          m = m,
          m_each = m_each,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    }
    prob_mat <-
      probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <-
      prob_mat[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }
