#' probabilities of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' # Two Group Designs
#' clusters <- rep(letters, times = 1:26)
#' prob_mat <- cluster_ra_probabilities(clusters = clusters)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m = 10)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      m_each = c(9, 17),
#'                                      conditions = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, num_arms = 3)
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters, m_each = c(7, 7, 12),
#'                          conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                          conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- cluster_ra_probabilities(clusters = clusters,
#'                                      prob_each = c(.1, .2, .7))
#' head(prob_mat)
#'
#'
#'
#' @export
cluster_ra_probabilities <-
  function(clusters = clust_var,
           m = NULL,
           m_each = NULL,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           conditions = NULL,
           simple = FALSE,
           check_inputs = TRUE,
           clust_var = NULL) {
  
    warn_deprecated_args(clust_var=clust_var)
    
    
    if (check_inputs) {
      input_check <-
        check_randomizr_arguments(
          clusters = clusters,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          conditions = conditions
        )
    }
    n_per_clust <- tapply(clusters, clusters, length)
    unique_clust <- names(n_per_clust)
    n_clust <- length(unique_clust)
    
    if (simple) {
      probs_clust <-
        simple_ra_probabilities(
          N = n_clust,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          conditions = conditions,
          check_inputs = check_inputs
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
          conditions = conditions,
          check_inputs = check_inputs
        )
    }
    prob_mat <-
      probs_clust[rep(1:n_clust, n_per_clust), , drop = FALSE]
    prob_mat <-
      prob_mat[order(unlist(split(1:length(clusters), clusters), FALSE, FALSE)), , drop = FALSE]
    return(prob_mat)
  }
