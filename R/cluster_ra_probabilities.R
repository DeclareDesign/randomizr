#' Probabilties of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' # Two Group Designs
#' clust_var <- rep(letters, times=1:26)
#' cluster_ra_probabilities(clust_var=clust_var)
#'
#' cluster_ra_probabilities(clust_var=clust_var, m=10)
#'
#' cluster_ra_probabilities(clust_var=clust_var, m_each = c(9, 17),
#'                          condition_names = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' cluster_ra_probabilities(clust_var=clust_var, num_arms=3)
#' cluster_ra_probabilities(clust_var=clust_var, m_each=c(7, 7, 12))
#'
#' cluster_ra_probabilities(clust_var=clust_var, m_each=c(7, 7, 12),
#'                          condition_names=c("control", "placebo", "treatment"))
#'
#' cluster_ra_probabilities(clust_var=clust_var,
#'                          condition_names=c("control", "placebo", "treatment"))
#'
#' cluster_ra_probabilities(clust_var=clust_var, prob_each = c(.1, .2, .7))
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
           simple = FALSE) {
    unique_clus <- unique(clust_var)
    n_clus <- length(unique_clus)
    
    if (simple) {
      probs_clus <-
        simple_ra_probabilities(
          N = n_clus,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    } else{
      probs_clus <-
        complete_ra_probabilities(
          N = n_clus,
          m = m,
          m_each = m_each,
          prob = prob,
          prob_each = prob_each,
          num_arms = num_arms,
          condition_names = condition_names
        )
    }
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    merged <- merged[order(merged$init_order), ]
    prob_mat <- as.matrix(merged[, colnames(probs_clus)])
    return(prob_mat)
  }
