#' Inclusion Probabilities: Stratified and Clustered Random Sampling
#'
#' @inheritParams strata_and_cluster_rs
#'
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' clusters <- rep(letters, times = 1:26)
#'
#' strata <- rep(NA, length(clusters))
#' strata[clusters %in% letters[1:5]] <- "stratum_1"
#' strata[clusters %in% letters[6:10]] <- "stratum_2"
#' strata[clusters %in% letters[11:15]] <- "stratum_3"
#' strata[clusters %in% letters[16:20]] <- "stratum_4"
#' strata[clusters %in% letters[21:26]] <- "stratum_5"
#'
#' table(strata, clusters)
#'
#' probs <- strata_and_cluster_rs_probabilities(strata = strata,
#'                                          clusters = clusters)
#'
#' table(probs, strata)
#' table(probs, clusters)
#'
#'
#' probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
#'                                          strata = strata,
#'                                          prob = .5)
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#' probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
#'                                          strata = strata,
#'                                          strata_n = c(2, 3, 2, 3, 2))
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#' probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
#'                                          strata = strata,
#'                                          strata_prob = c(.1, .2, .3, .4, .5))
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#'
#' @export
strata_and_cluster_rs_probabilities <-
  function(strata = strata_var,
           clusters = clust_var,
           prob = NULL,
           n = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           check_inputs = TRUE,
           strata_var = NULL,
           clust_var = NULL) {
    warn_deprecated_args(NULL, clust_var, strata_var)
    if (check_inputs) {
      input_check <-
        check_samplr_arguments(
          strata = strata,
          clusters = clusters,
          prob = prob,
          n = n,
          strata_n = strata_n,
          strata_prob = strata_prob
        )
    }
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)
    
    # get the stratum for each cluster
    clust_strata <- tapply(strata, clusters, unique)
    
    probs_clust <- strata_rs_probabilities(
      strata = clust_strata,
      prob = prob,
      n = n,
      strata_n = strata_n,
      strata_prob = strata_prob,
      check_inputs = check_inputs
    )
    
    prob_vec <- rep(probs_clust, n_per_clust)
    prob_vec <-
      prob_vec[order(unlist(split(1:length(clusters), clusters), FALSE, FALSE))]
    return(prob_vec)
  }
