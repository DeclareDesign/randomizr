#' Probabilties of assignment: Stratified Cluster Random Assignment
#'
#' @inheritParams strata_and_cluster_rs_probabilities
#'
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#'
#' clust_var <- rep(letters, times = 1:26)
#'
#' strata_var <- rep(NA, length(clust_var))
#' strata_var[clust_var %in% letters[1:5]] <- "stratum_1"
#' strata_var[clust_var %in% letters[6:10]] <- "stratum_2"
#' strata_var[clust_var %in% letters[11:15]] <- "stratum_3"
#' strata_var[clust_var %in% letters[16:20]] <- "stratum_4"
#' strata_var[clust_var %in% letters[21:26]] <- "stratum_5"
#'
#' table(strata_var, clust_var)
#'
#' probs <- strata_and_cluster_rs_probabilities(strata_var = strata_var,
#'                                          clust_var = clust_var)
#'
#' table(probs, strata_var)
#' table(probs, clust_var)
#'
#'
#' probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
#'                                          strata_var = strata_var,
#'                                          prob = .5)
#'
#' table(probs, clust_var)
#' table(probs, strata_var)
#'
#' probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
#'                                          strata_var = strata_var,
#'                                          strata_m = c(2, 3, 2, 3, 2))
#'
#' table(probs, clust_var)
#' table(probs, strata_var)
#'
#' probs <- strata_and_cluster_rs_probabilities(clust_var = clust_var,
#'                                          strata_var = strata_var,
#'                                          strata_prob = c(.1, .2, .3, .4, .5))
#'
#' table(probs, clust_var)
#' table(probs, strata_var)
#'
#'
#' @export
strata_and_cluster_rs_probabilities <-
  function(strata_var,
           clust_var,
           prob = NULL,
           strata_m = NULL,
           strata_prob = NULL,
           balance_load = FALSE) {
    unique_clus <- unique(clust_var)
    
    ## get the strata for each cluster
    clust_strata <- rep(NA, length(unique_clus))
    for (i in 1:length(unique_clus)) {
      clust_strata[i] <- unique(strata_var[clust_var == unique_clus[i]])
    }
    
    probs_clus <- strata_rs_probabilities(
      strata_var = clust_strata,
      prob = prob,
      strata_m = strata_m,
      strata_prob = strata_prob,
      balance_load = balance_load
    )
    
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    
    merged <- merged[order(merged$init_order), ]
    prob_vec <- merged$probs_clus
    return(prob_vec)
  }
