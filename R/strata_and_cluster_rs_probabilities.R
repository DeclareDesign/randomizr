#' Probabilties of assignment: Stratified Cluster Random Assignment
#'
#' @inheritParams strata_and_cluster_rs
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
#'                                          strata_n = c(2, 3, 2, 3, 2))
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
           strata_n = NULL,
           strata_prob = NULL,
           check_inputs = TRUE) {
    
    if(check_inputs){
      
      check_inputs <-
        check_samplr_arguments(
          strata_var = strata_var,
          clust_var = clust_var,
          prob = prob,
          strata_n = strata_n,
          strata_prob = strata_prob
        )
    }
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clust_var, clust_var, length)
    
    # get the stratum for each cluster
    clust_strata <- tapply(strata_var, clust_var, unique)
    
    probs_clust <- strata_rs_probabilities(
      strata_var = clust_strata,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
    prob_vec <- rep(probs_clust, n_per_clust)
    prob_vec <- prob_vec[order(unlist(split(1:length(clust_var),clust_var), FALSE, FALSE))]
    return(prob_vec)
  }
