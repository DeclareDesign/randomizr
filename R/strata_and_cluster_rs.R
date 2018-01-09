#' Stratified and Clustered Random Sampling
#'
#' A random sampling procedure in which units are sampled as clusters and clusters are nested within strata.
#'
#' @param strata A vector of length N that indicates which stratum each unit belongs to.
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param prob Use for a design in which either floor(N_clusters_stratum*prob) or ceiling(N_clusters_stratum*prob) clusters are sampled within each stratum. The probability of being sampled is exactly prob because with probability 1-prob, floor(N_clusters_stratum*prob) clusters will be sampled and with probability prob, ceiling(N_clusters_stratum*prob) clusters will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param n Use for a design in which the scalar n describes the fixed number of units to sample in each stratum. This number does not vary across strata.
#' @param strata_n Use for a design in which strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of being sampled can vary across strata.
#' @param check_inputs logical. Defaults to TRUE.
#' @param strata_var deprecated
#' @param clust_var deprecated
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#'
#' @examples
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
#' S <- strata_and_cluster_rs(strata = strata,
#'                           clusters = clusters)
#'
#' table(S, strata)
#' table(S, clusters)
#'
#'
#' S <- strata_and_cluster_rs(clusters = clusters,
#'                            strata = strata,
#'                            prob = .5)
#'
#' table(S, clusters)
#' table(S, strata)
#'
#' S <- strata_and_cluster_rs(clusters = clusters,
#'                            strata = strata,
#'                            strata_n = c(2, 3, 2, 3, 2))
#'
#' table(S, clusters)
#' table(S, strata)
#'
#' S <- strata_and_cluster_rs(clusters = clusters,
#'                            strata = strata,
#'                            strata_prob = c(.1, .2, .3, .4, .5))
#'
#' table(S, clusters)
#' table(S, strata)
#'
#'
#' @export
strata_and_cluster_rs <-
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
    
    # Conduct random assignment at cluster level
    S_clust <- strata_rs(
      strata = clust_strata,
      prob = prob,
      n = n,
      strata_n = strata_n,
      strata_prob = strata_prob,
      check_inputs = check_inputs
    )
    
    # back up to the individual level, maintaining original ordering
    assignment <- rep(S_clust, n_per_clust)
    assignment <-
      assignment[order(unlist(split(1:length(clusters), clusters), FALSE, FALSE))]
    return(assignment)
  }
