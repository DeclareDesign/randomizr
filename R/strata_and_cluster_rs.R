#' Stratified and Clustered Random Sampling
#'
#' \code{strata_and_cluster_rs} draws whole clusters, sampling separately within
#' each stratum. Use it when the sampling unit is a group rather than an
#' individual and the groups fall into categories you want represented in fixed
#' proportion. Sampling by cluster costs precision, since the effective sample
#' size is the number of clusters rather than the number of units; stratifying
#' buys some of it back by fixing how many clusters come from each stratum.
#'
#' Clusters must nest within strata: every unit in a cluster has to belong to
#' the same stratum.
#'
#' @seealso \code{\link{cluster_rs}()}, \code{\link{strata_rs}()},
#'   \code{\link{block_and_cluster_ra}()}
#'
#' @param strata A vector of length N indicating which stratum each unit belongs to. Every unit in a cluster must belong to the same stratum. (required)
#' @param clusters A vector of length N indicating which cluster each unit belongs to. (required)
#' @param prob Use for a design in which either \code{floor(N_clusters_stratum*prob)} or \code{ceiling(N_clusters_stratum*prob)} clusters are sampled within each stratum. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_clusters_stratum*prob} and the floor otherwise, which makes each cluster's probability of inclusion exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Must be of length N. \code{tapply(prob_unit, strata, unique)} will be passed to \code{strata_prob}, so it must be constant within each stratum. (optional)
#' @param n Use for a design in which the scalar \code{n} gives the fixed number of clusters to sample in every stratum. This count does not vary across strata. (optional)
#' @param n_unit Must be of length N. \code{tapply(n_unit, strata, unique)} will be passed to \code{strata_n}, so it must be constant within each stratum. (optional)
#' @param strata_n Use for a design in which \code{strata_n} gives the number of clusters to sample within each stratum. Must be as long as the number of strata, in the same order as \code{sort(unique(strata))}. (optional)
#' @param strata_prob Use for a design in which \code{strata_prob} gives the probability of being sampled within each stratum. Must be in the same order as \code{sort(unique(strata))}. Differs from \code{prob} in that the probability of being sampled can vary across strata. (optional)
#' @param check_inputs Logical. Whether to verify before sampling that the arguments are internally consistent: that clusters nest within strata, that counts do not exceed the number of clusters in a stratum, that probabilities lie between 0 and 1, and so on. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many samples from arguments that have already been verified; declaring the design once with \code{\link{declare_rs}()} and drawing from it with \code{\link{draw_rs}()} does this for you. (optional)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0). Every unit in a cluster receives the same value.
#'
#' @examples
#' # Twelve clusters, of sizes 1 through 12, nested in four strata of three
#' clusters <- rep(letters[1:12], times = 1:12)
#'
#' strata <- rep(NA, length(clusters))
#' strata[clusters %in% letters[1:3]] <- "stratum_1"
#' strata[clusters %in% letters[4:6]] <- "stratum_2"
#' strata[clusters %in% letters[7:9]] <- "stratum_3"
#' strata[clusters %in% letters[10:12]] <- "stratum_4"
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
#'                            prob = 0.5)
#'
#' table(S, clusters)
#' table(S, strata)
#'
#' S <- strata_and_cluster_rs(clusters = clusters,
#'                            strata = strata,
#'                            strata_n = c(1, 2, 1, 2))
#'
#' table(S, clusters)
#' table(S, strata)
#'
#' S <- strata_and_cluster_rs(clusters = clusters,
#'                            strata = strata,
#'                            strata_prob = c(0.2, 0.4, 0.6, 0.8))
#'
#' table(S, clusters)
#' table(S, strata)
#'
#'
#' @export
strata_and_cluster_rs <-
  function(strata = NULL,
           clusters = NULL,
           prob = NULL,
           prob_unit = NULL,
           n = NULL,
           n_unit = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           check_inputs = TRUE) {
    if (check_inputs)
      .invoke_check(check_samplr_arguments_new)
    
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)
    
    # get the stratum for each cluster
    clust_strata <- tapply(strata, clusters, unique)
    
    if(!is.null(prob_unit)){
      strata_prob <- as.vector(tapply(prob_unit, strata, unique))
    }
    
    if(!is.null(n_unit)){
      strata_n <- as.vector(tapply(n_unit, strata, unique))
    }
    
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
      assignment[order(unlist(split(seq_along(clusters), clusters), 
                              FALSE, FALSE))]
    return(assignment)
  }

#' Inclusion probabilities: Stratified and Clustered Random Sampling
#'
#' Returns each unit's probability of being sampled when clusters are drawn
#' within strata. Probabilities vary across strata and are constant within a
#' cluster.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each sampled unit by the reciprocal of its inclusion probability, which
#' \code{\link{obtain_inclusion_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{strata_and_cluster_rs}()}
#'
#' @inheritParams strata_and_cluster_rs
#'
#' @return A numeric vector of length N giving each unit's probability of being included in the sample. Every unit in a cluster shares one probability.
#'
#' @examples
#'
#' # Twelve clusters, of sizes 1 through 12, nested in four strata of three
#' clusters <- rep(letters[1:12], times = 1:12)
#'
#' strata <- rep(NA, length(clusters))
#' strata[clusters %in% letters[1:3]] <- "stratum_1"
#' strata[clusters %in% letters[4:6]] <- "stratum_2"
#' strata[clusters %in% letters[7:9]] <- "stratum_3"
#' strata[clusters %in% letters[10:12]] <- "stratum_4"
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
#'                                          prob = 0.5)
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#' probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
#'                                          strata = strata,
#'                                          strata_n = c(1, 2, 1, 2))
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#' probs <- strata_and_cluster_rs_probabilities(clusters = clusters,
#'                                          strata = strata,
#'                                          strata_prob = c(0.2, 0.4, 0.6, 0.8))
#'
#' table(probs, clusters)
#' table(probs, strata)
#'
#'
#' @export
strata_and_cluster_rs_probabilities <-
  function(strata = NULL,
           clusters = NULL,
           prob = NULL,
           prob_unit = NULL,
           n = NULL,
           n_unit = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           check_inputs = TRUE) {
    if (check_inputs)
      .invoke_check(check_samplr_arguments_new)
    
    
    # Setup: obtain unique clusters
    n_per_clust <- tapply(clusters, clusters, length)
    
    # get the stratum for each cluster
    clust_strata <- tapply(strata, clusters, unique)
    
    if(!is.null(prob_unit)){
      strata_prob <- as.vector(tapply(prob_unit, strata, unique))
    }
    
    if(!is.null(n_unit)){
      strata_n <- as.vector(tapply(n_unit, strata, unique))
    }
    
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
      prob_vec[order(unlist(split(seq_along(clusters), clusters), FALSE, FALSE))]
    return(prob_vec)
  }
