#' Stratified and Clustered Random Assignment
#'
#' A random sampling procedure in which units are sampled as clusters and clusters are nested within strata.
#'
#' @param strata_var A vector of length N that indicates which stratum each unit belongs to.
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param prob Use for a design in which either floor(N_clusters_stratum*prob) or ceiling(N_clusters_stratum*prob) clusters are sampled within each stratum. The probability of being sampled is exactly prob because with probability 1-prob, floor(N_clusters_stratum*prob) clusters will be sampled and with probability prob, ceiling(N_clusters_stratum*prob) clusters will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_n Use for a design in which strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of being sampled can vary across strata.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit.
#'
#' @examples
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
#' S <- strata_and_cluster_rs(strata_var = strata_var,
#'                           clust_var = clust_var)
#'
#' table(S, strata_var)
#' table(S, clust_var)
#' 
#' 
#' S <- strata_and_cluster_rs(clust_var = clust_var,
#'                            strata_var = strata_var,
#'                            prob = .5)
#' 
#' table(S, clust_var)
#' table(S, strata_var)
#' 
#' S <- strata_and_cluster_rs(clust_var = clust_var,
#'                            strata_var = strata_var,
#'                            strata_n = c(2, 3, 2, 3, 2))
#' 
#' table(S, clust_var)
#' table(S, strata_var)
#' 
#' S <- strata_and_cluster_rs(clust_var = clust_var,
#'                            strata_var = strata_var,
#'                            strata_prob = c(.1, .2, .3, .4, .5))
#' 
#' table(S, clust_var)
#' table(S, strata_var)
#' 
#'
#' @export
strata_and_cluster_rs <-
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
    
    # Conduct random assignment at cluster level
    S_clust <- strata_rs(
      strata_var = clust_strata,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
    # back up to the individual level, maintaining original ordering
    assignment <- rep(S_clust, n_per_clust)
    assignment <- assignment[order(unlist(split(1:length(clust_var), clust_var), FALSE, FALSE))]
    return(assignment)
  }
