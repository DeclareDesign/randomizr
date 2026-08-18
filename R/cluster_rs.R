#' Cluster Random Sampling
#'
#' \code{cluster_rs} draws whole groups of units (clusters) into the sample, so that either every unit in a cluster is sampled or none of them is. Use it when the sampling frame lists groups rather than individuals, for example when villages are drawn and then everyone in the drawn villages is interviewed. Because units come in whole clusters, the effective sample size is closer to the number of clusters than to the number of units.
#'
#' By default the clusters are drawn by complete random sampling, so a fixed number of clusters is sampled on every draw. Setting \code{simple = TRUE} draws each cluster independently instead, using \code{\link{simple_rs}()}.
#'
#' @seealso \code{\link{complete_rs}()}, \code{\link{strata_and_cluster_rs}()}, \code{\link{cluster_ra}()}, \code{\link{cluster_rs_probabilities}()}
#'
#' @param clusters A vector of length N indicating which cluster each unit belongs to. (required)
#' @param n Use for a design in which exactly \code{n} clusters are sampled. (optional)
#' @param n_unit \code{unique(n_unit)} will be passed to \code{n}; must be the same for all units and of length N. (optional)
#' @param prob Use for a design in which either \code{floor(N_clusters*prob)} or \code{ceiling(N_clusters*prob)} clusters are sampled. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N_clusters*prob} and the floor otherwise, which makes each cluster's probability of inclusion exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit \code{unique(prob_unit)} will be passed to \code{prob}; must be the same for all units and of length N. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, clusters are drawn independently (simple random sampling of clusters), so the number of sampled clusters varies from draw to draw. Do not specify \code{n} when \code{simple = TRUE}. (optional)
#' @param check_inputs Logical. Whether to verify before sampling that the arguments are internally consistent: that \code{n} does not exceed the number of clusters, that probabilities lie between 0 and 1, and so on. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many samples from arguments that have already been verified; declaring the design once with \code{\link{declare_rs}()} and drawing from it with \code{\link{draw_rs}()} does this for you. (optional)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0). Every unit in a cluster receives the same value.
#' @export
#' @examples
#' # Ten clusters, of sizes 1 through 10
#' clusters <- rep(letters[1:10], times = 1:10)
#'
#' S <- cluster_rs(clusters = clusters)
#' table(S, clusters)
#'
#' S <- cluster_rs(clusters = clusters, n = 4)
#' table(S, clusters)
#'
#' # Each cluster drawn independently, so the number sampled varies
#' S <- cluster_rs(clusters = clusters, prob = 0.4, simple = TRUE)
#' table(S, clusters)
#'
cluster_rs <- function(clusters = NULL,
                       n = NULL,
                       n_unit = NULL,
                       prob = NULL,
                       prob_unit = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  if (check_inputs){
    .invoke_check(check_samplr_arguments_new)
  }
  
  n_per_clust <- tapply(clusters, clusters, length)
  unique_clust <- names(n_per_clust)
  n_clust <- length(unique_clust)
  
  if (!is.null(prob_unit)) {
    prob_unit <- tapply(prob_unit, INDEX = clusters, FUN = unique)
  }
  
  if (!is.null(n_unit)) {
    n_unit <- tapply(n_unit, INDEX = clusters, FUN = unique)
  }
  
  if (simple) {
    S_clust <-
      simple_rs(N = n_clust,
                prob = prob,
                prob_unit = prob_unit)
    
  } else{
    S_clust <- complete_rs(
      N = n_clust,
      n = n,
      n_unit = n_unit,
      prob = prob,
      prob_unit = prob_unit
    )
  }
  assignment <- rep(S_clust, n_per_clust)
  assignment <-
    assignment[order(unlist(split(seq_along(clusters), clusters), FALSE, FALSE))]
  return(assignment)
}
#' Inclusion probabilities: Cluster Sampling
#'
#' Returns each unit's probability of being sampled when whole clusters are
#' drawn. Every unit in a cluster shares its cluster's probability.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each sampled unit by the reciprocal of its inclusion probability, which
#' \code{\link{obtain_inclusion_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{cluster_rs}()}
#'
#' @inheritParams cluster_rs
#'
#' @return A numeric vector of length N giving each unit's probability of being included in the sample. Every unit in a cluster shares one probability.
#'
#' @examples
#'
#' clusters <- rep(letters[1:10], times = 1:10)
#'
#' probs <- cluster_rs_probabilities(clusters = clusters)
#' table(probs, clusters)
#'
#' probs <- cluster_rs_probabilities(clusters = clusters, n = 4)
#' table(probs, clusters)
#'
#' probs <- cluster_rs_probabilities(clusters = clusters, prob = 0.3)
#' table(probs, clusters)
#'
#'
#' @export
cluster_rs_probabilities <-
  function(clusters = NULL,
           n = NULL,
           n_unit = NULL,
           prob = NULL,
           prob_unit = NULL,
           simple = FALSE,
           check_inputs = TRUE) {
    if (check_inputs)
      .invoke_check(check_samplr_arguments_new)
    
    n_per_clust <- tapply(clusters, clusters, length)
    unique_clust <- names(n_per_clust)
    n_clust <- length(unique_clust)
    
    
    if (!is.null(prob_unit)) {
      prob_unit <- tapply(prob_unit, INDEX = clusters, FUN = unique, simplify = FALSE)
    }
    if (!is.null(n_unit)) {
      n_unit <- tapply(n_unit, INDEX = clusters, FUN = unique, simplify = FALSE)
    }
    
    if (simple) {
      probs_clust <-
        simple_rs_probabilities(N = n_clust,
                                prob = prob,
                                prob_unit = prob_unit)
    } else{
      probs_clust <-
        complete_rs_probabilities(
          N = n_clust,
          n = n,
          n_unit = n_unit,
          prob = prob,
          prob_unit = prob_unit
        )
    }
    
    prob_vec <- rep(probs_clust, n_per_clust)
    prob_vec <-
      prob_vec[order(unlist(split(seq_along(clusters), clusters),
                            FALSE, FALSE))]
    return(prob_vec)
  }
