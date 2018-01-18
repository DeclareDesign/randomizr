#' Declare a random sampling procedure.
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param strata A vector of length N that indicates which stratum each unit belongs to.
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param n Use for a design in which n units (or clusters) are sampled. In a stratified design, exactly n units in each stratum will be sampled. (optional)
#' @param prob Use for a design in which either floor(N*prob) or ceiling(N*prob) units (or clusters) are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N*prob) units (or clusters) will be sampled and with probability prob, ceiling(N*prob) units (or clusters) will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_n Use for a design in which strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of being sampled can vary across strata.
#' @param simple logical, defaults to FALSE. If TRUE, simple random sampling is used. When simple = TRUE, please do not specify n or strata_n.
#' @param check_inputs logical. Defaults to TRUE.
#' @param strata_var deprecated
#' @param clust_var deprecated
#'
#' @return A list of class "declaration".  The list has five entries:
#'   $rs_function, a function that generates random samplings according to the declaration.
#'   $rs_type, a string indicating the type of random sampling used
#'   $probabilities_vector, A vector length N indicating the probability of being sampled.
#'   $strata, the stratification variable.
#'   $clusters, the clustering variable.
#'
#' @examples
#' # The declare_rs function is used in three ways:
#'
#' # 1. To obtain some basic facts about a sampling procedure:
#' declaration <- declare_rs(N = 100, n = 30)
#' declaration
#'
#' # 2. To draw a random sample:
#'
#' S <- draw_rs(declaration)
#' table(S)
#'
#' # 3. To obtain inclusion probabilities
#'
#' probs <- obtain_inclusion_probabilities(declaration)
#' table(probs, S)
#'
#' # Simple Random Sampling Declarations
#'
#' declare_rs(N = 100, simple = TRUE)
#' declare_rs(N = 100, prob = .4, simple = TRUE)
#'
#' # Complete Random Sampling Declarations
#'
#' declare_rs(N = 100)
#' declare_rs(N = 100, n = 30)
#'
#' # Stratified Random Sampling Declarations
#'
#' strata <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' declare_rs(strata = strata)
#' declare_rs(strata = strata, prob = .5)
#'
#'
#' # Cluster Random Sampling Declarations
#'
#' clusters <- rep(letters, times = 1:26)
#' declare_rs(clusters = clusters)
#' declare_rs(clusters = clusters, n = 10)
#'
#' # Stratified and Clustered Random Sampling Declarations
#'
#' clusters <- rep(letters, times = 1:26)
#' strata <- rep(NA, length(clusters))
#' strata[clusters %in% letters[1:5]] <- "stratum_1"
#' strata[clusters %in% letters[6:10]] <- "stratum_2"
#' strata[clusters %in% letters[11:15]] <- "stratum_3"
#' strata[clusters %in% letters[16:20]] <- "stratum_4"
#' strata[clusters %in% letters[21:26]] <- "stratum_5"
#'
#' table(strata, clusters)
#'
#' declare_rs(clusters = clusters, strata = strata)
#' declare_rs(clusters = clusters, strata = strata, prob = .3)
#'
#' @export
declare_rs <- function(N = NULL,
                       strata = strata_var,
                       clusters = clust_var,
                       n = NULL,
                       prob = NULL,
                       strata_n = NULL,
                       strata_prob = NULL,
                       simple = FALSE,
                       check_inputs = TRUE, 
                       strata_var=NULL, 
                       clust_var=NULL) {

  warn_deprecated_args(NULL, clust_var, strata_var)
  
  
  
  if (check_inputs) {
    input_check <- check_samplr_arguments(
      N = N,
      strata = strata,
      clusters = clusters,
      n = n,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
  }
  # Determine rs_type
  if (!is.null(strata) && !is.null(clusters)) {
    rs_type <- "stratified_and_clustered"
  } else if (!is.null(clusters)) {
    rs_type <- "clustered"
  } else if (!is.null(strata)) {
    rs_type <- "stratified"
    if(simple) stop("You can't specify 'simple' with strata.")
  } else if (simple == FALSE) {
    rs_type <- "complete"
  } else {
    rs_type <- "simple"
    if (!is.null(n)) {
      stop("You can't specify 'n' when using simple random sampling.")
    }
  }
  
  if (rs_type == "simple") {

    rs_function <- function() {
      simple_rs(N = N,
                prob = prob,
                check_inputs = check_inputs)
    }
    probabilities_vector <-
      simple_rs_probabilities(N = N,
                              prob = prob,
                              check_inputs = check_inputs)
  }
  
  if (rs_type == "complete") {
    rs_function <- function() {
      complete_rs(
        N = N,
        n = n,
        prob = prob,
        check_inputs = check_inputs
      )
    }
    
    probabilities_vector <-
      complete_rs_probabilities(
        N = N,
        n = n,
        prob = prob,
        check_inputs = check_inputs
      )
    
  }
  
  if (rs_type == "stratified") {
    rs_function <- function() {
      strata_rs(
        strata = strata,
        n = n,
        strata_n = strata_n,
        prob = prob,
        strata_prob = strata_prob,
        check_inputs = check_inputs
      )
    }
    
    probabilities_vector <-
      strata_rs_probabilities(
        strata = strata,
        n = n,
        strata_n = strata_n,
        prob = prob,
        strata_prob = strata_prob,
        check_inputs = check_inputs
      )
  }
  
  
  if (rs_type == "clustered") {
    rs_function <- function() {
      cluster_rs(
        clusters = clusters,
        n = n,
        prob = prob,
        simple = simple,
        check_inputs = check_inputs
      )
    }
    
    probabilities_vector <-
      cluster_rs_probabilities(
        clusters = clusters,
        n = n,
        prob = prob,
        simple = simple,
        check_inputs = check_inputs
      )
    
  }
  
  if (rs_type == "stratified_and_clustered") {
    rs_function <- function() {
      strata_and_cluster_rs(
        clusters = clusters,
        strata = strata,
        prob = prob,
        n = n,
        strata_n = strata_n,
        strata_prob = strata_prob,
        check_inputs = check_inputs
      )
    }
    
    probabilities_vector <-
      strata_and_cluster_rs_probabilities(
        clusters = clusters,
        strata = strata,
        prob = prob,
        n = n,
        strata_prob = strata_prob,
        strata_n = strata_n,
        check_inputs = check_inputs
      )
    
  }
  
  return_object <- list(
    rs_function = rs_function,
    rs_type = rs_type,
    probabilities_vector = probabilities_vector,
    strata = strata,
    clusters = clusters,
    check_inputs = check_inputs
  )
  
  class(return_object) <- "rs_declaration"
  return(return_object)
  
}

#' Draw a random sample
#'
#' You can either give draw_rs() an declaration, as created by \code{\link{declare_rs}} or you can specify the other arguments to describe a random sampling procedure.
#'
#' @param declaration A random sampling declaration, created by \code{\link{declare_rs}}.
#' @inheritParams declare_rs
#' @examples
#' declaration <- declare_rs(N = 100, n = 30)
#' S <- draw_rs(declaration = declaration)
#' table(S)
#'
#' # equivalent to
#' S <- draw_rs(N = 100, n = 30)
#' table(S)
#'
#' @export
draw_rs <- function(declaration = NULL,
                    N = NULL,
                    strata = strata_var,
                    clusters = clust_var,
                    n = NULL,
                    prob = NULL,
                    strata_n = NULL,
                    strata_prob = NULL,
                    simple = FALSE, strata_var = NULL, clust_var = NULL) {
  if (!is.null(declaration)) {
    if (class(declaration) != "rs_declaration") {
      stop("You must provide a random sampling declaration created by declare_rs().")
    }
  } else{
    declaration <-
      declare_rs(
        N = N,
        strata = strata,
        clusters = clusters,
        n = n,
        prob = prob,
        strata_n = strata_n,
        strata_prob = strata_prob,
        simple = simple
      )
    
  }
  return(declaration$rs_function())
}

#' Obtain inclusion probabilities
#'
#' You can either give obtain_inclusion_probabilities() an declaration, as created by \code{\link{declare_rs}} or you can specify the other arguments to describe a random sampling procedure.\cr \cr
#' This function is especially useful when units have different inclusion probabilties and the analyst plans to use inverse-probability weights.
#'
#'
#' @param declaration A random sampling declaration, created by \code{\link{declare_rs}}.
#' @inheritParams declare_rs
#'
#' @examples
#'
#' # Draw a stratified random sample
#' strata <- rep(c("A", "B","C"), times=c(50, 100, 200))
#'
#' declaration <- declare_rs(strata = strata)
#'
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(declaration = declaration)
#'
#' table(strata, observed_probabilities)
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(strata = strata)
#'
#' table(strata, observed_probabilities)
#'
#' @export
obtain_inclusion_probabilities <-
  function(declaration = NULL,
           N = NULL,
           strata = strata_var,
           clusters = clust_var,
           n = NULL,
           prob = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           simple = FALSE, strata_var = NULL, clust_var = NULL) {
    # checks
    if (!is.null(declaration)) {
      if (class(declaration) != "rs_declaration") {
        stop("You must provide a random sampling declaration created by declare_rs().")
      }
    } else{
      declaration <-
        declare_rs(
          N = N,
          strata = strata,
          clusters = clusters,
          n = n,
          prob = prob,
          strata_n = strata_n,
          strata_prob = strata_prob,
          simple = simple
        )
    }
    
    probabilities_vector <- declaration$probabilities_vector
    return(probabilities_vector)
  }

#' @export
print.rs_declaration <- function(x, ...) {
  S <- x$rs_function()
  n <- length(S)
  
  constant_probabilities <-
    all(x$probabilities_vector[1] == x$probabilities_vector)
  
  if (x$rs_type == "stratified")
    cat("Random sampling procedure: Stratified random sampling", "\n")
  if (x$rs_type == "clustered")
    cat("Random sampling procedure: Cluster random sampling", "\n")
  if (x$rs_type == "simple")
    cat("Random sampling procedure: Simple random sampling", "\n")
  if (x$rs_type == "stratified_and_clustered")
    cat("Random sampling procedure: Stratified and clustered random sampling",
        "\n")
  if (x$rs_type == "complete")
    cat("Random sampling procedure: Complete random sampling",
        "\n")
  cat("Number of units:", n, "\n")
  if (!is.null(x$strata)) {
    cat("Number of strata:", length(unique(x$strata)), "\n")
  }
  if (!is.null(x$clusters)) {
    cat("Number of clusters:", length(unique(x$clusters)), "\n")
  }
  
  if (constant_probabilities) {
    cat("The inclusion probabilities are constant across units.")
  } else{
    cat(
      "The inclusion probabilities are NOT constant across units. Your analysis strategy must account for differential inclusion probabilities, typically by employing inverse probability weights."
    )
  }
}
