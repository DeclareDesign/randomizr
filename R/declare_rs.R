#' Declare a random sampling procedure.
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param strata_var A vector of length N that indicates which stratum each unit belongs to.
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param n Use for a design in which n units (or clusters) are sampled. (optional)
#' @param prob Use for a design in which either floor(N*prob) or ceiling(N*prob) units (or clusters) are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N*prob) units (or clusters) will be sampled and with probability prob, ceiling(N*prob) units (or clusters) will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_n Use for a design in which strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of assignment can vary across strata.
#' @param simple logical, defaults to FALSE. If TRUE, simple random assignment is used. When simple = TRUE, please do not specify n or strata_n.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A list of class "rs_declaration".  The list has five entries:
#'   $rs_function, a function that generates random assignments accroding to the declaration.
#'   $rs_type, a string indicating the type of random assignment used
#'   $probabilities_vector, A vector length N indicating the probability of being sampled.
#'   $strata_var, the stratification variable.
#'   $clust_var, the clustering variable.
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
#' # Complete Random Assignment Declarations
#'
#' declare_rs(N = 100)
#' declare_rs(N = 100, n = 30)
#'
#' # Stratified Random Sampling Declarations
#'
#' strata_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' declare_rs(strata_var = strata_var)
#' declare_rs(strata_var = strata_var, prob = .5)
#'
#'
#' # Cluster Random Sampling Declarations
#'
#' clust_var <- rep(letters, times = 1:26)
#' declare_rs(clust_var = clust_var)
#' declare_rs(clust_var = clust_var, n = 10)
#'
#' # Stratified and Clustered Random Sampling Declarations
#'
#' clust_var <- rep(letters, times = 1:26)
#' strata_var <- rep(NA, length(clust_var))
#' strata_var[clust_var %in% letters[1:5]] <- "stratum_1"
#' strata_var[clust_var %in% letters[6:10]] <- "stratum_2"
#' strata_var[clust_var %in% letters[11:15]] <- "stratum_3"
#' strata_var[clust_var %in% letters[16:20]] <- "stratum_4"
#' strata_var[clust_var %in% letters[21:26]] <- "stratum_5"
#'
#' table(strata_var, clust_var)
#'
#' declare_rs(clust_var = clust_var, strata_var = strata_var)
#' declare_rs(clust_var = clust_var, strata_var = strata_var, prob = .3)
#'
#' @export
declare_rs <- function(N = NULL,
                       strata_var = NULL,
                       clust_var = NULL,
                       n = NULL,
                       prob = NULL,
                       strata_n = NULL,
                       strata_prob = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  if (check_inputs) {
    check_inputs <- check_samplr_arguments(
      N = N,
      strata_var = strata_var,
      clust_var = clust_var,
      n = n,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
    
  }
  # Determine rs_type
  if (simple == FALSE) {
    rs_type <- "complete"
  } else{
    rs_type <- "simple"
  }
  if (!is.null(strata_var) & is.null(clust_var)) {
    rs_type <- "stratified"
  }
  if (is.null(strata_var) & !is.null(clust_var)) {
    rs_type <- "clustered"
  }
  if (!is.null(strata_var) & !is.null(clust_var)) {
    rs_type <- "stratified_and_clustered"
  }
  
  if (rs_type == "simple" & is.null(clust_var)) {
    if (!is.null(n)) {
      stop("You can't specify 'n' when using simple random assignment.")
    }
    if (!is.null(strata_var)) {
      stop("You can't specify 'strata_var' when using simple random assignment.")
    }
    
    rs_function <- function() {
      simple_rs(N = N,
                prob = prob)
    }
    probabilities_vector <-
      simple_rs_probabilities(N = N,
                              prob = prob)
  }
  
  if (rs_type == "complete") {
    rs_function <- function() {
      complete_rs(N = N,
                  n = n,
                  prob = prob)
    }
    
    probabilities_vector <-
      complete_rs_probabilities(N = N,
                                n = n,
                                prob = prob)
    
  }
  
  if (rs_type == "stratified") {
    rs_function <- function() {
      strata_rs(
        strata_var = strata_var,
        strata_n = strata_n,
        prob = prob,
        strata_prob = strata_prob
      )
    }
    
    probabilities_vector <-
      strata_rs_probabilities(
        strata_var = strata_var,
        strata_n = strata_n,
        prob = prob,
        strata_prob = strata_prob
      )
  }
  
  
  if (rs_type == "clustered") {
    rs_function <- function() {
      cluster_rs(
        clust_var = clust_var,
        n = n,
        prob = prob,
        simple = simple
      )
    }
    
    probabilities_vector <-
      cluster_rs_probabilities(
        clust_var = clust_var,
        n = n,
        prob = prob,
        simple = simple
      )
    
  }
  
  if (rs_type == "stratified_and_clustered") {
    rs_function <- function() {
      strata_and_cluster_rs(
        clust_var = clust_var,
        strata_var = strata_var,
        prob = prob,
        strata_n = strata_n,
        strata_prob = strata_prob
      )
    }
    
    probabilities_vector <-
      strata_and_cluster_rs_probabilities(
        clust_var = clust_var,
        strata_var = strata_var,
        prob = prob,
        strata_prob = strata_prob,
        strata_n = strata_n
      )
    
  }
  
  return_object <- list(
    rs_function = rs_function,
    rs_type = rs_type,
    probabilities_vector = probabilities_vector,
    strata_var = strata_var,
    clust_var = clust_var
  )
  
  class(return_object) <- "rs_declaration"
  return(return_object)
  
}

#' Draw a random sample
#'
#' You can either give draw_rs() an rs_declaration, as created by \code{\link{declare_rs}} or you can specify the other arguments to describe a random assignment procedure.
#'
#' @param rs_declaration A random sampling declaration, created by \code{\link{declare_rs}}.
#' @inheritParams declare_rs
#' @examples
#' declaration <- declare_rs(N = 100, n = 30)
#' S <- draw_rs(rs_declaration = declaration)
#' table(S)
#'
#' # equivalent to
#' S <- draw_rs(N = 100, n = 30)
#' table(S)
#'
#' @export
draw_rs <- function(rs_declaration = NULL,
                    N = NULL,
                    strata_var = NULL,
                    clust_var = NULL,
                    n = NULL,
                    prob = NULL,
                    strata_n = NULL,
                    strata_prob = NULL,
                    simple = FALSE) {
  if (!is.null(rs_declaration)) {
    if (class(rs_declaration) != "rs_declaration") {
      stop("You must provide a random sampling declaration created by declare_rs().")
    }
  } else{
    rs_declaration <-
      declare_rs(
        N = N,
        strata_var = strata_var,
        clust_var = clust_var,
        n = n,
        prob = prob,
        strata_n = strata_n,
        strata_prob = strata_prob,
        simple = simple
      )
    
  }
  return(rs_declaration$rs_function())
}

#' Obtain inclusion probabilities
#'
#' You can either give obtain_inclusion_probabilities() an rs_declaration, as created by \code{\link{declare_rs}} or you can specify the other arguments to describe a random sampling procedure.\cr \cr
#' This function is especially useful when units have different inclusion probabilties and the analyst plans to use inverse-probability weights.
#'
#'
#' @param rs_declaration A random sampling declaration, created by \code{\link{declare_rs}}.
#' @inheritParams declare_rs
#'
#' @examples
#'
#' # Draw a stratified random sample
#' strata_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#'
#' declaration <- declare_rs(strata_var = strata_var)
#'
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(rs_declaration = declaration)
#'
#' table(strata_var, observed_probabilities)
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(strata_var = strata_var)
#'
#' table(strata_var, observed_probabilities)
#'
#' @export
obtain_inclusion_probabilities <-
  function(rs_declaration = NULL,
           N = NULL,
           strata_var = NULL,
           clust_var = NULL,
           n = NULL,
           prob = NULL,
           strata_n = NULL,
           strata_prob = NULL,
           simple = FALSE) {
    # checks
    if (!is.null(rs_declaration)) {
      if (class(rs_declaration) != "rs_declaration") {
        stop("You must provide a random assignment declaration created by declare_rs().")
      }
    } else{
      rs_declaration <-
        declare_rs(
          N = N,
          strata_var = strata_var,
          clust_var = clust_var,
          n = n,
          prob = prob,
          strata_n = strata_n,
          strata_prob = strata_prob,
          simple = simple
        )
    }
    
    probabilities_vector <- rs_declaration$probabilities_vector
    return(probabilities_vector)
  }

#' @export
print.rs_declaration <- function(x, ...) {
  S <- x$rs_function()
  n <- length(S)
  
  constant_probabilities <-
    all(x$probabilities_vector[1] == x$probabilities_vector)
  
  cat("\n")
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
  if (!is.null(x$strata_var)) {
    cat("Number of strata:", length(unique(x$strata_var)), "\n")
  }
  if (!is.null(x$clust_var)) {
    cat("Number of clusters:", length(unique(x$clust_var)), "\n")
  }
  
  if (constant_probabilities) {
    cat("The inclusion probabilities are constant across units.")
  } else{
    cat(
      "The inclusion probabilities are NOT constant across units. Your analysis strategy must account for differential inclusion probabilities, typically by employing inverse probability weights."
    )
  }
}
