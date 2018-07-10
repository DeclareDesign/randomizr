#' Declare a random sampling procedure.
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param strata A vector of length N that indicates which stratum each unit belongs to.
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param n Use for a design in which n units (or clusters) are sampled. In a stratified design, exactly n units in each stratum will be sampled. (optional)
#' @param prob Use for a design in which either floor(N*prob) or ceiling(N*prob) units (or clusters) are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N*prob) units (or clusters) will be sampled and with probability prob, ceiling(N*prob) units (or clusters) will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_n Use for a design in which strata_n describes the number of units to sample within each stratum.
#' @param strata_prob Use for a design in which strata_prob describes the probability of being sampled within each stratum. Differs from prob in that the probability of being sampled can vary across strata.
#' @param simple logical, defaults to FALSE. If TRUE, simple random sampling is used. When \code{simple = TRUE}, please do not specify n or strata_n. When \code{simple = TRUE}, \code{prob} may vary by unit.
#' @param check_inputs logical. Defaults to TRUE.
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
                       strata = NULL,
                       clusters = NULL,
                       n = NULL,
                       prob = NULL,
                       strata_n = NULL,
                       strata_prob = NULL,
                       simple = FALSE,
                       check_inputs = TRUE) {
  all_args <-  mget(names(formals(sys.function())))
  
  
  
  if (check_inputs) {
    input_check <- check_samplr_arguments_new(all_args)
    for (i in names(input_check))
      all_args[[i]] <- input_check[[i]]
    all_args$check_inputs <-
      FALSE # don't need to recheck when using declaration
  }
  
  is_strata <- is.vector(strata) || is.factor(strata)
  is_clust <- is.vector(clusters) || is.factor(clusters)
  
  # Determine rs_type
  if (is_strata && is_clust) {
    rs_type <- "stratified_and_clustered"
  } else if (is_clust) {
    rs_type <- "clustered"
  } else if (is_strata) {
    rs_type <- "stratified"
  } else if (simple == FALSE) {
    rs_type <- "complete"
  } else {
    rs_type <- "simple"
  }
  
  
  return_object <- list2env(all_args, parent = emptyenv())
  return_object$rs_function <- function() {
    .Deprecated("draw_rs")
    rs_function(return_object)
  }
  
  delayedAssign("rs_type", {
    warning("rs_type is deprecated; check the class attribute instead.")
    rs_type
  }, assign.env = return_object)
  
  delayedAssign("cleaned_arguments", {
    warning("cleaned_arguments is deprecated")
    input_check
  }, assign.env = return_object)
  
  delayedAssign("probabilities_vector",
                rs_probabilities(return_object),
                assign.env = return_object)
  
  class(return_object) <-
    c("rs_declaration",  paste0("rs_", rs_type))
  attr(return_object, "call") <- match.call()
  
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
draw_rs <- function(declaration = NULL) {
  if (is.null(declaration)) {
    all_args <- mget(names(formals(declare_rs)))
    declaration <- do.call(declare_rs, all_args)
  }
  rs_function(declaration)
}

formals(draw_rs) <- c(formals(draw_rs), formals(declare_rs))


#' Obtain inclusion probabilities
#'
#' You can either give obtain_inclusion_probabilities() an declaration, as created by \code{\link{declare_rs}} or you can specify the other arguments to describe a random sampling procedure.\cr \cr
#' This function is especially useful when units have different inclusion probabilities and the analyst plans to use inverse-probability weights.
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
obtain_inclusion_probabilities <- function(declaration = NULL) {
  # checks
  if (is.null(declaration)) {
    all_args <- mget(names(formals(declare_rs)))
    declaration <- do.call(declare_rs, all_args)
  } else if (!inherits(declaration, "rs_declaration")) {
    stop("You must provide a random sampling declaration created by declare_rs().")
  }
  
  declaration$probabilities_vector
}

formals(obtain_inclusion_probabilities) <-
  c(formals(obtain_inclusion_probabilities),
    formals(declare_rs))


#' @export
summary.rs_declaration <- function(object, ...) {
  print(object, ... = ...)
}

#' @export
print.rs_declaration <- function(x, ...) {
  S <- draw_rs(x)
  n <- length(S)
  
  cat("Random sampling procedure:",
      switch(
        class(x)[2],
        "rs_stratified" = "Stratified",
        "rs_clustered" = "Cluster",
        "rs_simple" = "Simple",
        "rs_stratified_and_clustered" = "Stratified and clustered",
        "rs_complete" = "Complete"
      ),
      "random sampling",
      "\n")
  
  cat("Number of units:", n, "\n")
  
  if (!is.null(x$strata)) {
    cat("Number of strata:", length(unique(x$strata)), "\n")
  }
  
  if (!is.null(x$clusters)) {
    cat("Number of clusters:", length(unique(x$clusters)), "\n")
  }
  
  # awaiting num permutations
  # if (obtain_num_permutations(x) == Inf) {
  #   cat("The number of possible random assignments is approximately infinite. \n")
  # } else {
  #   cat(paste0("The number of possible random assignments is ",
  #              obtain_num_permutations(x),
  #              ". "),
  #       "\n")
  # }
  
  if (is_constant(x$probabilities_vector)) {
    cat("The inclusion probabilities are constant across units.")
  } else{
    cat(
      "The inclusion probabilities are NOT constant across units.",
      "Your analysis strategy must account for differential inclusion probabilities,",
      "typically by employing inverse probability weights."
    )
  }
  invisible(x)
}
