#' Declare a Random Sampling Procedure
#'
#' \code{declare_rs} describes a sampling design once so that the rest of the
#' package can work from it. Pass the result to \code{\link{draw_rs}()} to draw a
#' sample, or to \code{\link{obtain_inclusion_probabilities}()} to recover each
#' unit's probability of selection. Declaring is worth the extra line whenever a
#' design is drawn more than once, since the probabilities are then computed
#' from the same object that produced the sample rather than reconstructed by
#' hand.
#'
#' \code{declare_rs} covers the same four designs as the sampling functions
#' themselves: simple, complete, stratified, and clustered, in any combination.
#' Which one it declares is inferred from the arguments given.
#'
#' @seealso \code{\link{draw_rs}()}, \code{\link{obtain_inclusion_probabilities}()},
#'   \code{\link{declare_ra}()}
#'
#' @param N The number of units in the sampling frame. Must be a positive integer. (required)
#' @param strata A vector of length N indicating which stratum each unit belongs to. Supply to use stratified random sampling. (optional)
#' @param clusters A vector of length N indicating which cluster each unit belongs to. Supply to sample whole clusters. (optional)
#' @param n Use for a design in which exactly \code{n} units (or clusters) are sampled. In a stratified design, exactly \code{n} units in each stratum are sampled. (optional)
#' @param n_unit Of length N. Under complete random sampling, must be constant across units. Under stratified random sampling, must be constant within strata. (optional)
#' @param prob Use for a design in which either \code{floor(N*prob)} or \code{ceiling(N*prob)} units (or clusters) are sampled. Which of the two is used is itself random: the ceiling is drawn with probability equal to the fractional part of \code{N*prob} and the floor otherwise, which makes each unit's probability of inclusion exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit Of length N. Under simple random sampling, may differ for each unit or cluster. Under complete random sampling, must be constant across units. Under stratified random sampling, must be constant within strata. (optional)
#' @param strata_n Use for a design in which \code{strata_n} gives the number of units to sample within each stratum, in the order of \code{sort(unique(strata))}. (optional)
#' @param strata_prob Use for a design in which \code{strata_prob} gives the probability of being sampled within each stratum, in the order of \code{sort(unique(strata))}. Differs from \code{prob} in that the probability of being sampled can vary across strata. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, simple random sampling is used, so the size of the realized sample varies from draw to draw. Do not specify \code{n} or \code{strata_n} when \code{simple = TRUE}; \code{prob} may then vary by unit. (optional)
#' @param check_inputs Logical. Whether to verify before declaring that the arguments are internally consistent: that counts do not exceed the frame, that probabilities lie between 0 and 1, that stratum-level arguments have one entry per stratum, and so on. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when declaring many designs from arguments that have already been verified. (optional)
#'
#' @return A list of class \code{"rs_declaration"} with entries:
#'   \describe{
#'     \item{\code{rs_function}}{A function that draws a random sample from the declared procedure.}
#'     \item{\code{rs_type}}{A string indicating the type of random sampling used.}
#'     \item{\code{probabilities_vector}}{A vector of length N giving each unit's probability of being included in the sample.}
#'     \item{\code{strata}}{The stratification variable, if supplied.}
#'     \item{\code{clusters}}{The clustering variable, if supplied.}
#'   }
#'
#' @examples
#' # A declaration is used in three ways.
#'
#' # 1. To obtain some basic facts about a sampling procedure:
#'
#' declaration <- declare_rs(N = 100, n = 30)
#' declaration
#'
#' # 2. To draw a random sample:
#'
#' S <- draw_rs(declaration)
#' table(S)
#'
#' # 3. To obtain inclusion probabilities:
#'
#' probs <- obtain_inclusion_probabilities(declaration)
#' table(probs, S)
#'
#'
#' # Simple Random Sampling Declarations
#'
#' declare_rs(N = 100, simple = TRUE)
#'
#' declare_rs(N = 100, prob = 0.4, simple = TRUE)
#'
#'
#' # Complete Random Sampling Declarations
#'
#' declare_rs(N = 100)
#'
#' declare_rs(N = 100, n = 30)
#'
#'
#' # Stratified Random Sampling Declarations
#'
#' strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#'
#' declare_rs(strata = strata)
#'
#' declare_rs(strata = strata, prob = 0.5)
#'
#'
#' # Cluster Random Sampling Declarations
#'
#' clusters <- rep(letters[1:10], times = 1:10)
#'
#' declare_rs(clusters = clusters)
#'
#' declare_rs(clusters = clusters, n = 4)
#'
#'
#' # Stratified and Clustered Random Sampling Declarations
#'
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
#' declare_rs(clusters = clusters, strata = strata)
#'
#' declare_rs(clusters = clusters, strata = strata, prob = 0.3)
#'
#' @export
declare_rs <- function(N = NULL,
                       strata = NULL,
                       clusters = NULL,
                       n = NULL,
                       n_unit = NULL,
                       prob = NULL,
                       prob_unit = NULL,
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
  
  
  delayedAssign("probabilities_matrix",
                cbind((1 - rs_probabilities(return_object)),
                      rs_probabilities(return_object)),
                assign.env = return_object)
  
  class(return_object) <-
    c("rs_declaration",  paste0("rs_", rs_type))
  attr(return_object, "call") <- match.call()
  
  return(return_object)
  
}


#' Draw a Random Sample
#'
#' \code{draw_rs} draws one random sample from a design. Give it a declaration
#' made by \code{\link{declare_rs}()}, or describe the design inline with the
#' same arguments \code{declare_rs()} takes. Declaring first pays off when the
#' same design is drawn repeatedly, or when the inclusion probabilities are
#' needed later by \code{\link{obtain_inclusion_probabilities}()}.
#'
#' @param declaration A random sampling declaration, created by \code{\link{declare_rs}()}. Supply either a declaration or the design arguments listed below, which are the ones \code{declare_rs()} takes: given those, \code{draw_rs} builds a declaration internally and draws one sample from it. (optional)
#' @inheritParams declare_rs
#' @examples
#' # Declare the design once, then draw from it
#' declaration <- declare_rs(N = 100, n = 30)
#'
#' S <- draw_rs(declaration = declaration)
#' table(S)
#'
#' # Equivalent, and convenient for a one-off sample: describe the design
#' # inline and skip the declaration
#' S <- draw_rs(N = 100, n = 30)
#' table(S)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0).
#' @seealso \code{\link{declare_rs}()}, \code{\link{obtain_inclusion_probabilities}()}
#' @export
draw_rs <- function(declaration = NULL) {
  if (is.null(declaration)) {
    all_args <- mget(names(formals(declare_rs)))
    declaration <- do.call(declare_rs, all_args)
  }
  rs_function(declaration)
}

formals(draw_rs) <- c(formals(draw_rs), formals(declare_rs))


#' Obtain Inclusion Probabilities
#'
#' Returns each unit's probability of being included in the sample under a declared design. Give \code{obtain_inclusion_probabilities()} a declaration made by \code{\link{declare_rs}()}, or describe the design inline with the same arguments \code{declare_rs()} takes.\cr \cr
#' This function is especially useful when units have different inclusion probabilities and the analyst plans to use inverse-probability weights: the weights are the reciprocals of what it returns.
#'
#'
#' @param declaration A random sampling declaration, created by \code{\link{declare_rs}()}. Supply either a declaration or the design arguments that \code{declare_rs()} takes. (optional)
#' @inheritParams declare_rs
#'
#' @examples
#'
#' # A stratified design in which the strata are sampled at different rates
#' strata <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#'
#' declaration <- declare_rs(strata = strata, strata_n = c(20, 30, 40))
#'
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(declaration = declaration)
#'
#' table(strata, observed_probabilities)
#'
#' # The weights for an inverse-probability-weighted analysis
#' ipw <- 1 / observed_probabilities
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' observed_probabilities <-
#'    obtain_inclusion_probabilities(strata = strata, strata_n = c(20, 30, 40))
#'
#' table(strata, observed_probabilities)
#'
#' @return A numeric vector of length N giving each unit's probability of being
#'   included in the sample. These are the quantities inverse-probability
#'   weights are built from: weight each sampled unit by the reciprocal of its
#'   value here.
#' @seealso \code{\link{declare_rs}()}, \code{\link{draw_rs}()}
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
