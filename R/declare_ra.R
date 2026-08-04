#' Declare a Random Assignment Procedure
#'
#' \code{declare_ra} creates a reusable declaration object that captures all the parameters of a random assignment procedure. The declaration separates the specification of the design from the act of conducting it: call \code{declare_ra} once to fix the design, then call \code{\link{conduct_ra}} repeatedly (for example, across simulation iterations) to draw assignments from the declared procedure. The declaration also precomputes and caches the probability of assignment for each unit, which \code{\link{obtain_condition_probabilities}} returns for use in inverse-probability-weighted estimators.
#'
#' \code{declare_ra} supports simple, complete, blocked, clustered, and blocked-and-clustered designs. It dispatches to the appropriate low-level function (\code{\link{simple_ra}}, \code{\link{complete_ra}}, \code{\link{block_ra}}, \code{\link{cluster_ra}}, or \code{\link{block_and_cluster_ra}}) based on which arguments are supplied.
#'
#' @seealso \code{\link{conduct_ra}}, \code{\link{obtain_condition_probabilities}}, \code{\link{declare_rs}}
#'
#' @param N The number of units. Must be a positive integer. (required)
#' @param blocks A vector of length N indicating which block each unit belongs to. Supply to use blocked random assignment. (optional)
#' @param clusters A vector of length N indicating which cluster each unit belongs to. Supply to use cluster random assignment. (optional)
#' @param m Use for a two-arm design: exactly \code{m} units (or clusters) are assigned to treatment. In a blocked design, exactly \code{m} units in each block are treated. (optional)
#' @param m_unit Use for a two-arm trial. Under complete random assignment, must be constant across units. Under blocked random assignment, must be constant within blocks. (optional)
#' @param m_each Use for a multi-arm design. A numeric vector giving the number of units (or clusters) assigned to each condition; must sum to N. (optional)
#' @param prob Use for a two-arm design: either \code{floor(N*prob)} or \code{ceiling(N*prob)} units (or clusters) are assigned to treatment so that the marginal probability of assignment equals exactly \code{prob}. Must be between 0 and 1. Under simple random assignment, may vary by unit. (optional)
#' @param prob_unit Use for a two-arm design. Of length N. Under simple random assignment, may differ by unit or cluster. Under complete random assignment, must be constant across units. Under blocked random assignment, must be constant within blocks. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition; entries must be nonnegative and sum to 1. Due to integer rounding the exact count in each condition may differ slightly from draw to draw, but the overall probability is exactly \code{prob_each}. (optional)
#' @param block_m Use for a two-arm blocked design: a vector giving the number of units to assign to treatment within each block, in the order of \code{sort(unique(blocks))}. (optional)
#' @param block_m_each Use for a multi-arm blocked design. A matrix with one row per block and one column per treatment arm giving the number of units assigned to each condition within each block. Rows respect the ordering of \code{sort(unique(blocks))}. (optional)
#' @param block_prob Use for a two-arm blocked design in which the treatment probability varies across blocks. In the order of \code{sort(unique(blocks))}. (optional)
#' @param block_prob_each Use for a multi-arm blocked design in which treatment probabilities vary across blocks. A matrix with one row per block and one column per arm; each row must sum to 1. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named 0 and 1 in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, simple random assignment is used. Do not specify \code{m}, \code{m_each}, \code{block_m}, or \code{block_m_each} when \code{simple = TRUE}. (optional)
#' @param permutation_matrix For custom random assignment procedures. (optional)
#' @param check_inputs Logical. Defaults to \code{TRUE}.
#'
#' @return A list of class \code{"ra_declaration"} with entries:
#'   \describe{
#'     \item{\code{ra_function}}{A function that draws a random assignment from the declared procedure.}
#'     \item{\code{ra_type}}{A string indicating the type of random assignment used.}
#'     \item{\code{probabilities_matrix}}{A matrix with N rows and \code{num_arms} columns giving each unit's probability of assignment to each condition.}
#'     \item{\code{blocks}}{The blocking variable, if supplied.}
#'     \item{\code{clusters}}{The clustering variable, if supplied.}
#'   }
#'
#' @examples
#' # The declare_ra function is used in three ways:
#'
#' # 1. To obtain some basic facts about a randomization:
#' declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
#' declaration
#'
#' # 2. To conduct a random assignment:
#'
#' Z <- conduct_ra(declaration)
#' table(Z)
#'
#' # 3. To obtain observed condition probabilities
#'
#' probs <- obtain_condition_probabilities(declaration, Z)
#' table(probs, Z)
#'
#' # Simple Random Assignment Declarations
#'
#' declare_ra(N=100, simple = TRUE)
#' declare_ra(N=100, prob = .4, simple = TRUE)
#' declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4),
#'            conditions=c("control", "placebo", "treatment"), simple=TRUE)
#'
#' # Complete Random Assignment Declarations
#'
#' declare_ra(N=100)
#' declare_ra(N=100, m_each = c(30, 70),
#'            conditions = c("control", "treatment"))
#' declare_ra(N=100, m_each=c(30, 30, 40))
#'
#'
#' # Block Random Assignment Declarations
#'
#' blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
#  declare_ra(blocks = blocks)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declare_ra(blocks = blocks, block_m_each = block_m_each)
#'
#'
#' # Cluster Random Assignment Declarations
#'
#' clusters <- rep(letters, times = 1:26)
#' declare_ra(clusters = clusters)
#' declare_ra(clusters = clusters, m_each = c(7, 7, 12))
#'
#' # Blocked and Clustered Random Assignment Declarations
#'
#' clusters <- rep(letters, times=1:26)
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:5]] <- "block_1"
#' blocks[clusters %in% letters[6:10]] <- "block_2"
#' blocks[clusters %in% letters[11:15]] <- "block_3"
#' blocks[clusters %in% letters[16:20]] <- "block_4"
#' blocks[clusters %in% letters[21:26]] <- "block_5"
#'
#' table(blocks, clusters)
#'
#' declare_ra(clusters = clusters, blocks = blocks)
#' declare_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
#'
#' @export
declare_ra <- function(N = NULL,
                       blocks = NULL,
                       clusters = NULL,
                       m = NULL,
                       m_unit = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_unit = NULL,
                       prob_each = NULL,
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob = NULL,
                       block_prob_each = NULL,
                       num_arms = NULL,
                       conditions = NULL,
                       simple = FALSE,
                       permutation_matrix = NULL,
                       check_inputs = TRUE) {
  input_check <- NULL
  all_args <-  mget(names(formals(sys.function())))
  
  if (check_inputs && is.null(permutation_matrix)) {
    input_check <- check_randomizr_arguments_new(all_args)
    for (i in names(input_check))
      all_args[[i]] <- input_check[[i]]
    all_args$check_inputs <-
      FALSE # don't need to recheck when using declaration
  }
  
  is_block <- !is.null(blocks) || is.factor(blocks)
  is_clust <- !is.null(clusters) || is.factor(clusters)
  
  # Determine ra_type
  if (is.matrix(permutation_matrix)) {
    ra_type <- "custom"
  } else  if (is_block && is_clust) {
    ra_type <- "blocked_and_clustered"
  } else  if (is_clust) {
    ra_type <- "clustered"
  } else  if (is_block) {
    ra_type <- "blocked"
  } else  if (simple == FALSE) {
    ra_type <- "complete"
  } else {
    ra_type <- "simple"
  }
  
  return_object <- list2env(all_args, parent = emptyenv())

  # Cache integer factor encoding for blocked designs so conduct_ra() can skip
  # the as.factor + tabulate on every simulation draw (~21 us saved per call).
  if (ra_type %in% c("blocked", "blocked_and_clustered") && !is.null(blocks)) {
    return_object[[".block_int"]] <- as.integer(as.factor(blocks))
  }
  
  return_object$ra_function <- function() {
    .Deprecated("conduct_ra")
    ra_function(return_object) #todo
  }
  
  delayedAssign("ra_type", {
    warning("ra_type is deprecated; check the object class instead.")
    ra_type
  }, assign.env = return_object)
  
  
  delayedAssign("cleaned_arguments", {
    warning("cleaned_arguments is deprecated")
    input_check
  }, assign.env = return_object)
  
  
  delayedAssign("probabilities_matrix",
                ra_probabilities(return_object),
                assign.env = return_object)
  
  
  class(return_object) <-
    c("ra_declaration", paste0("ra_", ra_type))
  attr(return_object, "call") <- match.call()
  return(return_object)
  
}


#' Conduct a random assignment
#'
#' \code{conduct_ra} draws one random assignment from a design. Give it a
#' declaration made by \code{\link{declare_ra}}, or describe the design inline
#' with the same arguments \code{declare_ra} takes. Declaring first pays off
#' when the same design is drawn repeatedly, or when the assignment
#' probabilities are needed later by \code{\link{obtain_condition_probabilities}}.
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @inheritParams declare_ra
#' @examples
#' declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))
#' Z <- conduct_ra(declaration = declaration)
#' table(Z)
#'
#' # equivalent to
#'
#' Z <- conduct_ra(N = 100, m_each = c(30, 30, 40))
#' table(Z)
#'
#' @return A vector of length N giving the treatment condition of each unit,
#'   numeric in a two-arm design and a factor (ordered by \code{conditions}) in
#'   a multi-arm design.
#' @seealso \code{\link{declare_ra}}, \code{\link{obtain_condition_probabilities}}
#' @export
conduct_ra <- function(declaration = NULL) {
  if (is.null(declaration)) {
    all_args <- mget(names(formals(declare_ra)))
    declaration <- do.call(declare_ra, all_args)
  } else if (!inherits(declaration, "ra_declaration")) {
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  ra_function(declaration)
}

formals(conduct_ra) <- c(formals(conduct_ra), formals(declare_ra))

#' Obtain the probabilities of units being in the conditions that they are in.
#'
#' Give obtain_condition_probabilities() a declaration made by \code{\link{declare_ra}}, or describe the design inline with the same arguments \code{declare_ra} takes.\cr \cr
#' This function is especially useful when units have different probabilities of assignment and the analyst plans to use inverse-probability weights.
#'
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @param assignment A vector of random assignments, often created by \code{\link{conduct_ra}}.
#' @inheritParams declare_ra
#'
#' @examples
#'
#' # Conduct a block random assignment
#' blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declaration <- declare_ra(blocks = blocks, block_m_each = block_m_each)
#' Z <- conduct_ra(declaration = declaration)
#' table(Z, blocks)
#'
#' observed_probabilities <-
#'    obtain_condition_probabilities(declaration = declaration, assignment = Z)
#'
#'
#' # Probabilities in the control group:
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#'
#' # Probabilities in the treatment group:
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' Z <- conduct_ra(blocks = blocks, block_m_each = block_m_each)
#' observed_probabilities <-
#'    obtain_condition_probabilities(assignment = Z,
#'                                   blocks = blocks,
#'                                   block_m_each = block_m_each)
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#' @return A vector of length N giving, for each unit, the probability that it
#'   was assigned to the condition it is actually in. These are the quantities
#'   inverse-probability weights are built from: weight each unit by the
#'   reciprocal of its value here.
#' @seealso \code{\link{declare_ra}}, \code{\link{conduct_ra}}
#' @export
obtain_condition_probabilities <-
  function(declaration = NULL,
           assignment) {
    # checks
    if (is.null(declaration)) {
      if (is.null(N)) {
        N <- length(assignment)
      }
      all_args <- mget(names(formals(declare_ra)))
      declaration <- do.call(declare_ra, all_args)
    } else if (!inherits(declaration, "ra_declaration")) {
      stop("You must provide a random assignment declaration created by declare_ra().")
    }
    
    
    pmat <-
      declaration$probabilities_matrix # this may have been delayAssigned
    cond_probs <-
      pmat[cbind(seq_len(nrow(pmat)),
                 match(paste0("prob_", assignment), colnames(pmat)))]
    return(cond_probs)
  }

formals(obtain_condition_probabilities) <-
  c(formals(obtain_condition_probabilities),
    formals(declare_ra))


#' @export
summary.ra_declaration <- function(object, ...) {
  print(object, ... = ...)
}

#' @export
#' @importFrom utils head
print.ra_declaration <- function(x, ...) {
  Z <- conduct_ra(x)
  n <- length(Z)
  
  conditions <- sort(unique(Z))
  num_arms <- length(conditions)
  
  cat("Random assignment procedure:" ,
      switch(
        class(x)[2],
        "ra_blocked" = "Block",
        "ra_clustered" = "Cluster",
        "ra_simple" = "Simple",
        "ra_blocked_and_clustered" = "Blocked and clustered",
        "ra_complete" = "Complete"
      ),
      "random assignment",
      "\n")
  
  cat("Number of units:", n, "\n")
  
  if (!is.null(x$blocks)) {
    cat(sprintf("Number of blocks: %d\n", length(unique(x$blocks))))
  }
  if (!is.null(x$clusters)) {
    cat(sprintf("Number of clusters: %d\n", length(unique(x$clusters))))
  }
  
  cat("Number of treatment arms:", num_arms, "\n")
  
  cat(sprintf(
    "The possible treatment categories are %s.\n",
    paste(conditions, collapse = " and ")
  ))
  
  if (obtain_num_permutations(x) == Inf) {
    cat("The number of possible random assignments is approximately infinite. \n")
  } else {
    cat(
      paste0(
        "The number of possible random assignments is ",
        obtain_num_permutations(x),
        ". "
      ),
      "\n"
    )
  }
  if (all(apply(x$probabilities_matrix, 2, is_constant))) {
    cat("The probabilities of assignment are constant across units: \n")
    print(apply(x$probabilities_matrix, 2, head, n = 1))
    
  } else{
    cat(
      "The probabilities of assignment are NOT constant across units.",
      "Your analysis strategy must account for differential probabilities of assignment,",
      "typically by employing inverse probability weights."
    )
  }
  invisible(x)
}
