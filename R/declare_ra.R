#' Declare a Random Assignment Procedure
#'
#' \code{declare_ra} creates a reusable declaration object that captures all the parameters of a random assignment procedure. The declaration separates the specification of the design from the act of conducting it: call \code{declare_ra} once to fix the design, then call \code{\link{conduct_ra}()} repeatedly (for example, across simulation iterations) to draw assignments from the declared procedure. The declaration also precomputes and caches the probability of assignment for each unit, which \code{\link{obtain_condition_probabilities}()} returns for use in inverse-probability-weighted estimators.
#'
#' \code{declare_ra} supports simple, complete, blocked, clustered, blocked-and-clustered, and balanced designs. It dispatches to the appropriate low-level function (\code{\link{simple_ra}()}, \code{\link{complete_ra}()}, \code{\link{block_ra}()}, \code{\link{cluster_ra}()}, \code{\link{block_and_cluster_ra}()}, or \code{\link{balanced_ra}()}) based on which arguments are supplied. Balanced assignment is opt-in: \code{declare_ra(N, prob = 0.5)} remains complete assignment. Use \code{ra_type = "balanced"} or supply \code{prob_unit_each} or \code{formula}.
#'
#' @seealso \code{\link{conduct_ra}()}, \code{\link{obtain_condition_probabilities}()}, \code{\link{balanced_ra}()}, \code{\link{declare_rs}()}
#'
#' @param N The number of units. A positive integer. Optional when
#'   \code{formula} or the length of \code{prob_unit} (or \code{blocks},
#'   or \code{clusters}) identifies N.
#' @param blocks A vector of length N indicating which block each unit belongs to. Supply to use blocked random assignment. (optional)
#' @param clusters A vector of length N indicating which cluster each unit belongs to. Supply to use cluster random assignment. (optional)
#' @param m Use for a two-arm design: exactly \code{m} units (or clusters) are assigned to treatment. In a blocked design, exactly \code{m} units in each block are treated. (optional)
#' @param m_unit Use for a two-arm trial. Under complete random assignment, must be constant across units. Under blocked random assignment, must be constant within blocks. (optional)
#' @param m_each Use for a multi-arm design. A numeric vector giving the number of units (or clusters) assigned to each condition; must sum to N. (optional)
#' @param prob Use for a two-arm design: either \code{floor(N*prob)} or \code{ceiling(N*prob)} units (or clusters) are assigned to treatment so that the marginal probability of assignment equals exactly \code{prob}. Must be between 0 and 1. Under simple random assignment, may vary by unit. (optional)
#' @param prob_unit Use for a two-arm design. Of length N. Under simple random assignment, may differ by unit or cluster. Under complete random assignment, must be constant across units. Under blocked random assignment, must be constant within blocks. Under balanced assignment (\code{ra_type = "balanced"}), may differ by unit. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition; entries must be nonnegative and sum to 1. Due to integer rounding the exact count in each condition may differ slightly from draw to draw, but the overall probability is exactly \code{prob_each}. Under balanced assignment the same vector is expanded to one row per unit. (optional)
#' @param prob_unit_each Use for balanced assignment with two or more arms. A numeric matrix with one row per unit and one column per condition, giving each unit's probability of assignment to each condition. Rows must sum to 1. Supplying this argument selects \code{\link{balanced_ra}()}. (optional)
#' @param block_m Use for a two-arm blocked design: a vector giving the number of units to assign to treatment within each block, in the order of \code{sort(unique(blocks))}. (optional)
#' @param block_m_each Use for a multi-arm blocked design. A matrix with one row per block and one column per treatment arm giving the number of units assigned to each condition within each block. Rows respect the ordering of \code{sort(unique(blocks))}. (optional)
#' @param block_prob Use for a two-arm blocked design in which the treatment probability varies across blocks. In the order of \code{sort(unique(blocks))}. (optional)
#' @param block_prob_each Use for a multi-arm blocked design in which treatment probabilities vary across blocks. A matrix with one row per block and one column per arm; each row must sum to 1. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named 0 and 1 in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param simple Logical, defaults to \code{FALSE}. If \code{TRUE}, simple random assignment is used. Do not specify \code{m}, \code{m_each}, \code{block_m}, or \code{block_m_each} when \code{simple = TRUE}. (optional)
#' @param ra_type Optional override. The only accepted value is \code{"balanced"}, which selects \code{\link{balanced_ra}()} and allows \code{prob_unit} to vary across units. Other designs are inferred from the arguments supplied; they cannot be forced with this argument. (optional)
#' @param formula For balanced assignment. A model formula whose model matrix is the balancing matrix \eqn{X} in the cube method, e.g. \code{~ x + B}. The intercept is the count constraint. Do not also pass \code{blocks}. Supplying \code{formula} selects \code{\link{balanced_ra}()}. Two-arm only. The formula's variables are looked up once, when the design is declared; \code{\link{conduct_ra}()} reuses the matrix built then, so a later change to those variables does not change the declared design. (optional)
#' @param permutation_matrix For random assignment procedures that none of the other arguments can describe. A matrix with one row per unit and one column per assignment the procedure can produce, whose entries are condition names. Supplying it declares a design that draws one of those columns at random with equal probability, and the probabilities of assignment are read off the matrix by counting how often each unit appears in each condition. Build the matrix by calling your own assignment function many times and binding the results, or with \code{\link{obtain_permutation_matrix}()} for a design randomizr already knows. Ignored if \code{NULL}. (optional)
#' @param check_inputs Logical. Whether to verify before declaring that the arguments are internally consistent: that counts sum to N, that probabilities lie between 0 and 1 and sum to 1, that block-level arguments have one entry per block, and so on. Defaults to \code{TRUE}. The check also fills in arguments that were left implicit, notably \code{conditions}, so with \code{FALSE} every argument the design needs must be supplied explicitly. It is skipped entirely when \code{permutation_matrix} is supplied. (optional)
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
#' # A declaration is used in three ways.
#'
#' # 1. To obtain some basic facts about a randomization:
#'
#' declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))
#' declaration
#'
#' # 2. To conduct a random assignment:
#'
#' Z <- conduct_ra(declaration)
#' table(Z)
#'
#' # 3. To obtain the probability that each unit is in the condition it is in:
#'
#' probs <- obtain_condition_probabilities(declaration, Z)
#' table(probs, Z)
#'
#'
#' # Simple Random Assignment Declarations
#'
#' declare_ra(N = 100, simple = TRUE)
#'
#' declare_ra(N = 100, prob = 0.4, simple = TRUE)
#'
#' declare_ra(N = 100, prob_each = c(0.3, 0.3, 0.4),
#'            conditions = c("control", "placebo", "treatment"), simple = TRUE)
#'
#'
#' # Complete Random Assignment Declarations
#'
#' declare_ra(N = 100)
#'
#' declare_ra(N = 100, m_each = c(30, 70),
#'            conditions = c("control", "treatment"))
#'
#' declare_ra(N = 100, m_each = c(30, 30, 40))
#'
#'
#' # Block Random Assignment Declarations
#'
#' blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#' declare_ra(blocks = blocks)
#'
#' # One row per block, one column per arm
#' block_m_each <- rbind(c(10, 40),
#'                       c(30, 70),
#'                       c(50, 150))
#'
#' declare_ra(blocks = blocks, block_m_each = block_m_each)
#'
#'
#' # Cluster Random Assignment Declarations
#'
#' clusters <- rep(letters[1:10], times = 1:10)
#'
#' declare_ra(clusters = clusters)
#'
#' declare_ra(clusters = clusters, m_each = c(3, 3, 4))
#'
#'
#' # Blocked and Clustered Random Assignment Declarations
#'
#' clusters <- rep(letters[1:12], times = 1:12)
#'
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:3]] <- "block_1"
#' blocks[clusters %in% letters[4:6]] <- "block_2"
#' blocks[clusters %in% letters[7:9]] <- "block_3"
#' blocks[clusters %in% letters[10:12]] <- "block_4"
#'
#' table(blocks, clusters)
#'
#' declare_ra(clusters = clusters, blocks = blocks)
#'
#' declare_ra(clusters = clusters, blocks = blocks, prob_each = c(0.2, 0.5, 0.3))
#'
#'
#' # Balanced assignment (heterogeneous probabilities, tight counts).
#' # Opt-in: without ra_type or prob_unit_each this remains complete assignment.
#'
#' p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
#' declare_ra(prob_unit = p, ra_type = "balanced")
#'
#' P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
#' declare_ra(prob_unit_each = P)
#'
#' x <- c(0, 1, 5, 6, 8, 9)
#' declare_ra(formula = ~ x)
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
                       prob_unit_each = NULL,
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob = NULL,
                       block_prob_each = NULL,
                       num_arms = NULL,
                       conditions = NULL,
                       simple = FALSE,
                       ra_type = NULL,
                       formula = NULL,
                       permutation_matrix = NULL,
                       check_inputs = TRUE) {
  input_check <- NULL
  all_args <-  mget(names(formals(sys.function())))
  ra_type_arg <- all_args$ra_type
  all_args$ra_type <- NULL

  if (!is.null(ra_type_arg) && !identical(ra_type_arg, "balanced")) {
    stop("`ra_type` accepts only \"balanced\" as an explicit override. ",
         "Other designs are inferred from the arguments supplied.",
         call. = FALSE)
  }

  # Balanced is opt-in. Existing declare_ra(N, prob = 0.5) stays complete.
  # prob_unit_each and formula exist only on balanced_ra, so supplying either
  # selects this path.
  is_balanced <- identical(ra_type_arg, "balanced") ||
    !is.null(all_args$prob_unit_each) ||
    !is.null(all_args$formula)

  if (is_balanced) {
    all_args <- prepare_balanced_ra_args(all_args, check_inputs,
                                        envir = parent.frame())
    ra_type <- "balanced"
  } else {
    if (check_inputs && is.null(permutation_matrix)) {
      input_check <- check_randomizr_arguments_new(all_args)
      for (i in names(input_check))
        all_args[[i]] <- input_check[[i]]
      all_args$check_inputs <-
        FALSE # don't need to recheck when using declaration
    }

    is_block <- !is.null(blocks) || is.factor(blocks)
    is_clust <- !is.null(clusters) || is.factor(clusters)

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


#' Conduct a Random Assignment
#'
#' \code{conduct_ra} draws one random assignment from a design. Give it a
#' declaration made by \code{\link{declare_ra}()}, or describe the design inline
#' with the same arguments \code{declare_ra()} takes. Declaring first pays off
#' when the same design is drawn repeatedly, or when the assignment
#' probabilities are needed later by \code{\link{obtain_condition_probabilities}()}.
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}()}. Supply either a declaration or the design arguments listed below, which are the ones \code{declare_ra()} takes: given those, \code{conduct_ra} builds a declaration internally and draws one assignment from it. (optional)
#' @inheritParams declare_ra
#' @examples
#' # Declare the design once, then draw from it
#' declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))
#'
#' Z <- conduct_ra(declaration = declaration)
#' table(Z)
#'
#' # Equivalent, and convenient for a one-off assignment: describe the design
#' # inline and skip the declaration
#' Z <- conduct_ra(N = 100, m_each = c(30, 30, 40))
#' table(Z)
#'
#' @return A vector of length N giving the treatment condition of each unit,
#'   numeric in a two-arm design and a factor (ordered by \code{conditions}) in
#'   a multi-arm design.
#' @seealso \code{\link{declare_ra}()}, \code{\link{obtain_condition_probabilities}()}
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

#' Obtain the Probability of the Condition Each Unit Is In
#'
#' A declaration holds the probability of every condition for every unit. \code{obtain_condition_probabilities} picks out, for each unit, the one probability that corresponds to the condition it was actually assigned to. Give it a declaration made by \code{\link{declare_ra}()}, or describe the design inline with the same arguments \code{declare_ra()} takes.\cr \cr
#' This function is especially useful when units have different probabilities of assignment and the analyst plans to use inverse-probability weights: the weights are the reciprocals of what it returns.
#'
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}()}. Supply either a declaration or the design arguments that \code{declare_ra()} takes. (optional)
#' @param assignment A vector of random assignments, often created by \code{\link{conduct_ra}()}. (required)
#' @inheritParams declare_ra
#'
#' @examples
#'
#' # Conduct a block random assignment in which the blocks have different
#' # probabilities of assignment to treatment
#' blocks <- rep(c("A", "B", "C"), times = c(50, 100, 200))
#'
#' block_m_each <- rbind(c(10, 40),
#'                       c(30, 70),
#'                       c(50, 150))
#'
#' declaration <- declare_ra(blocks = blocks, block_m_each = block_m_each)
#'
#' Z <- conduct_ra(declaration = declaration)
#' table(Z, blocks)
#'
#' observed_probabilities <-
#'    obtain_condition_probabilities(declaration = declaration, assignment = Z)
#'
#' # Probabilities in the control group:
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#'
#' # Probabilities in the treatment group:
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#' # The weights for an inverse-probability-weighted regression
#' ipw <- 1 / observed_probabilities
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' Z <- conduct_ra(blocks = blocks, block_m_each = block_m_each)
#'
#' observed_probabilities <-
#'    obtain_condition_probabilities(assignment = Z,
#'                                   blocks = blocks,
#'                                   block_m_each = block_m_each)
#'
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#' @return A vector of length N giving, for each unit, the probability that it
#'   was assigned to the condition it is actually in. These are the quantities
#'   inverse-probability weights are built from: weight each unit by the
#'   reciprocal of its value here.
#' @seealso \code{\link{declare_ra}()}, \code{\link{conduct_ra}()}
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
        "ra_complete" = "Complete",
        "ra_balanced" = "Balanced"
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

#' Map declare_ra arguments onto balanced_ra and validate
#'
#' @keywords internal
#' @noRd
prepare_balanced_ra_args <- function(all_args, check_inputs,
                                    envir = parent.frame()) {
  if (isTRUE(all_args$simple)) {
    stop("Cannot combine balanced assignment with `simple = TRUE`.",
         call. = FALSE)
  }
  if (!is.null(all_args$permutation_matrix)) {
    stop("Cannot combine balanced assignment with `permutation_matrix`.",
         call. = FALSE)
  }

  count_args <- c("m", "m_unit", "m_each",
                  "block_m", "block_m_each", "block_prob", "block_prob_each")
  specified_count <- count_args[!vapply(all_args[count_args], is.null, logical(1))]
  if (length(specified_count)) {
    stop("Balanced assignment is specified with probabilities ",
         "(`prob_unit`, `prob_unit_each`, `prob`, or `prob_each`), not with `",
         paste(specified_count, collapse = "`, `"), "`.",
         call. = FALSE)
  }

  n_prob <- sum(!vapply(
    all_args[c("prob", "prob_unit", "prob_each", "prob_unit_each")],
    is.null,
    logical(1)
  ))
  if (n_prob > 1L) {
    stop("Supply only one of `prob`, `prob_unit`, `prob_each`, and `prob_unit_each`.",
         call. = FALSE)
  }

  if (!is.null(all_args$prob)) {
    all_args$prob_unit <- all_args$prob
    all_args$prob <- NULL
  }

  n <- all_args$N
  if (is.null(n)) {
    if (!is.null(all_args$prob_unit_each)) {
      n <- nrow(as.matrix(all_args$prob_unit_each))
    } else if (!is.null(all_args$prob_unit) && length(all_args$prob_unit) > 1L) {
      n <- length(all_args$prob_unit)
    } else if (!is.null(all_args$blocks)) {
      n <- length(all_args$blocks)
    } else if (!is.null(all_args$clusters)) {
      n <- length(all_args$clusters)
    } else if (!is.null(all_args$formula)) {
      n <- n_from_formula(all_args$formula, envir = envir)
    }
  }

  if (!is.null(all_args$prob_each)) {
    if (is.null(n)) {
      stop("With `prob_each`, supply `N`, `blocks`, or `clusters` ",
           "so the number of units is known.",
           call. = FALSE)
    }
    pe <- all_args$prob_each
    if (!is.numeric(pe) || anyNA(pe) || any(pe < 0) || abs(sum(pe) - 1) > 1e-8) {
      stop("`prob_each` must be a numeric vector of nonnegative values that sum to 1.",
           call. = FALSE)
    }
    all_args$prob_unit_each <- matrix(pe, n, length(pe), byrow = TRUE)
    all_args$prob_each <- NULL
  }

  if (is.null(all_args$prob_unit) && is.null(all_args$prob_unit_each)) {
    if (is.null(n)) {
      stop("N, blocks, clusters, or a probability vector/matrix must be specified.",
           call. = FALSE)
    }
    # Same default as complete_ra: two-arm p = 0.5 unless num_arms or
    # conditions imply k arms, in which case each gets probability 1/k.
    k_default <- all_args$num_arms
    if (is.null(k_default) && !is.null(all_args$conditions)) {
      k_default <- length(all_args$conditions)
    }
    if (!is.null(k_default) && k_default != 2L) {
      all_args$prob_unit_each <- matrix(1 / k_default, n, k_default)
    } else {
      all_args$prob_unit <- 0.5
    }
  }

  all_args$N <- n

  if (!is.null(all_args$formula)) {
    if (!is.null(all_args$blocks)) {
      stop("Use B in the formula, or use blocks=, not both.", call. = FALSE)
    }
    if (!is.null(all_args$prob_unit_each)) {
      stop("`formula` is not yet supported with `prob_unit_each`.",
           call. = FALSE)
    }
    # Resolve the balancing matrix now, while the environment the formula was
    # written in is still live, and carry it in the declaration. conduct_ra()
    # then never looks the formula's variables up again.
    if (!is.null(n)) {
      all_args$.X <- balanced_formula_matrix(all_args$formula, n, envir = envir)
    }
  }

  P <- balanced_ra_matrix(
    if (is.null(all_args$prob_unit_each)) all_args$prob_unit else NULL,
    all_args$prob_unit_each,
    all_args$blocks,
    all_args$clusters,
    all_args$N,
    all_args$num_arms,
    check_inputs = check_inputs
  )
  k <- ncol(P)
  if (is.null(all_args$N)) {
    all_args$N <- nrow(P)
  }
  if (is.null(all_args$num_arms)) {
    all_args$num_arms <- k
  }
  if (is.null(all_args$conditions)) {
    all_args$conditions <- if (k == 2L && is.null(all_args$prob_unit_each)) {
      c(0, 1)
    } else {
      paste0("T", seq_len(k))
    }
  }
  if (length(all_args$conditions) != k) {
    stop("`conditions` must have one entry per condition. You supplied ",
         length(all_args$conditions), " for ", k, " conditions.",
         call. = FALSE)
  }

  all_args$check_inputs <- FALSE
  all_args
}
