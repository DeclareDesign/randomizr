#' Random assignment with heterogeneous probabilities
#'
#' \strong{Experimental.} \code{balanced_ra} assigns units to conditions when the
#' probability of assignment varies from unit to unit, holding the number
#' assigned to each condition as close to its target as arithmetic allows. It
#' fills the gap between [simple_ra()], which honours unit-varying probabilities
#' but lets the number treated wander, and [complete_ra()], which fixes the
#' number treated but requires every unit to share the same probability.
#'
#' The "balanced" in the name is balanced sampling in the sense of Deville and
#' Tillé (2004), meaning that the realised counts are held against their
#' targets. It does not refer to covariate balance, which is a different idea
#' and not what this function does.
#'
#' Two situations motivate it, both due to Macartan Humphreys, whose \code{probra}
#' package is the origin of this function.
#'
#' Imagine a race between contestants with unequal chances of winning, and you
#' want to simulate outcomes that respect those chances while exactly one
#' contestant wins each time. The design is neither simple nor complete: simple
#' random assignment gets the probabilities right and the winner count wrong,
#' complete random assignment does the reverse.
#'
#' Or imagine two districts of three villages each, three villages to be
#' treated, assignment probabilities equal, and blocking by district. Three does
#' not divide evenly into two districts, so the per-district target is 1.5 and
#' no design can hit it every time. What one wants is for each district to
#' receive one or two villages, never zero and never three.
#'
#' @section How the counts are held tight:
#' The assignment is drawn by the cube method of Deville and Tillé (2004),
#' specialised to this problem. Starting from the matrix of probabilities, the
#' algorithm repeatedly finds a cycle among the cells that are still fractional
#' and moves along it at random, in a way that leaves every unit's total and
#' every condition's total unchanged and drives at least one cell to 0 or 1.
#' When no cycle remains it moves along a path instead, which disturbs only the
#' two conditions at the ends. Each move is a fair bet, so the probability of
#' any unit landing in any condition is exactly what was asked for, and the
#' process stops at a genuine assignment rather than a rounded one.
#'
#' Each number of conditions takes the route suited to it. With two, the state
#' is a single vector, and the move reduces to picking two units in the same
#' block and shifting mass from one to the other, which is the pivotal method of
#' Deville and Tillé (1998). With three or more, the algorithm carries a working
#' set of as many units as there are conditions, which is always enough to
#' contain a cycle, and refills it as units settle; that is the fast cube
#' algorithm of Chauvet and Tillé (2006) in the form this problem takes.
#'
#' @section What is guaranteed:
#' Every unit receives exactly one condition. Each unit's probability of
#' receiving each condition is exactly the probability supplied. The number of
#' units in each condition is either the floor or the ceiling of that
#' condition's expected count, never further away.
#'
#' With \code{blocks}, the tight counts are the within-block ones, which is the
#' blocked design generalised to unequal probabilities. Without \code{blocks}, the
#' tight counts are the overall ones. Both cannot be guaranteed at once in
#' general, so \code{balanced_ra} guarantees whichever the call asks for.
#'
#' With \code{clusters}, the assignment happens at the cluster level and the counts
#' held tight are counts of clusters. Clusters of unequal size therefore give a
#' fixed number of treated clusters and a varying number of treated units, which
#' is how [cluster_ra()] behaves. \code{blocks} and \code{clusters} combine, giving a tight
#' number of treated clusters within each block.
#'
#' @section Inference:
#' Holding counts fixed makes assignments dependent across units. So does
#' [complete_ra()], and to much the same degree: the covariance between two
#' units' assignments is negative and of the same order under both, where under
#' [simple_ra()] it is zero. The dependence is the price of fixing the count
#' rather than anything new, and it is benign for the usual variance estimators.
#' With equal probabilities, where this function amounts to complete random
#' assignment, HC2 recovers the sampling distribution as well as it does there,
#' and stays conservative when treatment effects vary.
#'
#' With unequal probabilities and an inverse-probability-weighted estimator, HC2
#' runs one to two percent small, so a nominal 95 percent interval covers about
#' 94.5. Most of that comes from the weighting rather than from the design,
#' since simple random assignment with the same probabilities behaves the same
#' way, and it shrinks as N grows.
#'
#' \strong{Estimate blocked designs with block fixed effects.} Blocking on
#' something that predicts the outcome buys a great deal of precision. With
#' blocks cut from a covariate explaining 80 percent of the variance in the
#' untreated outcome, the estimator is four times as precise as under simple
#' random assignment analysed the same way, and the gain grows with how well the
#' covariate predicts. Adjusting for those same strata in the estimator buys
#' most of the same thing, so blocking the design and adjusting the estimator
#' are close substitutes rather than additive; doing both is only slightly
#' better than either alone.
#'
#' What is not optional is the reporting. An estimator that ignores the blocks
#' keeps every bit of that precision and cannot see any of it, reporting
#' standard errors twice as wide as they should be, for 100 percent coverage of
#' a nominal 95 percent interval. That holds for any blocked design, including
#' [block_ra()], and block fixed effects repair it.
#'
#' \strong{Where [block_ra()] applies, the two agree.} With probabilities
#' constant within block, this function and [block_ra()] are indistinguishable
#' on the same blocks under the same estimator. \code{balanced_ra} is the
#' generalisation rather than a competitor: [block_ra()] requires one
#' probability per block and refuses unit-varying ones, which is the case this
#' function exists to serve.
#'
#' @section Cost:
#' Both paths are linear in the number of units and written in C++, so a draw is
#' cheap enough to sit inside a simulation loop. With two conditions, 2,000
#' units in 50 blocks take about a quarter of a millisecond and 10,000 units
#' about 1.2 milliseconds. With three or more, 2,000 units in four conditions
#' take about 9 milliseconds and 10,000 units about 46. Cost grows with roughly
#' the square of the number of conditions, so ten conditions on 2,000 units run
#' to about 58 milliseconds.
#'
#' @section Experimental:
#' This function is new in randomizr 2.0.0 and its interface may change. It does
#' not yet participate in [declare_ra()], so [conduct_ra()] and
#' [obtain_condition_probabilities()] do not accept a \code{balanced_ra} design. Use
#' \code{balanced_ra_probabilities()} to recover assignment probabilities.
#'
#' @param prob_unit A numeric vector of length N giving each unit's probability
#'   of assignment to treatment, for a two-arm design. Unlike elsewhere in
#'   randomizr these need not be equal across units; varying them is the point
#'   of this function. Supply either \code{prob_unit} or \code{prob_unit_each}. (optional)
#' @param prob_unit_each A numeric matrix with one row per unit and one column
#'   per condition, giving each unit's probability of assignment to each
#'   condition, for a multi-arm design. Rows must sum to 1. Supply either
#'   \code{prob_unit} or \code{prob_unit_each}. (optional)
#' @param blocks A vector of length N indicating which block each unit belongs
#'   to. When supplied, counts are held tight within each block. (optional)
#' @param clusters A vector of length N indicating which cluster each unit
#'   belongs to. Whole clusters are assigned together, so the probabilities must
#'   be the same for every unit in a cluster, and the tight counts become counts
#'   of clusters rather than of units. May be combined with \code{blocks}, in which
#'   case every cluster must sit entirely inside one block. (optional)
#' @param N The number of units. Inferred from the other arguments when omitted. (optional)
#' @param num_arms The number of treatment arms. Inferred when omitted. (optional)
#' @param conditions A vector giving the names of the conditions. (optional)
#' @param check_inputs Logical. Whether to verify before assigning that the arguments are internally consistent: that probabilities lie between 0 and 1, that rows of a probability matrix sum to 1, that probabilities are constant within a cluster, and that clusters nest within blocks. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many assignments from probabilities that have already been verified. (optional)
#'
#' @return A vector of length N giving the condition of each unit, numeric in a
#'   two-arm design and a factor (ordered by \code{conditions}) in a multi-arm design.
#'
#' @references
#' Deville, J.-C. and Tillé, Y. (2004). Efficient balanced sampling: the cube
#' method. \emph{Biometrika} 91(4), 893-912. \doi{10.1093/biomet/91.4.893}
#'
#' Deville, J.-C. and Tillé, Y. (1998). Unequal probability sampling without
#' replacement through a splitting method. \emph{Biometrika} 85(1), 89-101.
#' \doi{10.1093/biomet/85.1.89}
#'
#' Chauvet, G. and Tillé, Y. (2006). A fast algorithm for balanced sampling.
#' \emph{Computational Statistics} 21(1), 53-62.
#' \doi{10.1007/s00180-006-0250-2}
#'
#' Deville, J.-C. and Tillé, Y. (2005). Variance approximation under balanced
#' sampling. \emph{Journal of Statistical Planning and Inference} 128(2),
#' 569-591. \doi{10.1016/j.jspi.2003.11.011}
#'
#' @seealso \code{\link{balanced_ra_probabilities}()}, \code{\link{complete_ra}()},
#'   \code{\link{block_ra}()}, \code{\link{simple_ra}()}
#'
#' @examples
#' # A race between contestants with unequal chances, in which exactly one wins
#' # because the chances sum to 1.
#' chances <- c(0.5, 0.3, 0.15, 0.05)
#' winners <- replicate(1000, which(balanced_ra(prob_unit = chances) == 1))
#' table(winners) / 1000     # close to chances
#'
#' # Unequal probabilities, two arms, with the number treated held tight.
#' p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
#' Z <- balanced_ra(prob_unit = p)
#' table(Z)
#'
#' # Repeating the draw: probabilities are honoured, and exactly 3 are treated
#' # every time because the probabilities sum to 3.
#' reps <- replicate(1000, balanced_ra(prob_unit = p))
#' rowMeans(reps)          # close to p
#' table(colSums(reps))    # always 3
#'
#' # Two districts of three villages, three to be treated, blocked by district.
#' # The per-district target is 1.5, so each district gets one or two.
#' districts <- rep(c("north", "south"), each = 3)
#' reps <- replicate(1000, balanced_ra(prob_unit = rep(0.5, 6), blocks = districts))
#' table(colSums(reps[districts == "north", ]))
#'
#' # Three arms with unit-varying probabilities.
#' P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
#' table(replicate(1000, balanced_ra(prob_unit_each = P))[1, ])
#'
#' # Whole clusters assigned together, with unequal cluster probabilities. The
#' # number of treated clusters is fixed; the number of treated units is not,
#' # because the clusters differ in size.
#' clusters <- rep(1:6, times = c(3, 1, 4, 2, 5, 3))
#' p_cluster <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
#' Z <- balanced_ra(prob_unit = p_cluster[clusters], clusters = clusters)
#' table(clusters, Z)
#'
#' # Blocks and clusters together: a tight number of treated clusters in each
#' # block.
#' blocks <- ifelse(clusters <= 3, "east", "west")
#' Z <- balanced_ra(prob_unit = rep(0.5, length(clusters)),
#'              clusters = clusters, blocks = blocks)
#' table(blocks, Z)
#'
#' @export
balanced_ra <- function(prob_unit = NULL,
                    prob_unit_each = NULL,
                    blocks = NULL,
                    clusters = NULL,
                    N = NULL,
                    num_arms = NULL,
                    conditions = NULL,
                    check_inputs = TRUE) {

  P <- balanced_ra_matrix(prob_unit, prob_unit_each, blocks, clusters, N, num_arms,
                      check_inputs)
  Z <- if (is.null(clusters)) cube_assign(P, blocks) else
    cube_assign_clusters(P, clusters, blocks)
  k <- ncol(P)

  if (is.null(conditions)) {
    conditions <- if (k == 2 && is.null(prob_unit_each)) c(0, 1) else paste0("T", seq_len(k))
  }
  if (length(conditions) != k) {
    stop("`conditions` must have one entry per condition. You supplied ",
         length(conditions), " for ", k, " conditions.")
  }
  assignment <- conditions[max.col(Z, ties.method = "first")]
  clean_condition_names(assignment, conditions)
}

#' Probabilities of assignment: heterogeneous-probability random assignment
#'
#' \strong{Experimental.} Returns the probability that each unit is assigned to
#' each condition under [balanced_ra()]. Because those probabilities are supplied by
#' the caller rather than derived from a design, this function mainly validates
#' and normalises them into the matrix form the other \code{_probabilities} functions
#' return.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in.
#'
#' @inheritParams balanced_ra
#' @return A matrix of probabilities of assignment, one row per unit and one
#'   column per condition, with columns named \code{prob_<condition>}.
#' @seealso \code{\link{balanced_ra}()}
#' @examples
#' balanced_ra_probabilities(prob_unit = c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5))
#' @export
balanced_ra_probabilities <- function(prob_unit = NULL,
                                  prob_unit_each = NULL,
                                  blocks = NULL,
                                  clusters = NULL,
                                  N = NULL,
                                  num_arms = NULL,
                                  conditions = NULL,
                                  check_inputs = TRUE) {
  P <- balanced_ra_matrix(prob_unit, prob_unit_each, blocks, clusters, N, num_arms,
                      check_inputs)
  k <- ncol(P)
  if (is.null(conditions)) {
    conditions <- if (k == 2 && is.null(prob_unit_each)) c(0, 1) else paste0("T", seq_len(k))
  }
  colnames(P) <- paste0("prob_", conditions)
  P
}

#' Build and validate the unit-by-condition probability matrix
#'
#' The validation probra never had. randomizr refuses probabilities outside
#' [0, 1] everywhere else, and this keeps that promise here.
#'
#' @keywords internal
#' @noRd
balanced_ra_matrix <- function(prob_unit, prob_unit_each, blocks, clusters, N,
                           num_arms, check_inputs = TRUE) {
  if (is.null(prob_unit) && is.null(prob_unit_each)) {
    stop("Supply either `prob_unit` (two arms) or `prob_unit_each` (multiple arms).")
  }
  if (!is.null(prob_unit) && !is.null(prob_unit_each)) {
    stop("Supply only one of `prob_unit` and `prob_unit_each`.")
  }

  if (!is.null(prob_unit_each)) {
    P <- as.matrix(prob_unit_each)
    if (!is.numeric(P)) stop("`prob_unit_each` must be numeric.")
  } else {
    if (!is.numeric(prob_unit)) stop("`prob_unit` must be numeric.")
    if (length(prob_unit) == 1L) {
      n <- N %||% (if (!is.null(blocks)) length(blocks)
                   else if (!is.null(clusters)) length(clusters) else NULL)
      if (is.null(n)) {
        stop("With a scalar `prob_unit`, supply `N`, `blocks` or `clusters` ",
             "so the number of units is known.")
      }
      prob_unit <- rep(prob_unit, n)
    }
    P <- cbind(1 - prob_unit, prob_unit)
  }

  if (check_inputs) {
    if (anyNA(P)) stop("Assignment probabilities must not be missing.")
    if (any(P < 0) || any(P > 1)) {
      stop("Assignment probabilities must be between 0 and 1.")
    }
    if (any(abs(rowSums(P) - 1) > 1e-8)) {
      stop("Each unit's probabilities must sum to 1 across conditions.")
    }
    if (!is.null(N) && nrow(P) != N) {
      stop("`N` is ", N, " but the probabilities describe ", nrow(P), " units.")
    }
    if (!is.null(blocks) && length(blocks) != nrow(P)) {
      stop("`blocks` has length ", length(blocks), " but the probabilities ",
           "describe ", nrow(P), " units.")
    }
    if (!is.null(num_arms) && ncol(P) != num_arms) {
      stop("`num_arms` is ", num_arms, " but the probabilities describe ",
           ncol(P), " conditions.")
    }
    if (nrow(P) < 1L) stop("There must be at least one unit.")
    if (!is.null(clusters)) {
      if (length(clusters) != nrow(P)) {
        stop("`clusters` has length ", length(clusters), " but the ",
             "probabilities describe ", nrow(P), " units.")
      }
      cl <- factor(clusters)
      spread <- max(vapply(seq_len(ncol(P)), function(j)
        max(tapply(P[, j], cl, function(v) diff(range(v)))), numeric(1)))
      if (spread > 1e-9) {
        stop("Assignment probabilities must be the same for every unit in a ",
             "cluster, since a cluster is assigned as a whole.")
      }
      if (!is.null(blocks)) {
        nb <- tapply(as.character(blocks), cl, function(v) length(unique(v)))
        if (any(nb > 1L)) {
          stop("Each cluster must sit entirely inside one block. Clusters ",
               "spanning blocks: ", paste(names(nb)[nb > 1L], collapse = ", "), ".")
        }
      }
    }
  }
  P
}

#' Assign whole clusters
#'
#' Collapse to one row per cluster, assign at the cluster level, expand back.
#' The probabilities are constant within a cluster by the time this is reached,
#' so the first row of each cluster speaks for it.
#'
#' @keywords internal
#' @noRd
cube_assign_clusters <- function(P, clusters, blocks = NULL, tol = 1e-12) {
  cl <- factor(clusters)
  first <- match(levels(cl), as.character(cl))
  Pc <- P[first, , drop = FALSE]
  bc <- if (is.null(blocks)) NULL else blocks[first]
  Zc <- cube_assign(Pc, bc, tol)
  Zc[as.integer(cl), , drop = FALSE]
}

#' Cube-method flight and landing on the transportation polytope
#'
#' Repeatedly finds a cycle among the fractional cells and moves along it at
#' random, which leaves every row and column total untouched and fixes at least
#' one cell. When no cycle remains it moves along a maximal path, which shifts
#' only the two conditions at the ends.
#'
#' The reason this always terminates in a valid assignment: a unit's row sums to
#' exactly 1 and its integral entries are 0 or 1, so a unit with exactly one
#' fractional entry would need that entry to be an integer. Every unit therefore
#' has fractional degree 0 or at least 2, every leaf of the fractional graph is
#' a condition, and a walk can only ever run out of edges at a condition. So
#' either some condition is a leaf, and starting there gives a maximal
#' condition-to-condition path, or every degree is at least 2 and a cycle must
#' exist. There is no third case.
#'
#' With \code{blocks}, the right-hand nodes are (block, condition) pairs rather than
#' conditions, which moves the tight counts inside the blocks. Nothing else
#' changes.
#'
#' @keywords internal
#' @noRd

#' Two-arm fast path: the pivotal method
#'
#' With two conditions the state is a single vector, since the second column is
#' one minus the first, and the balancing constraints are the per-block sums. In
#' the fractional graph every unit then has exactly one edge, to its own block,
#' so the graph is a forest of stars: no cycle can exist and every maximal path
#' is unit-block-unit. The general walk collapses to "take two fractional units
#' in the same block and move one up while the other moves down", which settles
#' at least one of them per move and costs no graph rebuild.
#'
#' Written as a single pass. One fractional unit is held open, the rest are
#' walked once, and whichever of a pair survives becomes the new open unit. The
#' general routine is quadratic in the number of cells because it rebuilds the
#' graph each move; this is linear, which matters because assignment functions
#' run inside simulation loops.
#'
#' @keywords internal
#' @noRd
cube_assign <- function(P, blocks = NULL, tol = 1e-12) {
  n <- nrow(P)
  b <- if (is.null(blocks)) rep(1L, n) else as.integer(factor(blocks))
  ord <- sample.int(n)                    # the input order must not matter
  # Two conditions collapse to a single vector, where the walk has a linear-time
  # form. Both paths are in src/cube.cpp.
  if (ncol(P) == 2L) {
    z <- cube_two_arm_cpp(P[, 2L], b, ord, tol)
    return(cbind(1 - z, z))
  }
  cube_multi_cpp(P, b, ord, tol)
}

