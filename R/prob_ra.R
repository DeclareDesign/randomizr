#' Random assignment with heterogeneous probabilities
#'
#' \strong{Experimental.} `prob_ra` assigns units to conditions when the
#' probability of assignment varies from unit to unit, holding the number
#' assigned to each condition as close to its target as arithmetic allows. It
#' fills the gap between [simple_ra()], which honours unit-varying probabilities
#' but lets the number treated wander, and [complete_ra()], which fixes the
#' number treated but requires every unit to share the same probability.
#'
#' Two situations motivate it, both due to Macartan Humphreys, whose `probra`
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
#' @section What is guaranteed:
#' Every unit receives exactly one condition. Each unit's probability of
#' receiving each condition is exactly the probability supplied. The number of
#' units in each condition is either the floor or the ceiling of that
#' condition's expected count, never further away.
#'
#' With `blocks`, the tight counts are the within-block ones, which is the
#' blocked design generalised to unequal probabilities. Without `blocks`, the
#' tight counts are the overall ones. Both cannot be guaranteed at once in
#' general, so `prob_ra` guarantees whichever the call asks for.
#'
#' With `clusters`, the assignment happens at the cluster level and the counts
#' held tight are counts of clusters. Clusters of unequal size therefore give a
#' fixed number of treated clusters and a varying number of treated units, which
#' is how [cluster_ra()] behaves. `blocks` and `clusters` combine, giving a tight
#' number of treated clusters within each block.
#'
#' @section Cost:
#' The two-arm case runs in time linear in the number of units, which is what
#' makes it usable inside a simulation loop: 2,000 units in 50 blocks takes
#' about a millisecond and a half per draw. Designs with three or more
#' conditions use the general walk, which rebuilds its graph on each move and so
#' grows faster than linearly: about 5 milliseconds per draw at 60 units and
#' three arms, 0.23 seconds at 600 units and four arms. The fast cube algorithm
#' of Chauvet and Tillé (2006) would make the multi-arm case linear too and is
#' the obvious next step if that cost bites.
#'
#' @section Experimental:
#' This function is new in randomizr 2.0.0 and its interface may change. It does
#' not yet participate in [declare_ra()], so [conduct_ra()] and
#' [obtain_condition_probabilities()] do not accept a `prob_ra` design. Use
#' `prob_ra_probabilities()` to recover assignment probabilities.
#'
#' @param prob_unit A numeric vector of length N giving each unit's probability
#'   of assignment to treatment, for a two-arm design. Unlike elsewhere in
#'   randomizr these need not be equal across units; varying them is the point
#'   of this function.
#' @param prob_unit_each A numeric matrix with one row per unit and one column
#'   per condition, giving each unit's probability of assignment to each
#'   condition, for a multi-arm design. Rows must sum to 1.
#' @param blocks A vector of length N indicating which block each unit belongs
#'   to. When supplied, counts are held tight within each block.
#' @param clusters A vector of length N indicating which cluster each unit
#'   belongs to. Whole clusters are assigned together, so the probabilities must
#'   be the same for every unit in a cluster, and the tight counts become counts
#'   of clusters rather than of units. May be combined with `blocks`, in which
#'   case every cluster must sit entirely inside one block.
#' @param N The number of units. Inferred from the other arguments when omitted.
#' @param num_arms The number of treatment arms. Inferred when omitted.
#' @param conditions A vector giving the names of the conditions.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N giving the condition of each unit, numeric in a
#'   two-arm design and a factor (ordered by `conditions`) in a multi-arm design.
#'
#' @references
#' Deville, J.-C. and Tillé, Y. (2004). Efficient balanced sampling: the cube
#' method. \emph{Biometrika} 91(4), 893-912.
#'
#' @seealso \code{\link{prob_ra_probabilities}}, \code{\link{complete_ra}},
#'   \code{\link{block_ra}}, \code{\link{simple_ra}}
#'
#' @examples
#' # A race between contestants with unequal chances, in which exactly one wins
#' # because the chances sum to 1.
#' chances <- c(0.5, 0.3, 0.15, 0.05)
#' winners <- replicate(1000, which(prob_ra(prob_unit = chances) == 1))
#' table(winners) / 1000     # close to chances
#'
#' # Unequal probabilities, two arms, with the number treated held tight.
#' p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
#' Z <- prob_ra(prob_unit = p)
#' table(Z)
#'
#' # Repeating the draw: probabilities are honoured, and exactly 3 are treated
#' # every time because the probabilities sum to 3.
#' reps <- replicate(1000, prob_ra(prob_unit = p))
#' rowMeans(reps)          # close to p
#' table(colSums(reps))    # always 3
#'
#' # Two districts of three villages, three to be treated, blocked by district.
#' # The per-district target is 1.5, so each district gets one or two.
#' districts <- rep(c("north", "south"), each = 3)
#' reps <- replicate(1000, prob_ra(prob_unit = rep(0.5, 6), blocks = districts))
#' table(colSums(reps[districts == "north", ]))
#'
#' # Three arms with unit-varying probabilities.
#' P <- cbind(c(.15, .47), c(.65, .48), c(.20, .05))
#' table(replicate(1000, prob_ra(prob_unit_each = P))[1, ])
#'
#' # Whole clusters assigned together, with unequal cluster probabilities. The
#' # number of treated clusters is fixed; the number of treated units is not,
#' # because the clusters differ in size.
#' clusters <- rep(1:6, times = c(3, 1, 4, 2, 5, 3))
#' p_cluster <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
#' Z <- prob_ra(prob_unit = p_cluster[clusters], clusters = clusters)
#' table(clusters, Z)
#'
#' # Blocks and clusters together: a tight number of treated clusters in each
#' # block.
#' blocks <- ifelse(clusters <= 3, "east", "west")
#' Z <- prob_ra(prob_unit = rep(0.5, length(clusters)),
#'              clusters = clusters, blocks = blocks)
#' table(blocks, Z)
#'
#' @export
prob_ra <- function(prob_unit = NULL,
                    prob_unit_each = NULL,
                    blocks = NULL,
                    clusters = NULL,
                    N = NULL,
                    num_arms = NULL,
                    conditions = NULL,
                    check_inputs = TRUE) {

  P <- prob_ra_matrix(prob_unit, prob_unit_each, blocks, clusters, N, num_arms,
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
#' each condition under [prob_ra()]. Because those probabilities are supplied by
#' the caller rather than derived from a design, this function mainly validates
#' and normalises them into the matrix form the other `_probabilities` functions
#' return.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each unit by the reciprocal of the probability of the condition it landed in.
#'
#' @inheritParams prob_ra
#' @return A matrix of probabilities of assignment, one row per unit and one
#'   column per condition, with columns named `prob_<condition>`.
#' @seealso \code{\link{prob_ra}}
#' @examples
#' prob_ra_probabilities(prob_unit = c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5))
#' @export
prob_ra_probabilities <- function(prob_unit = NULL,
                                  prob_unit_each = NULL,
                                  blocks = NULL,
                                  clusters = NULL,
                                  N = NULL,
                                  num_arms = NULL,
                                  conditions = NULL,
                                  check_inputs = TRUE) {
  P <- prob_ra_matrix(prob_unit, prob_unit_each, blocks, clusters, N, num_arms,
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
prob_ra_matrix <- function(prob_unit, prob_unit_each, blocks, clusters, N,
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
#' With `blocks`, the right-hand nodes are (block, condition) pairs rather than
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
cube_two_arm <- function(p, blocks = NULL, tol = 1e-12) {
  n <- length(p)
  z <- p
  b <- if (is.null(blocks)) rep(1L, n) else as.integer(factor(blocks))
  ord <- sample.int(n)                    # the input order must not matter
  u <- runif(n + 1L)
  ui <- 0L
  for (bl in unique(b)) {
    idx <- ord[b[ord] == bl]
    open <- NA_integer_
    for (t in idx) {
      if (z[t] <= tol || z[t] >= 1 - tol) next
      if (is.na(open)) { open <- t; next }
      i <- open; j <- t
      du <- min(1 - z[i], z[j])
      dd <- min(z[i], 1 - z[j])
      ui <- ui + 1L
      if (u[ui] < dd / (du + dd)) { z[i] <- z[i] + du; z[j] <- z[j] - du }
      else                        { z[i] <- z[i] - dd; z[j] <- z[j] + dd }
      open <- if (z[i] > tol && z[i] < 1 - tol) i else j
      if (z[open] <= tol || z[open] >= 1 - tol) open <- NA_integer_
    }
    # At most one unit per block survives; rounding it fairly moves that block's
    # count by less than one, so it stays floor-or-ceiling.
    if (!is.na(open)) { ui <- ui + 1L; z[open] <- as.numeric(u[ui] < z[open]) }
  }
  round(z)
}

cube_assign <- function(P, blocks = NULL, tol = 1e-12) {
  n <- nrow(P); k <- ncol(P)
  # Two conditions collapse to a single vector, where the walk has a linear-time
  # form. See cube_two_arm().
  if (k == 2L) {
    z <- cube_two_arm(P[, 2L], blocks, tol)
    return(cbind(1 - z, z))
  }
  b <- if (is.null(blocks)) rep(1L, n) else as.integer(factor(blocks))
  Z <- P
  # Each move fixes at least one of the n * k cells, so this cannot spin.
  for (iter in seq_len(n * k + 1L)) {
    fr <- which(Z > tol & Z < 1 - tol, arr.ind = TRUE)
    if (!nrow(fr)) break
    nE <- nrow(fr)
    rid <- n + (b[fr[, "row"]] - 1L) * k + fr[, "col"]
    ends <- cbind(fr[, "row"], rid)
    # split() rather than a double loop appending with c(): the loop copies the
    # growing vector on every append, which made this the dominant cost.
    nv <- n + max(b) * k
    adj <- split(rep.int(seq_len(nE), 2L), factor(c(ends[, 1L], ends[, 2L]),
                                                 levels = seq_len(nv)))
    deg <- lengths(adj)
    leaf <- which(deg == 1L)
    leaf <- leaf[leaf > n]
    v <- if (length(leaf)) leaf[1L] else ends[1L, 1L]

    used <- logical(nE)                     # membership test instead of setdiff
    seen <- integer(nv)                     # position of each node in the walk
    vs <- integer(nE + 1L); vs[1L] <- v; seen[v] <- 1L
    es <- integer(nE); ne <- 0L; cyc <- NULL
    repeat {
      cand <- adj[[v]]
      cand <- cand[!used[cand]]
      if (!length(cand)) break
      e <- cand[1L]
      w <- if (ends[e, 1L] == v) ends[e, 2L] else ends[e, 1L]
      used[e] <- TRUE
      ne <- ne + 1L; es[ne] <- e
      if (seen[w]) { cyc <- es[seen[w]:ne]; break }
      vs[ne + 1L] <- w; seen[w] <- ne + 1L
      v <- w
    }
    es <- es[seq_len(ne)]

    idx <- fr[if (is.null(cyc)) es else cyc, , drop = FALSE]
    sgn <- rep(c(1, -1), length.out = nrow(idx))
    zz <- Z[idx]
    dplus  <- min(ifelse(sgn > 0, 1 - zz, zz))
    dminus <- min(ifelse(sgn > 0, zz, 1 - zz))
    if (!is.finite(dplus + dminus) || dplus + dminus <= 0) break
    Z[idx] <- if (runif(1L) < dminus / (dplus + dminus)) zz + sgn * dplus
              else                                       zz - sgn * dminus
    Z[Z < tol] <- 0
    Z[Z > 1 - tol] <- 1
  }
  round(Z)
}
