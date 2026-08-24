#' Random assignment with tight targets
#'
#' \strong{Experimental.} \code{balanced_ra} draws random assignment with tight
#' targets: condition counts at the floor or ceiling of what the probabilities
#' imply, and, with \code{formula}, covariate totals too. Each unit's probability stays
#' exact. That is useful when probabilities vary across units, and also when
#' they do not: leftover pairing keeps two-arm blocked counts tight overall as
#' well as within each block, and cube-on-X balances a continuous covariate
#' without binning it.
#'
#' With unit-varying probabilities it fills the gap between [simple_ra()], which
#' honors those probabilities but lets the number treated wander, and
#' [complete_ra()], which fixes the number treated but requires every unit to
#' share the same probability.
#'
#' The "balanced" in the name is balanced sampling in the sense of Deville and
#' Tillé (2004). With the default arguments the realized counts are held
#' against their targets. Pass \code{formula} to add linear balancing
#' constraints on covariates (cube-on-X): the flight keeps \eqn{X'Z} near
#' \eqn{X'\pi}. Landing may drop a constraint, so exact tightness on every
#' column is not always possible. \code{blocks} is a different device: it
#' tightens counts inside discrete groups. The two cannot be combined.
#'
#' Two motivating cases: a race in which contestants have unequal chances and
#' exactly one must win; and two districts of three villages, three to treat,
#' blocked by district, so that each district should receive one or two and
#' the total should be three.
#'
#' @section What is guaranteed:
#' Every unit receives exactly one condition. Each unit's probability of each
#' condition is the probability supplied. Counts are tight within each block
#' always, and tight overall as well when there are two arms. With three or
#' more arms and \code{blocks}, the overall count can wander; see the
#' "Introduction to balanced_ra" article. With \code{clusters},
#' the tight counts are counts of clusters. With \code{formula}, first-order
#' inclusion probabilities remain exact; covariate totals are as close as
#' the landing phase allows. See the "Randomizing against continuous
#' covariates" article.
#'
#' @section Balance when probabilities vary:
#' The cube holds \eqn{X'Z} near \eqn{X'\pi}, which is the treated total of each
#' balancing column against the total its assignment probabilities imply. When
#' every unit shares a probability, that target amounts to splitting the column
#' evenly between the arms, and \code{formula} does what its name suggests. When
#' probabilities vary from unit to unit, the two targets come apart.
#'
#' Suppose \eqn{p_i} rises with \eqn{x_i}. High-\eqn{x} units are meant to be
#' treated more often, so the treated group ought to have the higher mean of
#' \eqn{x}, and it does: the average treated-minus-control difference in
#' \eqn{x} under \code{formula = ~ x} is the same one
#' \code{\link{simple_ra}()} gives on the same probabilities. What the cube
#' tightens, several-fold in the simulations in the "Randomizing against
#' continuous covariates" article, is the spread of that
#' difference around its target, and with it the Horvitz-Thompson residual
#' for the \eqn{x} total.
#'
#' In short, \code{formula} does not equalize the arms when \eqn{p_i} varies, and
#' it is not meant to. Weight by the reciprocal of the assignment probability, as
#' for any unequal-probability design;
#' \code{\link{balanced_ra_probabilities}()} returns the probabilities to weight
#' by. With a constant \eqn{p} the question does not arise.
#'
#' @section Order of the covariates:
#' The flight phase sorts units by the first non-intercept column of \eqn{X} and
#' works through them in a sliding window, so each step pairs units with nearby
#' values of that column. The design therefore balances smooth functions of the
#' first covariate and not only its linear total: in simulations at
#' \eqn{N = 200} with a constant \eqn{p}, the treated-minus-control spread in
#' \eqn{x^2} and \eqn{x^3} runs several times tighter than under
#' \code{\link{complete_ra}()}, though how much tighter varies with the
#' covariate draw, and a heavy-tailed \eqn{x} narrows the gain.
#'
#' The gain is also uneven. Only the first non-intercept column drives the
#' sort, so under \code{~ x1 + x2} the spread in \eqn{x_1^2} tightens while
#' the spread in \eqn{x_2^2} stays about where complete assignment leaves it.
#' Both linear totals are held tight. A covariate you name but
#' do not put first is balanced in its own right and in nothing else, and a
#' covariate you do not name at all is not balanced.
#'
#' Sorting is a choice made here rather than a feature of the cube method, which
#' constrains only the linear span of \eqn{X}. Put the covariate whose
#' relationship with the outcome you least trust yourself to model first in the
#' formula.
#'
#' @section Intervals are conservative:
#' \code{formula} removes assignment variance that the usual variance estimators
#' do not know about, so they report intervals wider than the true sampling
#' variability warrants. HC2 on an unadjusted regression covered the true effect
#' on every draw simulated at \eqn{N = 200} with a strongly prognostic \eqn{x},
#' its average standard error more than twice the true sampling standard
#' deviation; the article "HC2 coverage under balanced assignment" on the
#' package site carries the simulation. Horvitz-Thompson through
#' \code{estimatr::horvitz_thompson()} runs from correct to about twice too
#' wide, depending on how well \eqn{X} predicts the outcome; estimatr's
#' development version (2.0.0, not yet on CRAN) warns when it is handed a
#' declaration with a formula. Coverage stayed at or above the
#' nominal rate in everything measured, so the intervals are valid. They are
#' merely wasteful.
#'
#' Covariate adjustment recovers the precision the design bought. Fitting Lin's
#' estimator on the same columns returned intervals near their nominal width and
#' coverage near 0.95. Adjustment does not always suffice: when the outcome was
#' quadratic in \eqn{x} and the adjustment was linear in \eqn{x}, coverage
#' returned to 1.000 while the sampling variance fell to about a fifth of what
#' complete assignment plus the same adjustment delivered. Insurance against a
#' misspecified adjustment model is the clearest case for this design, and it is
#' the case in which the reported interval understates what was gained.
#'
#' An exact variance for cube assignment is not a missing feature. Joint
#' inclusion probabilities have no closed form, which is what Deville and
#' Tillé (2005) approximate; randomizr does not implement that approximation.
#'
#' @section Experimental:
#' This function is new in randomizr 2.0.1 and its interface may change. Declare
#' a design with [declare_ra()] by setting \code{ra_type = "balanced"} or by
#' supplying \code{prob_unit_each} or \code{formula}; \code{\link{conduct_ra}()}
#' and \code{\link{obtain_condition_probabilities}()} then dispatch here.
#' Four studies of the function are articles on the package site rather than
#' vignettes in the package, listed at
#' \url{https://declaredesign.org/r/randomizr/articles/}: "Introduction to
#' balanced_ra" has the count-tight algorithm, "Randomizing against
#' continuous covariates" has cube-on-X, and "HC2 coverage following
#' balanced_ra" and "Speed of balanced_ra relative to complete and blocked
#' assignment" are simulation studies.
#'
#' @param N The number of units. Optional when \code{formula} or the length of
#'   \code{prob_unit} (or \code{blocks} or \code{clusters})
#'   identifies N. A single positive integer. If supplied it must match. (optional)
#' @param prob A single number between 0 and 1: the probability of assignment
#'   to treatment, shared by every unit, for a two-arm design. Defaults to 0.5
#'   when no probability argument is supplied, so \code{balanced_ra(4)} is
#'   complete assignment of four units. Supply exactly one of \code{prob},
#'   \code{prob_unit} and \code{prob_unit_each}. (optional)
#' @param prob_unit A numeric vector of length N giving each unit's probability
#'   of assignment to treatment, for a two-arm design. Unlike elsewhere in
#'   randomizr these need not be equal across units. A single number is refused,
#'   since that is what \code{prob} is for. Supply exactly one of \code{prob},
#'   \code{prob_unit} and \code{prob_unit_each}. (optional)
#' @param prob_unit_each A numeric matrix with one row per unit and one column
#'   per condition, giving each unit's probability of assignment to each
#'   condition, for a multi-arm design. Rows must sum to 1. Supply exactly one
#'   of \code{prob}, \code{prob_unit} and \code{prob_unit_each}. (optional)
#' @param blocks A vector of length N indicating which block each unit belongs
#'   to. When supplied, two-arm counts are held tight within each block and
#'   overall; with three or more arms the tight counts are the within-block ones. (optional)
#' @param clusters A vector of length N indicating which cluster each unit
#'   belongs to. Whole clusters are assigned together, so the probabilities must
#'   be the same for every unit in a cluster, and the tight counts become counts
#'   of clusters rather than of units. May be combined with \code{blocks}, in which
#'   case every cluster must sit entirely inside one block. (optional)
#' @param num_arms The number of treatment arms. Inferred when omitted. Supplied without any probability argument, \code{num_arms} (or \code{conditions}) of three or more expands to equal-probability assignment, as in \code{\link{complete_ra}()}. (optional)
#' @param conditions A vector giving the names of the conditions. (optional)
#' @param formula A model formula whose model matrix is the balancing matrix
#'   \eqn{X} in the cube method, e.g. \code{~ x + B}. The intercept column is the
#'   count constraint; \code{~ 0 + x} drops it and the treated count may wander.
#'   Names are looked up where the formula was written, then in the calling
#'   frame, so the usual \code{dat |> mutate(Z = balanced_ra(formula = ~ x))}
#'   finds the column \code{x}. Two-arm only. Cannot be combined with
#'   \code{blocks} or \code{prob_unit_each}. (optional)
#' @param check_inputs Logical. Whether to verify before assigning that the arguments are internally consistent: that probabilities lie between 0 and 1, that rows of a probability matrix sum to 1, that probabilities are constant within a cluster, and that clusters nest within blocks. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many assignments from probabilities that have already been verified. (optional)
#' @param .X Internal. A balancing matrix already built from \code{formula},
#'   supplied by \code{\link{declare_ra}()} so that the formula's variables are
#'   looked up once, when the design is declared, rather than on every draw. Not
#'   for direct use. (optional)
#'
#' @return A vector of length N giving the condition of each unit. As in
#'   \code{\link{complete_ra}()}: integer 0/1 in a two-arm design, unless
#'   \code{num_arms} or \code{conditions} is supplied explicitly, in which
#'   case a factor ordered by \code{conditions}; a factor in a multi-arm design.
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
#'   \code{\link{block_ra}()}, \code{\link{simple_ra}()},
#'   the articles \emph{Introduction to balanced_ra} and
#'   \emph{Randomizing against continuous covariates}
#'
#' @examples
#' # Four units, default probability 0.5: complete assignment of two treated.
#' table(balanced_ra(4))
#'
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
#' # Repeating the draw: probabilities are honored, and exactly 3 are treated
#' # every time because the probabilities sum to 3.
#' reps <- replicate(1000, balanced_ra(prob_unit = p))
#' rowMeans(reps)          # close to p
#' table(colSums(reps))    # always 3
#'
#' # Two districts of three villages, three to be treated, blocked by district.
#' # Each district gets one or two; the total is always three.
#' districts <- rep(c("north", "south"), each = 3)
#' reps <- replicate(1000, balanced_ra(blocks = districts))
#' table(colSums(reps))                           # always 3
#' table(colSums(reps[districts == "north", ]))   # 1 or 2
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
#' Z <- balanced_ra(prob = 0.5, clusters = clusters, blocks = blocks)
#' table(blocks, Z)
#'
#' # Cube-on-X: keep the treated total of a continuous covariate near its
#' # target. The intercept in ~ x is the count constraint. N is inferred
#' # from the looked-up formula variables.
#' x <- c(0, 1, 5, 6, 8, 9)
#' Z <- balanced_ra(formula = ~ x)
#' sum(x * Z)   # near 14.5
#'
#' @export
balanced_ra <- function(N = NULL,
                    prob = NULL,
                    prob_unit = NULL,
                    prob_unit_each = NULL,
                    blocks = NULL,
                    clusters = NULL,
                    num_arms = NULL,
                    conditions = NULL,
                    formula = NULL,
                    check_inputs = TRUE,
                    .X = NULL) {

  # num_arms or conditions without probabilities expand to equal-probability
  # balanced assignment, as they do in complete_ra() and on the declare_ra()
  # path (prepare_balanced_ra_args does the same there).
  if (is.null(prob) && is.null(prob_unit) && is.null(prob_unit_each)) {
    k_default <- num_arms %||%
      (if (!is.null(conditions)) length(conditions) else NULL)
    if (!is.null(k_default) && k_default != 2L) {
      n0 <- N %||% (if (!is.null(blocks)) length(blocks)
                    else if (!is.null(clusters)) length(clusters) else NULL)
      if (is.null(n0)) {
        stop("With `num_arms` or `conditions` alone, supply `N`, `blocks` or ",
             "`clusters` so the number of units is known.", call. = FALSE)
      }
      prob_unit_each <- matrix(1 / k_default, n0, k_default)
    }
  }
  num_arms_supplied <- !is.null(num_arms)

  prob_unit <- balanced_prob_args(prob, prob_unit, prob_unit_each, N)
  if (!is.null(formula) && !is.null(blocks)) {
    stop("Use B in the formula, or use blocks=, not both.")
  }
  if (!is.null(formula) && !is.null(prob_unit_each)) {
    stop("`formula` is not yet supported with `prob_unit_each`.")
  }
  if (!is.null(formula) && is.null(N) &&
      (is.null(prob_unit) || length(prob_unit) == 1L) &&
      is.null(prob_unit_each)) {
    N <- n_from_formula(formula, envir = parent.frame())
  }
  P <- balanced_ra_matrix(if (is.null(prob_unit_each)) prob_unit else NULL,
                      prob_unit_each, blocks, clusters, N, num_arms,
                      check_inputs)
  Z <- if (!is.null(formula)) {
    # A declaration resolves the model matrix once, at declare time, and passes
    # it in. Resolving it again here would look up the formula's variables in
    # whatever environment happens to be live at conduct time.
    X <- if (is.null(.X)) {
      balanced_formula_matrix(formula, nrow(P), envir = parent.frame())
    } else {
      balanced_check_matrix(.X, nrow(P))
    }
    z <- if (is.null(clusters)) cube_on_x_cpp(P[, 2L], X, 1e-12) else
      cube_on_x_clusters(P[, 2L], X, clusters)
    cbind(1 - z, z)
  } else if (is.null(clusters)) {
    cube_assign(P, blocks)
  } else {
    cube_assign_clusters(P, clusters, blocks)
  }
  k <- ncol(P)

  if (is.null(conditions)) {
    # complete_ra's convention: two arms are 0 and 1 however the probabilities
    # were specified, unless num_arms was supplied explicitly, which asks for
    # named arms.
    conditions <- if (k == 2 && !num_arms_supplied) c(0L, 1L) else paste0("T", seq_len(k))
  }
  if (length(conditions) != k) {
    stop("`conditions` must have one entry per condition. You supplied ",
         length(conditions), " for ", k, " conditions.")
  }
  assignment <- conditions[max.col(Z, ties.method = "first")]
  clean_condition_names(assignment, conditions)
}

#' Probabilities of assignment: Balanced Random Assignment
#'
#' \strong{Experimental.} Returns the probability that each unit is assigned to
#' each condition under [balanced_ra()]. Because those probabilities are supplied by
#' the caller rather than derived from a design, this function mainly validates
#' and normalizes them into the matrix form the other \code{_probabilities} functions
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
balanced_ra_probabilities <- function(N = NULL,
                                  prob = NULL,
                                  prob_unit = NULL,
                                  prob_unit_each = NULL,
                                  blocks = NULL,
                                  clusters = NULL,
                                  num_arms = NULL,
                                  conditions = NULL,
                                  formula = NULL,
                                  check_inputs = TRUE) {
  if (is.null(prob) && is.null(prob_unit) && is.null(prob_unit_each)) {
    k_default <- num_arms %||%
      (if (!is.null(conditions)) length(conditions) else NULL)
    if (!is.null(k_default) && k_default != 2L) {
      n0 <- N %||% (if (!is.null(blocks)) length(blocks)
                    else if (!is.null(clusters)) length(clusters) else NULL)
      if (is.null(n0)) {
        stop("With `num_arms` or `conditions` alone, supply `N`, `blocks` or ",
             "`clusters` so the number of units is known.", call. = FALSE)
      }
      prob_unit_each <- matrix(1 / k_default, n0, k_default)
    }
  }
  num_arms_supplied <- !is.null(num_arms)
  prob_unit <- balanced_prob_args(prob, prob_unit, prob_unit_each, N)
  if (!is.null(formula) && is.null(N) &&
      (is.null(prob_unit) || length(prob_unit) == 1L) &&
      is.null(prob_unit_each)) {
    N <- n_from_formula(formula, envir = parent.frame())
  }
  P <- balanced_ra_matrix(if (is.null(prob_unit_each)) prob_unit else NULL,
                      prob_unit_each, blocks, clusters, N, num_arms,
                      check_inputs)
  k <- ncol(P)
  if (is.null(conditions)) {
    conditions <- if (k == 2 && !num_arms_supplied) c(0L, 1L) else paste0("T", seq_len(k))
  }
  colnames(P) <- paste0("prob_", conditions)
  P
}

#' Build and validate the unit-by-condition probability matrix
#'
#' randomizr refuses probabilities outside [0, 1] everywhere else, and this
#' keeps that promise here.
#'
#' @keywords internal
#' @noRd
balanced_ra_matrix <- function(prob_unit, prob_unit_each, blocks, clusters, N,
                           num_arms, check_inputs = TRUE) {
  if (!is.null(N) && (!is.numeric(N) || length(N) != 1L || is.na(N) ||
                      N < 1 || N != as.integer(N))) {
    stop("`N` must be a single positive integer. ",
         "To supply probabilities, use `prob_unit`.")
  }
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
        stop("With `prob` alone, supply `N`, `blocks` or `clusters` ",
             "so the number of units is known.")
      }
      prob_unit <- rep(prob_unit, n)
    }
    P <- cbind(1 - prob_unit, prob_unit)
  }

  if (check_inputs) {
    if (!is.null(blocks) && anyNA(blocks)) {
      stop("`blocks` must not contain NA.")
    }
    if (!is.null(clusters) && anyNA(clusters)) {
      stop("`clusters` must not contain NA.")
    }
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
  if (anyNA(clusters)) stop("`clusters` must not contain NA.", call. = FALSE)
  if (length(clusters) != nrow(P)) {
    stop("`clusters` has length ", length(clusters), " but the probabilities ",
         "describe ", nrow(P), " units.", call. = FALSE)
  }
  cl <- factor(clusters)
  first <- match(levels(cl), as.character(cl))
  Pc <- P[first, , drop = FALSE]
  bc <- if (is.null(blocks)) NULL else blocks[first]
  Zc <- cube_assign(Pc, bc, tol)
  Zc[as.integer(cl), , drop = FALSE]
}

n_from_formula <- function(formula, envir = parent.frame(), data = NULL) {
  if (!is.null(data)) return(nrow(data))
  if (length(all.vars(formula)) == 0L) return(NULL)
  # na.pass, or model.matrix drops NA rows and N is silently inferred too
  # small; balanced_check_matrix() is where an NA becomes an error.
  tryCatch(
    nrow(stats::model.frame(formula,
                            data = formula_lookup_data(formula, envir),
                            na.action = stats::na.pass)),
    error = function(e) stop(conditionMessage(e), call. = FALSE)
  )
}

#' Require that every variable an argument names is a column of `data`
#'
#' The point of \code{data} is that the design is written against that table
#' and nothing else, so an expression naming anything absent from it is an
#' error rather than a silent fall-through to the calling environment.
#'
#' @keywords internal
#' @noRd
check_vars_in_data <- function(vars, data, what) {
  absent <- setdiff(vars, names(data))
  if (length(absent) == 0L) return(invisible(NULL))
  stop(what, " names ",
       paste0("`", absent, "`", collapse = ", "),
       ", which `data` does not have. When `data` is supplied every variable ",
       "must be a column of it. Columns: ",
       paste0("`", names(data), "`", collapse = ", "), ".",
       call. = FALSE)
}

#' The arguments of [declare_ra()] that carry one value per unit
#'
#' These are the ones \code{data} resolves. \code{permutation_matrix} also has
#' one row per unit and is deliberately absent: it enumerates assignments
#' rather than describing units, so its rows are never columns of \code{data}.
#' \code{prob}, \code{m} and \code{m_each} are absent because they are not
#' per-unit; \code{prob} in particular must be of length 1 whatever the design,
#' though its documentation used to suggest otherwise.
#'
#' @keywords internal
#' @noRd
.unit_length_args <- c("blocks", "clusters", "m_unit", "prob_unit",
                       "prob_unit_each")

#' The scalar counterpart of each per-unit argument of [declare_ra()]
#'
#' Every per-unit slot refuses a single number and points at its scalar
#' counterpart, so one argument never means two things: \code{prob} is the
#' shared-probability slot and \code{prob_unit} the per-unit one, in
#' \code{balanced_ra()} as everywhere else.
#'
#' @keywords internal
#' @noRd
.scalar_slot_for <- c(prob_unit = "prob", m_unit = "m")

#' Resolve `prob`, `prob_unit` and `prob_unit_each` to one probability argument
#'
#' \code{prob} is the scalar slot and \code{prob_unit} the per-unit one, as in
#' every other assignment function. Returns \code{NULL} when
#' \code{prob_unit_each} is the one in use, and otherwise the value to hand
#' \code{balanced_ra_matrix()} as its \code{prob_unit}, which recycles a
#' scalar itself. \code{n} is the unit count if it is already known, and is
#' used only to let a one-unit design supply a length-one \code{prob_unit}.
#'
#' @keywords internal
#' @noRd
balanced_prob_args <- function(prob, prob_unit, prob_unit_each, n = NULL) {
  n_given <- sum(!is.null(prob), !is.null(prob_unit), !is.null(prob_unit_each))
  if (n_given > 1L) {
    stop("Supply only one of `prob`, `prob_unit` and `prob_unit_each`.",
         call. = FALSE)
  }
  if (!is.null(prob_unit_each)) return(NULL)
  if (!is.null(prob_unit)) {
    if (length(prob_unit) == 1L && !identical(as.integer(n), 1L)) {
      stop("`prob_unit` gives one value per unit. For a single value shared ",
           "by every unit, use `prob`.", call. = FALSE)
    }
    return(prob_unit)
  }
  if (is.null(prob)) prob <- 0.5
  if (length(prob) != 1L) {
    stop("`prob` must be a single number. To let it vary across units, use ",
         "`prob_unit`.", call. = FALSE)
  }
  prob
}

#' Resolve one per-unit argument against `data`
#'
#' \code{expr} is the argument as written. A bare column name is the ordinary
#' case; any expression is allowed so long as every variable in it is a column
#' of \code{data}, so \code{interaction(region, year)} and
#' \code{cbind(p_a, p_b)} work and \code{df$bl} does not. A length-one
#' character string naming a column is taken as that column, which is what
#' programmatic callers need.
#'
#' The size check uses \code{NROW}, so a matrix argument such as
#' \code{prob_unit_each} is measured by its rows. A length-one result is
#' passed through rather than rejected: whether a scalar is legal is the
#' design's question (every per-unit slot refuses one and names its scalar
#' counterpart; \code{N = 1} is the exemption), not this function's.
#'
#' @keywords internal
#' @noRd
resolve_from_data <- function(expr, data, arg_name, envir) {
  if (is.null(expr)) return(NULL)
  # Through a wrapper's ..., match.call() renders the argument as ..1, ..2,
  # and so on. substitute(...()) in the frame that owns the dots recovers the
  # expressions the wrapper was handed, so `function(d, ...) conduct_ra(data =
  # d, ...)` still resolves `blocks = bl` as the column bl. Nested wrappers
  # unwrap one frame per pass, up to a small bound.
  for (i in 1:5) {
    if (!(is.symbol(expr) && grepl("^\\.\\.[0-9]+$", as.character(expr)))) break
    idx <- as.integer(sub("^\\.\\.", "", as.character(expr)))
    dots <- tryCatch(eval(quote(substitute(...())), envir),
                     error = function(e) NULL)
    if (is.null(dots) || idx > length(dots)) break
    expr <- dots[[idx]]
  }
  vars <- all.vars(expr)
  check_vars_in_data(vars, data, paste0("`", arg_name, "`"))
  out <- eval(expr, data, envir)
  if (length(vars) == 0L && is.character(out) && length(out) == 1L) {
    check_vars_in_data(out, data, paste0("`", arg_name, "`"))
    out <- data[[out]]
  }
  if (!is.null(out) && NROW(out) != nrow(data) && NROW(out) != 1L) {
    stop("`", arg_name, "` has ", NROW(out),
         if (is.matrix(out)) " rows" else " elements",
         " but `data` has ", nrow(data), " rows.", call. = FALSE)
  }
  out
}

#' Resolve formula data from `data` or the calling environment
#'
#' When \code{data} is omitted, look in \code{environment(formula)} first, as
#' \code{\link[stats]{lm}} does, then in \code{envir} (typically
#' \code{parent.frame()} of [balanced_ra()]) so that a data mask still works,
#' then walk the call stack.
#'
#' @keywords internal
#' @noRd
formula_lookup_data <- function(formula, envir) {
  vars <- all.vars(formula)
  try_env <- function(env) {
    if (is.null(env) || !is.environment(env)) return(NULL)
    ok <- tryCatch({
      stats::model.frame(formula, data = env)
      TRUE
    }, error = function(e) FALSE)
    if (ok) env else NULL
  }

  # The environment the formula was written in comes first, as in stats::lm().
  # Searching the caller or the stack ahead of it lets a same-named object
  # elsewhere, and globalenv() is always on the stack, shadow the covariate the
  # design was written against, with no error to say so.
  found <- try_env(environment(formula))
  if (!is.null(found)) return(found)

  found <- try_env(envir)
  if (!is.null(found)) return(found)

  n <- sys.nframe()
  if (n > 1L) {
    for (i in seq.int(n - 1L, 1L, by = -1L)) {
      found <- try_env(sys.frame(i))
      if (!is.null(found)) return(found)
    }
  }

  stop("Could not find formula variable",
       if (length(vars) == 1L) " " else "s ",
       paste0("`", vars, "`", collapse = ", "),
       " in the calling environment.",
       call. = FALSE)
}

#' Model matrix for cube-on-X
#'
#' @keywords internal
#' @noRd
balanced_formula_matrix <- function(formula, n, envir = parent.frame(),
                                    data = NULL) {
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula, e.g. ~ x + B.")
  }
  if (!is.null(data)) {
    check_vars_in_data(all.vars(formula), data, "`formula`")
  } else if (length(all.vars(formula)) == 0L) {
    data <- data.frame(row.names = seq_len(n))
  } else {
    data <- formula_lookup_data(formula, envir)
  }
  X <- tryCatch(
    # na.pass keeps NA rows in the frame, so an NA covariate reaches
    # balanced_check_matrix() as an NA cell and a clear error, instead of
    # model.matrix dropping the row and the row-count check misfiring.
    stats::model.matrix(formula,
                        data = stats::model.frame(formula, data = data,
                                                  na.action = stats::na.pass)),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("not found", msg, ignore.case = TRUE)) {
        vars <- all.vars(formula)
        stop("Could not find formula variable",
             if (length(vars) == 1L) " " else "s ",
             paste0("`", vars, "`", collapse = ", "),
             " in the calling environment.",
             call. = FALSE)
      }
      stop(msg, call. = FALSE)
    }
  )
  balanced_check_matrix(X, n)
}

#' Validate a balancing matrix
#'
#' Shared by the formula path, which builds \code{X} here, and the declaration
#' path, which built it when the design was declared.
#'
#' @keywords internal
#' @noRd
balanced_check_matrix <- function(X, n) {
  X <- as.matrix(X)
  if (!is.numeric(X)) {
    stop("The balancing matrix must be numeric.")
  }
  if (nrow(X) != n) {
    stop("`formula` produces ", nrow(X), " rows but the probabilities describe ",
         n, " units.")
  }
  if (anyNA(X) || any(!is.finite(X))) {
    stop("The model matrix from `formula` must be finite and non-missing.")
  }
  if (ncol(X) < 1L) {
    stop("`formula` produced no columns. Include at least an intercept or a covariate.")
  }
  storage.mode(X) <- "double"
  X
}

#' Cube-on-X at the cluster-collapsed level
#'
#' Whole clusters move together, so unit-level \(X'Z\) equals the cluster-sum
#' of X times the cluster assignment. Collapse, assign, expand.
#'
#' @keywords internal
#' @noRd
cube_on_x_clusters <- function(p, X, clusters, tol = 1e-12) {
  cl <- factor(clusters)
  first <- match(levels(cl), as.character(cl))
  pc <- p[first]
  Xc <- rowsum(X, cl, reorder = TRUE)
  zc <- cube_on_x_cpp(pc, Xc, tol)
  zc[as.integer(cl)]
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
#' With \code{blocks}, flight stays inside each block. Landing pairs the leftover
#' units across blocks so the overall counts stay tight as well.
#'
#' Two-arm fast path: the pivotal method.
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
  # Memory safety is not check_inputs' to waive: an NA or short blocks vector
  # would index outside the C++ kernels' buffers. The kernels guard too; this
  # is the readable message.
  if (length(b) != n) {
    stop("`blocks` has length ", length(b), " but the probabilities describe ",
         n, " units.", call. = FALSE)
  }
  if (anyNA(b)) stop("`blocks` must not contain NA.", call. = FALSE)
  ord <- sample.int(n)                    # the input order must not matter
  # Two conditions collapse to a single vector, where the walk has a linear-time
  # form. Both paths are in src/cube.cpp.
  if (ncol(P) == 2L) {
    z <- cube_two_arm_cpp(P[, 2L], b, ord, tol)
    return(cbind(1 - z, z))
  }
  cube_multi_cpp(P, b, ord, tol)
}

