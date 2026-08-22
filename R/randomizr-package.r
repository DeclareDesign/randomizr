# Why `@importFrom Rcpp evalCpp` is here, and must stay:
#
# The 2.0 rewrite introduced this package's first C++ (`src/block_assign.cpp`).
# Rcpp resolves its own C entry points, `enterRNGScope` among them, only after
# its namespace has been loaded, and `Imports: Rcpp` in DESCRIPTION does not
# load it. Without the directive below, every call into the new code dies on
# `function 'enterRNGScope' not provided by package 'Rcpp'`, which R CMD check
# reports as errors in examples, tests and vignettes alike. Routine registration
# is done by hand in `src/onload.c`, so `useDynLib` needs no
# `.registration = TRUE`.
#
# This is a note to maintainers and deliberately NOT roxygen: written with #'
# it became the \details section of ?randomizr, which is the first help page a
# user of the package sees.

#' randomizr: Easy-to-Use Tools for Common Forms of Random Assignment and Sampling
#'
#' randomizr generates random assignments for common experimental designs and
#' random samples for common sampling designs. The functions are named for the
#' procedure they implement, and each has a `_probabilities` companion that
#' returns the probability of each unit falling into each condition, which is
#' what inverse-probability weights are built from.
#'
#' @section Random assignment:
#' \itemize{
#'   \item [simple_ra()] assigns each unit independently, so the number treated
#'     varies from draw to draw.
#'   \item [complete_ra()] fixes the number treated on every draw.
#'   \item [block_ra()] conducts complete assignment separately within blocks of
#'     similar units, which increases precision.
#'   \item [cluster_ra()] assigns whole groups together, for interventions that
#'     cannot be delivered to individuals.
#'   \item [block_and_cluster_ra()] does both at once.
#'   \item [balanced_ra()] honors unit-varying probabilities and holds
#'     condition counts at the floor or ceiling of their targets. Pass
#'     \code{formula} to add cube-on-X covariate totals.
#'   \item [declare_ra()] describes a design once so it can be reused by
#'     [conduct_ra()] to draw assignments and by
#'     [obtain_condition_probabilities()] to recover the probabilities.
#'     Balanced assignment is opt-in: \code{ra_type = "balanced"},
#'     \code{prob_unit_each}, or \code{formula}.
#' }
#'
#' @section Random sampling:
#' The sampling functions mirror the assignment ones: [simple_rs()],
#' [complete_rs()], [strata_rs()], [cluster_rs()] and
#' [strata_and_cluster_rs()], with [declare_rs()], [draw_rs()] and
#' [obtain_inclusion_probabilities()] playing the roles that
#' [declare_ra()], [conduct_ra()] and [obtain_condition_probabilities()] play
#' for assignment.
#'
#' @section Randomization inference:
#' [obtain_permutation_matrix()] enumerates or samples the assignments a design
#' could have produced, and [obtain_num_permutations()] counts them.
#'
#' @examples
#' # Complete random assignment: exactly 50 of 100 units treated, every draw.
#' Z <- complete_ra(N = 100, m = 50)
#' table(Z)
#'
#' # Blocking on a covariate usually buys precision.
#' blocks <- rep(c("small", "large"), times = c(60, 40))
#' Z <- block_ra(blocks = blocks)
#' table(blocks, Z)
#'
#' # Declare once, then draw and recover probabilities from the same object.
#' declaration <- declare_ra(N = 100, m = 50)
#' Z <- conduct_ra(declaration)
#' probs <- obtain_condition_probabilities(declaration, Z)
#' table(probs)
#'
#' @references
#' Blair, G., Cooper, J., Coppock, A. and Humphreys, M. (2019). Declaring and
#' Diagnosing Research Designs. \emph{American Political Science Review} 113(3),
#' 838-859. \doi{10.1017/S0003055419000194}
#'
#' Gerber, A. S. and Green, D. P. (2012). \emph{Field Experiments: Design,
#' Analysis, and Interpretation}. New York: W. W. Norton.
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stats runif
"_PACKAGE"
