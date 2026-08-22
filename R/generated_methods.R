# S3 dispatch for ra_function, ra_probabilities, rs_function, rs_probabilities
# generics. Each method unpacks only the formals its target function accepts,
# using mget() so that delayedAssign slots (probabilities_matrix, etc.) are
# never triggered as a side effect.

.mget_formals <- function(fn, envir)
  mget(names(formals(fn)), envir, ifnotfound = list(NULL))

#' @export
ra_function.ra_blocked <- function(this) {
  args <- .mget_formals(block_ra, this)
  # Supply declaration-cached encoding so block_ra skips as.factor + tabulate.
  args[[".block_int"]]   <- this[[".block_int"]]
  args[[".N_per_block"]] <- this[["N_per_block"]]
  do.call(block_ra, args)
}

#' @export
ra_function.ra_blocked_and_clustered <- function(this)
  do.call(block_and_cluster_ra, .mget_formals(block_and_cluster_ra, this))

#' @export
ra_function.ra_clustered <- function(this)
  do.call(cluster_ra, .mget_formals(cluster_ra, this))

#' @export
ra_function.ra_complete <- function(this)
  do.call(complete_ra, .mget_formals(complete_ra, this))

#' @export
ra_function.ra_custom <- function(this)
  custom_ra(permutation_matrix = this[["permutation_matrix"]])

#' @export
ra_function.ra_simple <- function(this)
  do.call(simple_ra, .mget_formals(simple_ra, this))

#' @export
ra_function.ra_balanced <- function(this) {
  args <- .mget_formals(balanced_ra, this)
  # balanced_ra uses missing(prob_unit) to allow a matrix; do not pass NULL.
  if (!is.null(args$prob_unit_each)) args$prob_unit <- NULL
  do.call(balanced_ra, Filter(Negate(is.null), args))
}

#' @export
ra_probabilities.ra_blocked <- function(this)
  do.call(block_ra_probabilities, .mget_formals(block_ra_probabilities, this))

#' @export
ra_probabilities.ra_blocked_and_clustered <- function(this)
  do.call(block_and_cluster_ra_probabilities,
          .mget_formals(block_and_cluster_ra_probabilities, this))

#' @export
ra_probabilities.ra_clustered <- function(this)
  do.call(cluster_ra_probabilities, .mget_formals(cluster_ra_probabilities, this))

#' @export
ra_probabilities.ra_complete <- function(this)
  do.call(complete_ra_probabilities, .mget_formals(complete_ra_probabilities, this))

#' @export
ra_probabilities.ra_custom <- function(this)
  custom_ra_probabilities(permutation_matrix = this[["permutation_matrix"]])

#' @export
ra_probabilities.ra_simple <- function(this)
  do.call(simple_ra_probabilities, .mget_formals(simple_ra_probabilities, this))

#' @export
ra_probabilities.ra_balanced <- function(this) {
  args <- .mget_formals(balanced_ra_probabilities, this)
  if (!is.null(args$prob_unit_each)) args$prob_unit <- NULL
  do.call(balanced_ra_probabilities, Filter(Negate(is.null), args))
}

#' @export
rs_function.rs_clustered <- function(this)
  do.call(cluster_rs, .mget_formals(cluster_rs, this))

#' @export
rs_function.rs_complete <- function(this)
  do.call(complete_rs, .mget_formals(complete_rs, this))

#' @export
rs_function.rs_simple <- function(this)
  do.call(simple_rs, .mget_formals(simple_rs, this))

#' @export
rs_function.rs_stratified <- function(this)
  do.call(strata_rs, .mget_formals(strata_rs, this))

#' @export
rs_function.rs_stratified_and_clustered <- function(this)
  do.call(strata_and_cluster_rs, .mget_formals(strata_and_cluster_rs, this))

#' @export
rs_probabilities.rs_clustered <- function(this)
  do.call(cluster_rs_probabilities, .mget_formals(cluster_rs_probabilities, this))

#' @export
rs_probabilities.rs_complete <- function(this)
  do.call(complete_rs_probabilities, .mget_formals(complete_rs_probabilities, this))

#' @export
rs_probabilities.rs_simple <- function(this)
  do.call(simple_rs_probabilities, .mget_formals(simple_rs_probabilities, this))

#' @export
rs_probabilities.rs_stratified <- function(this)
  do.call(strata_rs_probabilities, .mget_formals(strata_rs_probabilities, this))

#' @export
rs_probabilities.rs_stratified_and_clustered <- function(this)
  do.call(strata_and_cluster_rs_probabilities,
          .mget_formals(strata_and_cluster_rs_probabilities, this))
