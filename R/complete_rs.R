#' Complete Random Sampling
#'
#' complete_rs implements a random sampling procedure in which fixed numbers of units are sampled. The canonical example of complete random sampling is a procedure in which exactly n of N units are sampled.\cr \cr
#' Users can set the exact number of units to sample with n. Alternatively, users can specify the probability of being sampled with prob and complete_rs will infer the correct number of units to sample.
#' complete_rs will either sample floor(N*prob) or ceiling(N*prob) units, choosing between these two values to ensure that the overall probability of being sampled is exactly prob.
#' Users should specify N and not more than one of n or prob. \cr \cr
#' If only N is specified, N/2 units will be sampled. If N is odd, either floor(N/2) units or ceiling(N/2) units will be sampled.
#'
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param n Use for a design in which exactly n units are sampled. (optional)
#' @param prob Use for a design in which either floor(N*prob) or ceiling(N*prob) units are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N*prob) units will be sampled and with probability prob, ceiling(N*prob) units will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#'
#' @importFrom stats rbinom
#'
#' @examples
#' S <- complete_rs(N = 100)
#' table(S)
#'
#' S <- complete_rs(N = 100, n = 50)
#' table(S)
#'
#' S <- complete_rs(N = 100, prob = .111)
#' table(S)
#'
#' # If N = n, sample with 100% probability...
#' complete_ra(N=2, n=2)
#'
#' # except if N = n = 1, in which case sample with 50% probability
#' complete_ra(N=1, n=1)
#'
#'
complete_rs <- function(N,
                        n = NULL,
                        prob = NULL,
                        check_inputs = TRUE) {
  # Checks
  if (check_inputs) {
    check_inputs <-
      check_samplr_arguments(N = N,
                             n = n,
                             prob = prob)
  }
  
  if (N == 1) {
    if (is.null(n) & is.null(prob)) {
      assignment <- simple_rs(N, prob = 0.5)
      return(assignment)
    }
    if (!is.null(n)) {
      if (!n %in% c(0, 1)) {
        stop(
          "The number of units sampled (n) must be less than or equal to the total number of units (N)"
        )
      }
      if (n == 0) {
        assignment <- 0
        return(assignment)
      }
      if (n == 1) {
        assignment <- simple_rs(N, prob = 0.5)
        return(assignment)
      }
    }
    if (!is.null(prob)) {
      assignment <- simple_rs(N, prob = prob)
    }
  }
  
  if (N > 1) {
    if (is.null(n) & is.null(prob)) {
      n_floor <- floor(N / 2)
      n_ceiling <- ceiling(N / 2)
      
      if (n_ceiling > n_floor) {
        prob_fix_up <- ((N * .5) - n_floor) / (n_ceiling - n_floor)
      } else{
        prob_fix_up <- .5
      }
      
      if (simple_rs(1, prob_fix_up) == 0) {
        n <- n_floor
      } else{
        n <- n_ceiling
      }
      assignment <-  sample(rep(c(0, 1), c(N - n, n)))
      return(assignment)
    }
    if (!is.null(n)) {
      if (n == N) {
        assignment <- rep(1, N)
        return(assignment)
      }
      assignment <- sample(rep(c(0, 1), c(N - n, n)))
      return(assignment)
    }
    if (!is.null(prob)) {
      n_floor <- floor(N * prob)
      n_ceiling <- ceiling(N * prob)
      if (n_ceiling == N) {
        n <- n_floor
        assignment <- sample(rep(c(0, 1), c(N - n, n)))
        return(assignment)
      }
      
      if (n_ceiling > n_floor) {
        prob_fix_up <- ((N * prob) - n_floor) / (n_ceiling - n_floor)
      } else{
        prob_fix_up <- .5
      }
      
      if (simple_rs(1, prob_fix_up) == 0) {
        n <- n_floor
      } else{
        n <- n_ceiling
      }
      assignment <- sample(rep(c(0, 1), c(N - n, n)))
      return(assignment)
    }
  }
}
