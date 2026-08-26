#' Complete Random Sampling
#'
#' \code{complete_rs} draws a sample of a fixed size: exactly \code{n} of \code{N} units are sampled on every draw. Fixing the sample size is what distinguishes it from simple random sampling, where the realized size varies.
#'
#' Set the number of units to sample directly with \code{n}, or give a target probability with \code{prob} and let \code{complete_rs} work out the number. When \code{N*prob} is not a whole number, either \code{floor(N*prob)} or \code{ceiling(N*prob)} units are sampled: the ceiling is drawn with probability equal to the fractional part of \code{N*prob} and the floor otherwise, which makes each unit's probability of inclusion exactly \code{prob}. Specify \code{N} and not more than one of \code{n} or \code{prob}.
#'
#' If only \code{N} is specified, half the units are sampled. When \code{N} is odd, either \code{floor(N/2)} or \code{ceiling(N/2)} units are sampled.
#'
#'
#' @seealso \code{\link{simple_rs}()}, \code{\link{strata_rs}()}, \code{\link{cluster_rs}()}, \code{\link{complete_ra}()}, \code{\link{complete_rs_probabilities}()}
#'
#' @param N The number of units in the sampling frame. Must be a positive integer. (required)
#' @param n Use for a design in which exactly \code{n} units are sampled. (optional)
#' @param n_unit \code{unique(n_unit)} will be passed to \code{n}; must be the same for all units and of length N. (optional)
#' @param prob Use for a design in which either \code{floor(N*prob)} or \code{ceiling(N*prob)} units are sampled, chosen so that each unit's probability of inclusion is exactly \code{prob}. Must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_unit \code{unique(prob_unit)} will be passed to \code{prob}; must be the same for all units and of length N. Under complete random sampling the probability cannot vary by unit; use \code{\link{simple_rs}()} if it must. (optional)
#' @param check_inputs Logical. Whether to verify before sampling that the arguments are internally consistent: that \code{n} does not exceed N, that probabilities lie between 0 and 1, that vectors are of length N, and so on. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many samples from arguments that have already been verified; declaring the design once with \code{\link{declare_rs}()} and drawing from it with \code{\link{draw_rs}()} does this for you. (optional)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0).
#' @export
#'
#' @examples
#' S <- complete_rs(N = 100)
#' table(S)
#'
#' S <- complete_rs(N = 100, n = 50)
#' table(S)
#' 
#' S <- complete_rs(N = 100, n_unit = rep(30, 100))
#' table(S)
#'
#' S <- complete_rs(N = 100, prob = 0.111)
#' table(S)
#' 
#' S <- complete_rs(N = 100, prob_unit = rep(0.1, 100))
#' table(S)
#' 
#' # If N = n, every unit is sampled with probability 1
#' complete_rs(N = 2, n = 2)
#'
#' # The single-unit case works the same way: n = 1 out of N = 1 is sampled
#' # with probability 1. Up through randomizr 0.12.0 this case was instead
#' # treated as a coin flip, so the unit was sampled only half of the time.
#' # The change is noted here because it silently alters the inclusion
#' # probabilities in code written against those versions.
#' complete_rs(N = 1, n = 1)
#'
#'
complete_rs <- function(N,
                        n = NULL,
                        n_unit = NULL,
                        prob = NULL,
                        prob_unit = NULL,
                        check_inputs = TRUE) {
  # Checks
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  if (!is.null(prob_unit)) {
    unique_prob_unit <- unique(prob_unit)
    if (length(unique_prob_unit) > 1) {
      stop("In a complete random sampling design, `prob_unit` must be the same for all units")
    }
    prob <- unique(prob_unit)
  }
  
  if(!is.null(n_unit)) {
    unique_n_unit <- unique(n_unit)
    if (length(unique_n_unit) > 1) {
      stop("In a complete random sampling design, `n_unit` must be the same for all units")
    }
    n <- unique(n_unit)
  }
  
  if (is.null(n)) {
    
    if (is.null(prob)) {
      prob <- .5
    } 
    
    Np <- N*prob
    n_dn <- floor(Np)
    n_up <- ceiling(Np)
    
    # If rounding doesn't matter or rounds up to 100% use n_dn except when N=1
    n <- if (n_up == n_dn || (N > 1 && n_up == N)) {
      n_dn
    } else  {
      n_dn + sample(0:1, 1, prob = abs(1:0 - (Np - n_dn)))
    }
  }
  
  assignment <- sample(rep(c(0, 1), c(N - n, n)))
  return(assignment)

    
}

#' Inclusion probabilities: Complete Random Sampling
#'
#' Returns each unit's probability of being sampled under complete random
#' sampling, where the sample size is fixed on every draw.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each sampled unit by the reciprocal of its inclusion probability, which
#' \code{\link{obtain_inclusion_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{complete_rs}()}
#'
#' @inheritParams complete_rs
#' @return A numeric vector of length N giving each unit's probability of being included in the sample.
#'
#' @examples
#' probs <- complete_rs_probabilities(N = 100)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N = 100, n = 50)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N = 100, prob = 0.3)
#' table(probs)
#'
#' @export
complete_rs_probabilities <- function(N,
                                      n = NULL,
                                      n_unit = NULL,
                                      prob = NULL,
                                      prob_unit = NULL,
                                      check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  if (!is.null(prob_unit)) {
    unique_prob_unit <- unique(prob_unit)
    if (length(unique_prob_unit) > 1) {
      stop("In a complete random sampling design, `prob_unit` must be the same for all units")
    }
    prob <- unique(prob_unit)
  }
  
  if(!is.null(n_unit)) {
    unique_n_unit <- unique(n_unit)
    if (length(unique_n_unit) > 1) {
      stop("In a complete random sampling design, `n_unit` must be the same for all units")
    }
    n <- unique(n_unit)
  }
  
  prob_vec <-  if (is.numeric(n))  {
    #n / max(N,2) # 0,1=> 0, 1,1 => 1/2
    n / N
  } else if (is.numeric(prob)) {
    ifelse(N > 1 && ceiling(N * prob) == N,
           floor(N * prob) / N, prob)
  } else {
    .5
  }
  
  rep_len(prob_vec, N)
}
