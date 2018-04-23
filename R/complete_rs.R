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
#' complete_rs(N=2, n=2)
#'
#' # Up through randomizr 0.12.0, 
#' # This behavior has been deprecated
#' complete_rs(N=1, n=1) # sampled with 50% probability
#'
#'
complete_rs <- function(N,
                        n = NULL,
                        prob = NULL,
                        check_inputs = TRUE) {
  # Checks
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  if (is.null(n)) {
    
    if (is.null(prob)) {
      prob <- .5
    } 
    
    Np <- N*prob
    n_dn <- floor(Np)
    n_up <- ceiling(Np)
    
    # If rounding doesn't matter or rounds up to 100% use n_dn, (except when N=1)
    n <- if (n_up == n_dn || (N > 1 && n_up == N)) n_dn 
         else n_dn + sample(0:1, 1, prob = abs(1:0 - (Np - n_dn)))
         
  }
  
  assignment <- sample(rep(c(0, 1), c(N - n, n)))
  return(assignment)

    
}

#' Inclusion Probabilities: Complete Random Sampling
#'
#' @inheritParams complete_rs
#' @return A vector length N indicating the probability of being sampled.
#'
#' @examples
#' probs <- complete_rs_probabilities(N = 100)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N = 100, n = 50)
#' table(probs)
#'
#' probs <- complete_rs_probabilities(N=100, prob = .3)
#' table(probs)
#'
#' @export
complete_rs_probabilities <- function(N,
                                      n = NULL,
                                      prob = NULL,
                                      check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_samplr_arguments_new)
  
  prob_vec <-  if (is.numeric(n))  
                 #n / max(N,2) # 0,1=> 0, 1,1 => 1/2
                  n / N
               else if (is.numeric(prob)) 
                 ifelse(N > 1 && ceiling(N * prob) == N,  floor(N * prob) / N, prob)
               else .5

  rep_len(prob_vec, N)
}
