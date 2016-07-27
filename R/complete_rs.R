#' Complete Random Sampling
#'
#' complete_rs implements a random sampling procedure in which fixed numbers of units are sampled. The canonical example of complete random sampling is a procedure in which exactly m of N units are sampled.\cr \cr
#' Users can set the exact number of units to sample with m. Alternatively, users can specify the probability of being sampled with prob and complete_rs will infer the correct number of units to sample.
#' complete_rs will either sample floor(N*prob) or ceiling(N*prob) units, choosing between these two values to ensure that the overall probability of being sampled is exactly prob.
#' Users should specify N and not more than one of m or prob. \cr \cr
#' If only N is specified, N/2 units will be sampled. If N is odd, either floor(N/2) units or ceiling(N/2) units will be sampled.
#'
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param m Use for a design in which exactly m units are sampled. (optional)
#' @param prob Use for a design in which either floor(N*prob) or ceiling(N*prob) units are sampled. The probability of being sampled is exactly prob because with probability 1-prob, floor(N*prob) units will be sampled and with probability prob, ceiling(N*prob) units will be sampled. prob must be a real number between 0 and 1 inclusive. (optional)
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
#' S <- complete_rs(N = 100, m = 50)
#' table(S)
#'
#' S <- complete_rs(N = 100, prob = .111)
#' table(S)
#'
#' # If N = m, sample with 100% probability...
#' complete_ra(N=2, m=2)
#'
#' # except if N = m = 1, in which case sample with 50% probability
#' complete_ra(N=1, m=1)
#'
#'
complete_rs <- function(N,
                        m = NULL,
                        prob = NULL) {
  # Checks
  check_inputs <-
    check_samplr_arguments(
      N = N,
      m = m,
      prob = prob)
  
  if (N == 1) {
    if (is.null(m) & is.null(prob)) {
      assign <- simple_rs(N, prob = 0.5)
      return(assign)
    }
    if (!is.null(m)) {
      if (!m %in% c(0, 1)) {
        stop(
          "The number of units samples (m) must be less than or equal to the total number of units (N)"
        )
      }
      if (m == 0) {
        assign <- 0
        return(assign)
      }
      if (m == 1) {
        assign <- simple_rs(N, prob = 0.5)
        return(assign)
      }
    }
    if (!is.null(prob)) {
      assign <- simple_rs(N, prob = prob)
    }
  }
  
  if (N > 1) {
    if (is.null(m) & is.null(prob)) {
      m_floor <- floor(N / 2)
      m_ceiling <- ceiling(N / 2)
      
      if(m_ceiling > m_floor){
        prob_fix_up <- ((N*.5) - m_floor) / (m_ceiling - m_floor)
      }else{
        prob_fix_up <- .5
      }
      
      if (simple_rs(1, prob_fix_up) == 0) {
        m <- m_floor
      } else{
        m <- m_ceiling
      }
      assign <-  sample(rep(c(0, 1), c(N - m, m)))
      return(assign)
    }
    if (!is.null(m)) {
      if (m == N) {
        assign <- rep(1, N)
        return(assign)
      }
      assign <- sample(rep(c(0, 1), c(N - m, m)))
      return(assign)
    }
    if (!is.null(prob)) {
      m_floor <- floor(N * prob)
      m_ceiling <- ceiling(N * prob)
      if (m_ceiling == N) {
        m <- m_floor
        assign <- sample(rep(c(0, 1), c(N - m, m)))
        return(assign)
      }
      
      if(m_ceiling > m_floor){
        prob_fix_up <- ((N*prob) - m_floor) / (m_ceiling - m_floor)
      }else{
        prob_fix_up <- .5
      }
      
      if (simple_rs(1, prob_fix_up) == 0) {
        m <- m_floor
      } else{
        m <- m_ceiling
      }
      assign <- sample(rep(c(0, 1), c(N - m, m)))
      return(assign)
    }
  }
}
