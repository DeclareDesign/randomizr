#' Probabilties of assignment: Complete Random Assignment
#'
#' @inheritParams complete_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # 2-arm designs
#' complete_ra_probabilities(N=100)
#' complete_ra_probabilities(N=100, m=50)
#' complete_ra_probabilities(N=100, prob = .3)
#'
#' complete_ra_probabilities(N=100, m_each = c(30, 70),
#'                           condition_names = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' complete_ra_probabilities(N=100, num_arms=3)
#' complete_ra_probabilities(N=100, m_each=c(30, 30, 40))
#'
#' complete_ra_probabilities(N=100, m_each=c(30, 30, 40),
#'                           condition_names=c("control", "placebo", "treatment"))
#'
#' complete_ra_probabilities(N=100, condition_names=c("control", "placebo", "treatment"))
#' complete_ra_probabilities(N=100, prob_each = c(.2, .7, .1))
#'
#' @export
complete_ra_probabilities <- function(N,
                                      m = NULL,
                                      m_each = NULL,
                                      prob = NULL,
                                      prob_each = NULL,
                                      num_arms = NULL,
                                      condition_names = NULL) {
  # Setup: obtain number of arms and condition_names
  
  check_inputs <-
    check_randomizr_arguments(
      N = N,
      m = m,
      m_each = m_each,
      prob = prob,
      prob_each = prob_each,
      num_arms = num_arms,
      condition_names = condition_names
    )
  
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  
  
  if (is.null(m_each) &
      is.null(prob_each) & length(condition_names) == 2) {
    if (N == 1) {
      if (is.null(m) & is.null(prob)) {
        prob_mat <- matrix(
          rep(c(.5, .5), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        if (!m %in% c(0, 1)) {
          stop(
            "The number of units assigned to treatment (m) must be less than or equal to the total number of units (N)"
          )
        }
        if (m == 0) {
          prob_mat <- matrix(
            rep(c(1, 0), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
          return(prob_mat)
        }
        if (m == 1) {
          prob_mat <- matrix(
            rep(c(.5, .5), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
          return(prob_mat)
        }
      }
      if (!is.null(prob)) {
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
    }
    
    if (N > 1) {
      if (is.null(m) & is.null(prob)) {
        prob_mat <- matrix(
          rep(c(.5, .5), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        prob <- m / N
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(prob)) {
        m_floor <- floor(N * prob)
        m_ceiling <- ceiling(N * prob)
        if (m_ceiling == N) {
          m_ceiling <- m_floor
        }
        
        prob_temp <- (m_floor / N) * (1 - prob) + (m_ceiling / N) * (prob)
        
        prob_mat <-
          matrix(
            rep(c(1 - prob_temp, prob_temp), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
        return(prob_mat)
      }
    }
  }
  
  if (is.null(prob_each) & is.null(m_each)) {
    condition_probabilities <- rep(1 / num_arms, num_arms)
  }
  
  if (!is.null(prob_each)) {
    condition_probabilities <- prob_each
  }
  
  if (!is.null(m_each)) {
    condition_probabilities <- m_each / N
  }
  
  # Build prob_mat
  prob_mat <- matrix(
    rep(condition_probabilities, N),
    byrow = TRUE,
    ncol = length(condition_probabilities),
    dimnames = list(NULL,  paste0("prob_", condition_names))
  )
  return(prob_mat)
  
}
