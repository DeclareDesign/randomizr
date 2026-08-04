#' Complete Random Assignment
#'
#' \code{complete_ra} assigns exactly fixed numbers of units to each treatment condition. In the canonical two-arm case, exactly \code{m} of \code{N} units are assigned to treatment and \code{N-m} to control on every draw. This guarantee that the counts are fixed is the defining feature of complete random assignment, and it is what distinguishes it from simple random assignment (where counts vary from draw to draw).
#'
#' Researchers can specify counts directly (via \code{m} or \code{m_each}) or target probabilities (via \code{prob} or \code{prob_each}). When probabilities are specified and the implied counts are not integers, \code{complete_ra} uses stochastic rounding to ensure that the overall probability of assignment exactly equals the target. In a two-arm design, either \code{floor(N*prob)} or \code{ceiling(N*prob)} units are assigned to treatment, with the draw between these two values chosen so that \code{Pr(treatment)} equals exactly \code{prob}. In a multi-arm design, the remaining units after floor allocation are assigned using a single round of simple random assignment calibrated to hit the exact target probabilities.
#'
#' If only \code{N} is specified, a balanced two-arm trial (\code{prob = 0.5}) is assumed. When \code{N} is odd, either \code{floor(N/2)} or \code{ceiling(N/2)} units are assigned to treatment.
#'
#' @param N The number of units. Must be a positive integer. (required)
#' @param m Use for a two-arm design: exactly \code{m} units are assigned to treatment and \code{N-m} to control. (optional)
#' @param m_unit Use for a two-arm design. \code{unique(m_unit)} units are assigned to treatment; must be the same for all units and of length N. (optional)
#' @param m_each Use for a multi-arm design. A numeric vector giving the exact number of units assigned to each condition; must sum to N. (optional)
#' @param prob Use for a two-arm design: either \code{floor(N*prob)} or \code{ceiling(N*prob)} units are assigned to treatment so that the marginal probability of assignment equals exactly \code{prob}. Must be between 0 and 1. (optional)
#' @param prob_unit Use for a two-arm design. \code{unique(prob_unit)} will be passed to the \code{prob} argument; must be the same for all units. (optional)
#' @param prob_each Use for a multi-arm design. A numeric vector giving the probability of assignment to each condition; entries must be nonnegative and sum to 1. Due to integer rounding the exact count assigned to each condition may differ slightly from draw to draw, but the overall probability of assignment is exactly \code{prob_each}. (optional)
#' @param num_arms The number of treatment arms. If unspecified, determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, groups will be named 0 and 1 in a two-arm trial and T1, T2, T3, in a multi-arm trial. A two-group design in which \code{num_arms} is set to 2 will use condition names T1 and T2. (optional)
#' @param check_inputs Logical. Defaults to \code{TRUE}.
#'
#' @return A vector of length N indicating the treatment condition of each unit. Numeric in a two-arm trial; a factor (ordered by \code{conditions}) in a multi-arm trial.
#' @export
#'
#' @examples
#' # Two-arm Designs
#' Z <- complete_ra(N = 100)
#' table(Z)
#'
#' Z <- complete_ra(N = 100, m = 50)
#' table(Z)
#' 
#' Z <- complete_ra(N = 100, m_unit = rep(50, 100))
#' table(Z)
#'
#' Z <- complete_ra(N = 100, prob = .111)
#' table(Z)
#' 
#' Z <- complete_ra(N = 100, prob_unit = rep(0.1, 100))
#' table(Z)
#'
#' Z <- complete_ra(N = 100, conditions = c("control", "treatment"))
#' table(Z)
#'
#'
#' # Multi-arm Designs
#' Z <- complete_ra(N = 100, num_arms = 3)
#' table(Z)
#'
#' Z <- complete_ra(N = 100, m_each = c(30, 30, 40))
#' table(Z)
#'
#' Z <- complete_ra(N = 100, prob_each = c(.1, .2, .7))
#' table(Z)
#'
#' Z <- complete_ra(N = 100, conditions = c("control", "placebo", "treatment"))
#' table(Z)
#'
#' # Special Cases
#' # Two-arm trial where the conditions are by default "T1" and "T2"
#' Z <- complete_ra(N = 100, num_arms = 2)
#' table(Z)
#'
#' # If N = m, assign with 100% probability
#' complete_ra(N=2, m=2)
#'
#' # Up through randomizr 0.12.0, 
#' complete_ra(N=1, m=1) # assigned with 50% probability
#' # This behavior has been deprecated
#'
complete_ra <- function(N,
                        m = NULL,
                        m_unit = NULL,
                        m_each = NULL,
                        prob = NULL,
                        prob_unit = NULL,
                        prob_each = NULL,
                        num_arms = NULL,
                        conditions = NULL,
                        check_inputs = TRUE) {
  if (check_inputs) .invoke_check(check_randomizr_arguments_new)
  
    
    
  # Annoying that these checks aren't in the general error checker, but I don't know how to distinguish between simple and complete in there! - AC
  if (!is.null(prob_unit)) {
    unique_prob_unit <- unique(prob_unit)
    if (length(unique_prob_unit) > 1) {
      stop("In a complete random assignment design, `prob_unit` must be the same for all units")
    }
    prob <- unique(prob_unit)
  }
  
  if(!is.null(m_unit)) {
    unique_m_unit <- unique(m_unit)
    if (length(unique_m_unit) > 1) {
      stop("In a complete random assignment design, `m_unit` must be the same for all units")
    }
    m <- unique(m_unit)
    }
  
  # Simple 2 group design, returns zeros and ones
  if (is.null(m_each) &&
      is.null(prob_each) && length(conditions) == 2) {

    # Two-arm Design Case 1: Neither m nor prob is specified
    if (is.null(m) & is.null(prob)) {
      m_floor <- floor(N / 2)
      m_ceiling <- ceiling(N / 2)
      
      if (m_ceiling > m_floor) {
        prob_fix_up <- ((N * .5) - m_floor) / (m_ceiling - m_floor)
      } else{
        prob_fix_up <- .5
      }
      
      # randomizr 1.x drew this through simple_ra(1, prob_fix_up), whose
      # vsample() walk takes the second condition when the uniform exceeds
      # 1 - prob_fix_up. `runif(1) < prob_fix_up` has the same distribution but
      # the opposite decision on the same draw, which silently changed every
      # odd-N assignment. Keep 1.x's comparison.
      m <- if (runif(1L) > 1 - prob_fix_up) m_ceiling else m_floor
      
      assignment <-  sample(rep(conditions, c(N - m, m)))
      assignment <-
        clean_condition_names(assignment, conditions)
      return(assignment)
    }
    
    # Two-arm Design Case 2: m is specified
    if (!is.null(m)) {
      if (m == N) {
        assignment <- rep(1, N)
        assignment <-
          clean_condition_names(assignment, conditions)
        return(assignment)
      }
      assignment <- sample(rep(conditions, c(N - m, m)))
      assignment <-
        clean_condition_names(assignment, conditions)
      return(assignment)
    }
    
    # Two-arm Design Case 3: prob is specified
    if (!is.null(prob)) {
      m_floor <- floor(N * prob)
      m_ceiling <- ceiling(N * prob)
      if (m_ceiling == N) {
        m <- m_floor
        assignment <- sample(rep(conditions, c(N - m, m)))
        assignment <-
          clean_condition_names(assignment, conditions)
        return(assignment)
      }
      
      if (m_ceiling > m_floor) {
        prob_fix_up <- ((N * prob) - m_floor) / (m_ceiling - m_floor)
      } else{
        prob_fix_up <- .5
      }
      
      m <- sample(c(m_ceiling, m_floor), 1,
                  prob = c(prob_fix_up, 1 - prob_fix_up))
      
      assignment <- sample(rep(conditions, c(N - m, m)))
      assignment <-
        clean_condition_names(assignment, conditions)
      return(assignment)
    }
  }

  # Multi-arm Designs
  
  # Multi-arm Design Case 1: neither prob_each nor m_each specified
  if (is.null(prob_each) & is.null(m_each)) {
    prob_each <- rep(1 / num_arms, num_arms)
    assignment <-
      complete_ra(
        N = N,
        prob_each = prob_each,
        conditions = conditions,
        check_inputs = check_inputs
      )
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Multi-arm Design Case 2: prob_each is specified
  if (!is.null(prob_each)) {
    m_each_floor <- floor(N * prob_each)
    N_floor <- sum(m_each_floor)
    N_remainder <- N - N_floor
    
    if (N_remainder > 0) {
      prob_each_fix_up <- ((prob_each * N) - m_each_floor) / N_remainder
      conditions_vec <-
        unlist(list(
          rep(conditions, m_each_floor),
          sample(
            conditions,
            N_remainder,
            prob = prob_each_fix_up,
            replace = FALSE
          )
        ))
    } else{
      conditions_vec <- rep(conditions, m_each_floor)
    }
    assignment <- sample(conditions_vec, length(conditions_vec))
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
  
  # Multi-arm Design Case 3: m_each specified
  if (!is.null(m_each)) {
    conditions_vec <- rep(conditions, m_each)
    assignment <- sample(conditions_vec, length(conditions_vec))
    assignment <- clean_condition_names(assignment, conditions)
    return(assignment)
  }
}


#' probabilities of assignment: Complete Random Assignment
#'
#' @inheritParams complete_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # 2-arm designs
#' prob_mat <- complete_ra_probabilities(N=100)
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, m=50)
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, prob = .3)
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, m_each = c(30, 70),
#'                           conditions = c("control", "treatment"))
#' head(prob_mat)
#'
#' # Multi-arm Designs
#' prob_mat <- complete_ra_probabilities(N=100, num_arms=3)
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, m_each=c(30, 30, 40))
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, m_each=c(30, 30, 40),
#'                           conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, conditions=c("control", "placebo", "treatment"))
#' head(prob_mat)
#'
#' prob_mat <- complete_ra_probabilities(N=100, prob_each = c(.2, .7, .1))
#' head(prob_mat)
#'
#' @export
complete_ra_probabilities <- function(N,
                                      m = NULL,
                                      m_unit = NULL,
                                      m_each = NULL,
                                      prob = NULL,
                                      prob_unit = NULL,
                                      prob_each = NULL,
                                      num_arms = NULL,
                                      conditions = NULL,
                                      check_inputs = TRUE) {
  # Setup: obtain number of arms and conditions
  if (check_inputs) .invoke_check(check_randomizr_arguments_new)
  
  if (!is.null(prob_unit)) {
    unique_prob_unit <- unique(prob_unit)
    if (length(unique_prob_unit) > 1) {
      stop("In a complete random assignment design, `prob_unit` must be the same for all units")
    }
    prob <- unique(prob_unit)
  }
  
  if(!is.null(m_unit)) {
    unique_m_unit <- unique(m_unit)
    if (length(unique_m_unit) > 1) {
      stop("In a complete random assignment design, `m_unit` must be the same for all units")
    }
    m <- unique(m_unit)
  }
  
  if (is.null(m_each) &
      is.null(prob_each) & length(conditions) == 2) {
    if (N == 1) {
      if (is.null(m) & is.null(prob)) {
        prob_mat <- matrix(
          rep(c(.5, .5), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", conditions))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        if (m == 0) {
          prob_mat <- matrix(
            rep(c(1, 0), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", conditions))
          )
          return(prob_mat)
        }
        if (m == 1) {
          prob_mat <- matrix(
            # rep(c(.5, .5), N),
            rep(c(0, 1), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", conditions))
          )
          return(prob_mat)
        }
      }
      if (!is.null(prob)) {
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", conditions))
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
          dimnames = list(NULL,  paste0("prob_", conditions))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        prob <- m / N
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", conditions))
        )
        return(prob_mat)
      }
      if (!is.null(prob)) {
        m_floor <- floor(N * prob)
        m_ceiling <- ceiling(N * prob)
        if (m_ceiling == N) {
          m <- m_floor
          
          prob_mat <-
            matrix(
              rep(c(1 - (m / N), (m / N)), N),
              byrow = TRUE,
              ncol = 2,
              dimnames = list(NULL,  paste0("prob_", conditions))
            )
          return(prob_mat)
        }
        
        prob_mat <-
          matrix(
            rep(c(1 - prob, prob), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", conditions))
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
    dimnames = list(NULL,  paste0("prob_", conditions))
  )
  return(prob_mat)
  
}
