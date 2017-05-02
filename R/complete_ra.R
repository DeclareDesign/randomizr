#' Complete Random Assignment
#'
#' complete_ra implements a random assignment procedure in which fixed numbers of units are assigned to treatment conditions. The canonical example of complete random assignment is a procedure in which exactly m of N units are assigned to treatment and N-m units are assigned to control.\cr \cr
#' Users can set the exact number of units to assign to each condition with m or m_each. Alternatively, users can specify probabilities of assignment with prob or prob_each and complete_ra will infer the correct number of units to assign to each condition.
#' In a two-arm design, complete_ra will either assign floor(N*prob) or ceiling(N*prob) units to treatment, choosing between these two values to ensure that the overall probability of assignment is exactly prob.
#' In a multi-arm design, complete_ra will first assign floor(N*prob_each) units to their respective conditions, then will assign the remaining units using simple random assignment, choosing these second-stage probabilties so that the overall probabilities of assignment are exactly prob_each.\cr \cr
#' In most cases, users should specify N and not more than one of m, m_each, prob, prob_each, or num_arms. \cr \cr
#' If only N is specified, a two-arm trial in which N/2 units are assigned to treatment is assumed. If N is odd, either floor(N/2) units or ceiling(N/2) units will be assigned to treatment.
#'
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param m Use for a two-arm design in which m units are assigned to treatment and N-m units are assigned to control. (optional)
#' @param m_each Use for a multi-arm design in which the values of m_each determine the number of units assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)
#' @param prob Use for a two-arm design in which either floor(N*prob) or ceiling(N*prob) units are assigned to treatment. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N*prob) units will be assigned to treatment and with probability prob, ceiling(N*prob) units will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilties of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of units assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An execption is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A vector of length N that indicates the treatment condition of each unit. Is numeric in a two-arm trial and a factor variable (ordered by condition_names) in a multi-arm trial.
#' @export
#'
#' @importFrom stats rbinom
#'
#' @examples
#' # Two-arm Designs
#' Z <- complete_ra(N = 100)
#' table(Z)
#'
#' Z <- complete_ra(N = 100, m = 50)
#' table(Z)
#'
#' Z <- complete_ra(N = 100, prob = .111)
#' table(Z)
#'
#' Z <- complete_ra(N = 100, condition_names = c("control", "treatment"))
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
#' Z <- complete_ra(N = 100, condition_names = c("control", "placebo", "treatment"))
#' table(Z)
#'
#' # Special Cases
#' # Two-arm trial where the condition_names are by default "T1" and "T2"
#' Z <- complete_ra(N = 100, num_arms = 2)
#' table(Z)
#'
#' # If N = m, assign with 100% probability...
#' complete_ra(N=2, m=2)
#'
#' # except if N = m = 1, in which case assign with 50% probability
#' complete_ra(N=1, m=1)
#'
#'
complete_ra <- function(N,
                        m = NULL,
                        m_each = NULL,
                        prob = NULL,
                        prob_each = NULL,
                        num_arms = NULL,
                        condition_names = NULL,
                        check_inputs = TRUE) {
  if (check_inputs) {
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
  }
  
  # Simple 2 group design, returns zeros and ones
  if (is.null(m_each) &
      is.null(prob_each) & length(condition_names) == 2) {
    if (N == 1) {
      if (is.null(m) & is.null(prob)) {
        assignment <-
          simple_ra(N, prob = 0.5, condition_names = condition_names)
        assignment <- clean_condition_names(assignment, condition_names)
        return(assignment)
      }
      if (!is.null(m)) {
        if (!m %in% c(0, 1)) {
          stop(
            "The number of units assigned to treatment (m) must be less than or equal to the total number of units (N)"
          )
        }
        if (m == 0) {
          assignment <- condition_names[1]
          assignment <- clean_condition_names(assignment, condition_names)
          return(assignment)
        }
        if (m == 1) {
          assignment <-
            simple_ra(N, prob = 0.5, condition_names = condition_names)
          assignment <- clean_condition_names(assignment, condition_names)
          return(assignment)
        }
      }
      if (!is.null(prob)) {
        assignment <-
          simple_ra(N, prob = prob, condition_names = condition_names)
        assignment <- clean_condition_names(assignment, condition_names)
        return(assignment)
      }
    }
    
    if (N > 1) {
      if (is.null(m) & is.null(prob)) {
        m_floor <- floor(N / 2)
        m_ceiling <- ceiling(N / 2)
        
        if (m_ceiling > m_floor) {
          prob_fix_up <- ((N * .5) - m_floor) / (m_ceiling - m_floor)
        } else{
          prob_fix_up <- .5
        }
        
        if (simple_ra(1, prob_fix_up) == 0) {
          m <- m_floor
        } else{
          m <- m_ceiling
        }
        assignment <-  sample(rep(condition_names, c(N - m, m)))
        assignment <- clean_condition_names(assignment, condition_names)
        return(assignment)
      }
      if (!is.null(m)) {
        if (m == N) {
          assignment <- rep(1, N)
          assignment <- clean_condition_names(assignment, condition_names)
          return(assignment)
        }
        assignment <- sample(rep(condition_names, c(N - m, m)))
        assignment <- clean_condition_names(assignment, condition_names)
        return(assignment)
      }
      if (!is.null(prob)) {
        m_floor <- floor(N * prob)
        m_ceiling <- ceiling(N * prob)
        if (m_ceiling == N) {
          m <- m_floor
          assignment <- sample(rep(condition_names, c(N - m, m)))
          assignment <- clean_condition_names(assignment, condition_names)
          return(assignment)
        }
        
        if (m_ceiling > m_floor) {
          prob_fix_up <- ((N * prob) - m_floor) / (m_ceiling - m_floor)
        } else{
          prob_fix_up <- .5
        }
        
        if (simple_ra(1, prob_fix_up) == 0) {
          m <- m_floor
        } else{
          m <- m_ceiling
        }
        assignment <- sample(rep(condition_names, c(N - m, m)))
        assignment <- clean_condition_names(assignment, condition_names)
        return(assignment)
      }
    }
  }
  # All other types
  
  
  # Case 1: neither prob_each nor m_each specified
  if (is.null(prob_each) & is.null(m_each)) {
    prob_each <- rep(1 / num_arms, num_arms)
    assignment <-
      complete_ra(N = N,
                  prob_each = prob_each,
                  condition_names = condition_names)
    assignment <- clean_condition_names(assignment, condition_names)
    return(assignment)
  }
  
  # Case 2: prob_each is specified
  if (!is.null(prob_each)) {
    m_each_floor <- floor(N * prob_each)
    N_floor <- sum(m_each_floor)
    N_remainder <- N - N_floor
    
    if (N_remainder > 0) {
      prob_each_fix_up <- ((prob_each * N) - m_each_floor) / N_remainder
      conditions_vec <-
        c(
          rep(condition_names, m_each_floor),
          sample(
            condition_names,
            N_remainder,
            prob = prob_each_fix_up,
            replace = TRUE
          )
        )
    } else{
      conditions_vec <- rep(condition_names, m_each_floor)
    }
    
    assignment <- sample(conditions_vec)
    assignment <- clean_condition_names(assignment, condition_names)
    return(assignment)
  }
  
  # Case 4: m_each specified
  if (!is.null(m_each)) {
    assignment <- sample(rep(condition_names, m_each))
    assignment <- clean_condition_names(assignment, condition_names)
    return(assignment)
  }
}
