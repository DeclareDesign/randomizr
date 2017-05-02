#' Stratified Random Sampling
#'
#' strata_rs implements a random sampling procedure in which units that are grouped into strata defined by covariates are sample using complete random sampling within stratum For example, imagine that 50 of 100 men are sampled and 75 of 200 women are sampled.
#'
#' @param strata_var A vector of length N that indicates which stratum each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a design in which either floor(N_stratum*prob) or ceiling(N_stratum*prob) units are assigned to treatment within each stratum. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_stratum*prob) units will be assigned to treatment and with probability prob, ceiling(N_stratum*prob) units will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_n Use for a in which strata_n describes the number of units to assign to treatment within each stratum.
#' @param strata_prob Use for a in which strata_prob describes the probability of assignment to treatment within each stratum. Differs from prob in that the probability of assignment can vary across strata.
#' @param check_inputs logical. Defaults to TRUE.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#'
#' @examples
#'
#' strata_var <- rep(c("A", "B","C"), times = c(50, 100, 200))
#' Z <- strata_rs(strata_var = strata_var)
#' table(strata_var, Z)
#'
#' Z <- strata_rs(strata_var = strata_var, prob = .3)
#' table(strata_var, Z)
#'
#' Z <- strata_rs(strata_var = strata_var, strata_prob = c(.1, .2, .3))
#' table(strata_var, Z)
#'
#' Z <- strata_rs(strata_var = strata_var, strata_n = c(20, 30, 40))
#' table(strata_var, Z)
#'
#'
strata_rs <- function(strata_var,
                      prob = NULL,
                      strata_n = NULL,
                      strata_prob = NULL,
                      check_inputs = TRUE) {
  if (check_inputs) {
    check_inputs <- check_samplr_arguments(
      strata_var = strata_var,
      prob = prob,
      strata_n = strata_n,
      strata_prob = strata_prob
    )
  }
  
  strata_spots <-
    unlist(split(1:length(strata_var), strata_var), FALSE, FALSE)
  
  # Setup: obtain number of arms and condition_names
  N_per_stratum <- check_inputs$N_per_stratum
  
  if (is.null(prob) & is.null(strata_n) & is.null(strata_prob)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        MoreArgs = list(prob = prob,
                        check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <- unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
  
  # Case 2: strata_n is specified
  if (!is.null(strata_n)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        n = strata_n,
        MoreArgs = list(check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <- unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
  
  # Case 3: strata_prob is specified
  if (!is.null(strata_prob)) {
    assign_list <-
      mapply(
        FUN = complete_rs,
        N = N_per_stratum,
        prob = strata_prob,
        MoreArgs = list(check_inputs = FALSE),
        SIMPLIFY = FALSE
      )
    
    assignment <- unlist(assign_list, FALSE, FALSE)[order(strata_spots)]
    return(assignment)
  }
}
