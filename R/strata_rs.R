#' Stratified Random Sampling
#'
#' strata_rs implements a random sampling procedure in which units that are grouped into strata defined by covariates are sample using complete random sampling within stratum For example, imagine that 50 of 100 men are sampled and 75 of 200 women are sampled.
#'
#' @param strata_var A vector of length N that indicates which stratum each unit belongs to. Can be a character, factor, or numeric vector. (required)
#' @param prob Use for a design in which either floor(N_stratum*prob) or ceiling(N_stratum*prob) units are assigned to treatment within each stratum. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N_stratum*prob) units will be assigned to treatment and with probability prob, ceiling(N_stratum*prob) units will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param strata_m Use for a in which strata_m describes the number of units to assign to treatment within each stratum.
#' @param strata_prob Use for a in which strata_prob describes the probability of assignment to treatment within each stratum. Differs from prob in that the probability of assignment can vary across strata.
#' @param balance_load logical, defaults to FALSE. This feature is experimental. Please use with caution and perform many tests before using in a real research scenario.
#'
#' @return A numeric vector of length N that indicates if a unit is sampled (1) or not (0).
#' @export
#'
#' @importFrom stats r2dtable
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
#' Z <- strata_rs(strata_var = strata_var, strata_m = c(20, 30, 40))
#' table(strata_var, Z)
#'
#' # Experimental feature: load balancing
#' # This procedure constrains the total number of units that are sampled
#' # This will never exceed 5 sampled units total.
#'
#' strata_var <- rep(c("A", "B","C"), times=c(3, 3, 3))
#' Z <- strata_rs(strata_var = strata_var, balance_load = TRUE)
#' table(strata_var, Z)
#'
#' # compare to strata_rs without load balancing
#' # Sometimes this procedure assigns 6 total units to treatment
#' Z <- strata_rs(strata_var = strata_var, balance_load = FALSE)
#' table(strata_var, Z)
#'
strata_rs <- function(strata_var,
                      prob = NULL,
                      strata_m = NULL,
                      strata_prob = NULL,
                      balance_load = FALSE) {
  check_inputs <- check_samplr_arguments(
    strata_var = strata_var,
    prob = prob,
    strata_m = strata_m,
    strata_prob = strata_prob
  )
  
  strata <- sort(unique(strata_var))
  assign <- rep(NA, length(strata_var))
  
  # Setup: obtain number of arms and condition_names
  N_per_stratum <- check_inputs$N_per_stratum
  
  if (is.null(prob) & is.null(strata_m) & is.null(strata_prob)) {
    prob <- 0.5
  }
  
  # Case 1: prob is specified
  if (!is.null(prob)) {
    for (i in 1:length(strata)) {
      assign[strata_var == strata[i]] <-
        complete_rs(N = N_per_stratum[i], prob = prob)
    }
    return(assign)
  }
  
  # Case 2: strata_m is specified
  if (!is.null(strata_m)) {
    for (i in 1:length(strata)) {
      assign[strata_var == strata[i]] <-
        complete_rs(N = N_per_stratum[i], m = strata_m[i])
    }
    return(assign)
  }
  
  # Case 3: strata_prob is specified
  if (!is.null(strata_prob)) {
    for (i in 1:length(strata)) {
      assign[strata_var == strata[i]] <-
        complete_rs(N = N_per_stratum[i], prob = strata_prob[i])
    }
    return(assign)
  }
}
