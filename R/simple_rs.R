#' Simple Random Sampling
#'
#' \code{simple_rs} draws a sample in which every unit is included or not independently of the others, as a separate coin flip. Because the draws are independent, the size of the realized sample varies from draw to draw. For most applications in which the size of the sampling frame is known in advance, \code{\link{complete_rs}()} is preferable because it fixes the number of units sampled.
#'
#' If \code{prob} is not specified, each unit is sampled with probability 0.5.
#'
#' @seealso \code{\link{complete_rs}()}, \code{\link{strata_rs}()}, \code{\link{simple_ra}()}, \code{\link{simple_rs_probabilities}()}
#'
#' @param N The number of units in the sampling frame. Must be a positive integer. (required)
#' @param prob The probability of being sampled; must be a real number between 0 and 1 inclusive and of length 1. (optional)
#' @param prob_unit The probability of being sampled for each unit; must be a real number between 0 and 1 inclusive and of length N. Because units are drawn independently, this probability may differ from unit to unit. (optional)
#' @param check_inputs Logical. Whether to verify before sampling that the arguments are internally consistent: that probabilities lie between 0 and 1, that vectors are of length N, and that only one of \code{prob} and \code{prob_unit} is supplied. Defaults to \code{TRUE}. Set to \code{FALSE} to skip the checks when drawing many samples from arguments that have already been verified; declaring the design once with \code{\link{declare_rs}()} and drawing from it with \code{\link{draw_rs}()} does this for you. (optional)
#' @param simple Logical. Internal use only; leave at its default. \code{simple_rs} always draws units independently, and this argument exists so that the argument checker knows as much. (optional)
#'
#' @return A numeric vector of length N indicating whether each unit is sampled (1) or not (0).
#' @export
#'
#' @examples
#'
#' S <- simple_rs(N = 100)
#' table(S)
#'
#' S <- simple_rs(N = 100, prob = 0.3)
#' table(S)
#'
#' # A probability of inclusion that varies unit by unit
#' S <- simple_rs(N = 100, prob_unit = seq(0.1, 0.9, length.out = 100))
#' table(S)
#'
simple_rs <- 
  function(N, 
           prob = NULL, 
           prob_unit = NULL, 
           check_inputs = TRUE,
           simple = TRUE) {
  if(check_inputs) .invoke_check(check_samplr_arguments_new)
  if(is.null(prob)) prob <- .5
  simple_ra(N, prob, prob_unit, conditions=0:1, check_inputs = FALSE)    
}

#' Inclusion probabilities: Simple Random Sampling
#'
#' Returns each unit's probability of being sampled under simple random
#' sampling. Every unit is sampled independently, so the probabilities do not
#' depend on which other units were drawn.
#'
#' These are the quantities inverse-probability weights are built from: weight
#' each sampled unit by the reciprocal of its inclusion probability, which
#' \code{\link{obtain_inclusion_probabilities}()} extracts for you.
#'
#' @seealso \code{\link{simple_rs}()}
#'
#' @inheritParams simple_rs
#' @return A numeric vector of length N giving each unit's probability of being included in the sample.
#'
#' @examples
#' probs <- simple_rs_probabilities(N = 100)
#' table(probs)
#'
#' probs <- simple_rs_probabilities(N = 100, prob = 0.3)
#' table(probs)
#'
#' @export
simple_rs_probabilities <- function(N, prob = NULL, prob_unit = NULL, check_inputs = TRUE, simple = TRUE) {
  if(check_inputs) .invoke_check(check_samplr_arguments_new)
  if(is.null(prob)) prob <- .5
  simple_ra_probabilities(N, prob, prob_unit, conditions = 0:1, check_inputs = FALSE, simple = TRUE)[,2]
}
