#' Complete Random Assignment
#'
#' Random assignment where a fixed number (within rounding) of units is assigned to treatment conditions. The canonical example of complete random assignment is a procedure in which exactly m of N units is assigned to treatment.
#' 
#' @param N The total number of units in the experimental sample (required).
#' @param m If specified, a two-group design is assumed. m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units are assigned to treatment. If not specified (and no other arguments are specified), half of the sample (N/2) will be assigned to treatment (if N is odd, m will be set to either floor(N/2) or ceiling(N/2) with equal probability). m is NULL by default.
#' @param prob If specified, a two-group design is assumed. prob is the probability of assignment to treatment. Within rounding, N*prob subjects will be assigned to treatment.
#' @param num_arms The total number of treatment arms. If unspecified, num_arms will be determined from the length of m_each, prob_each, or condition_names.
#' @param m_each A numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed.
#' @param prob_each A numeric vector giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each. 
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named T1, T2, T3, etc. An execption is a two-group design in which N only or N and m are specified, in which the condition names default to 0 and 1.
#' @return A vector of length N that indicates the treatment condition of each unit.
#' @export
#' 
#' @importFrom stats rbinom
#' 
#' @examples
#' # Two Group Designs
#'
#' Z <- complete_ra(N=100)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m=50)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each = c(30, 70), 
#'                  condition_names = c("control", "treatment"))
#' table(Z)
#' 
#' # Multi-arm Designs
#' Z <- complete_ra(N=100, num_arms=3)
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each=c(30, 30, 40))
#' table(Z)
#' 
#' Z <- complete_ra(N=100, m_each=c(30, 30, 40), 
#'                  condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
#' Z <- complete_ra(N=100, condition_names=c("control", "placebo", "treatment"))
#' table(Z)
#' 
complete_ra <- function(N, m = NULL, prob = NULL, num_arms = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  
  # Checks
  
  N_check <- length(N) == 1 & (all.equal(N, as.integer(N))) & N > 0
  
  if(!N_check){
    stop("N must be an integer greater than 0")
  }
  
  if(!is.null(num_arms)){
    if(num_arms == 1){
      stop("The number of arms (specified with num_arms) must be greater than one.")
    }
  }
  if(!is.null(condition_names) & length(condition_names) ==1){
    stop("The number of arms (as inferred from the length of condition_names) must be greater than one.")
  }
  if(!is.null(m_each) & length(m_each) ==1){
    stop("The number of arms (as inferred from the length of m_each) must be greater than one.")
  }
  if(!is.null(prob_each) & length(prob_each) ==1){
    stop("The number of arms (as inferred from the length of prob_each) must be greater than one.")
  }
  if(!is.null(m) & !is.null(condition_names)){
    stop("Do not specify m and condition_names together. Use m_each and condition_names instead.")
  }
  if(!is.null(prob_each) & !is.null(m_each)){
    stop("Do not specify prob_each and m_each together. Use one or the other.")
  }
  
  # Simple 2 group design, returns zeros and ones
  
  if(is.null(m_each) & is.null(condition_names) & is.null(num_arms) & is.null(prob_each)){
    
    if(N == 1){
      if(is.null(m) & is.null(prob)){
        assign <- simple_ra(N, prob = 0.5)
        return(assign)
      }
      if(!is.null(m)){
        if(!m %in% c(0,1)){
          stop("The number of units assigned to treatment (m) must be less than or equal to the total number of units (N)")
        }
        if(m == 0){
          assign <- 0
          return(0)
        }
        if(m == 1){
          assign <- simple_ra(N, prob = 0.5)
          return(assign)
        }
      }
      if(!is.null(prob)){
        assign <- simple_ra(N, prob = prob)
      }
    }
    
    if(N > 1){
      
      if(is.null(m) & is.null(prob)){
        
        m_low <- floor(N/2)
        m_high <- ceiling(N/2)
        
        if(simple_ra(1, .5) == 0){
          m <- m_low
        }else{
          m <- m_high
        }
        assign <-  sample(rep(c(0,1), c(N-m, m)))
        return(assign)
      }
      if(!is.null(m)){
        if(m == N){
          assign <- rep(1, N)
          return(assign)
        }
        if(m > N){
          stop("The number of units assigned to treatment (m) must be smaller than the total number of units (N)")
        }
        if(m < 0){
          stop("The number of units assigned to treatment (m) must be nonnegative.")
        }
        assign <- sample(rep(c(0,1), c(N-m, m)))
        return(assign)
      }
      if(!is.null(prob)){
        m_low <- floor(N*prob)
        m_high <- ceiling(N*prob)
        if(m_high == N){m_high <- m_low}
        if(simple_ra(1, .5) == 0){
          m <- m_low
        }else{
          m <- m_high
        }
        assign <- sample(rep(c(0,1), c(N-m, m)))
        return(assign)
      }
    }
  }
  # All other types
  
  if(all(!is.null(m_each), sum(m_each) != N)){
    stop("The sum of the number assigned to each condition (m_each) must equal the total number of units (N)")
  }
  if(all(!is.null(condition_names), !is.null(m_each), length(m_each) != length(condition_names))){
    stop("The length of conditions_names must equal the length of m_each")
  }
  if(all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != length(condition_names))){
    stop("The length of conditions_names must equal the length of prob_each")
  }
  if(all(!is.null(m_each), !is.null(num_arms),length(m_each) != num_arms)){
    stop("The number of arms (n_arms) must equal the length of m_each")
  }
  if(all(!is.null(prob_each), !is.null(num_arms),length(prob_each) != num_arms)){
    stop("The number of arms (n_arms) must equal the length of prob_each")
  }
  if(all(!is.null(prob_each), sum(prob_each)!=1)){
    stop("If specified, the sum of prob_each must equal 1")
  }
  if(all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != num_arms)){
    stop("The length of condition_names must equal the number of arms (num_arms)")
  }
  
  
  # Obtain num_arms
  if(is.null(num_arms)){
    if(!is.null(m_each)){num_arms <- length(m_each)}
    if(!is.null(prob_each)){num_arms <- length(prob_each)}
    if(!is.null(condition_names)){num_arms <- length(condition_names)}
  }
  
  # Obtain condition_names
  if(is.null(condition_names)){
    condition_names <- paste0("T", 1:num_arms)
  }
  
  
  if(is.null(prob_each) & is.null(m_each)){
    if(N < num_arms){
      # Case 1: no prob_each and no m_each, and N < num_arms and is a multi-arm design
      assign <- sample(condition_names, N)
      return(assign)
    }else{
      # Case 2:  no prob_each and no m_each, and N >= num_arms and is a multi-arm design
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <-  N%%num_arms
      m_each <- m_each + sample(rep(c(0,1), c(num_arms-remainder, remainder)))
      assign <- sample(rep(condition_names, m_each))
      assign <- clean_condition_names(assign, condition_names)
      return(assign)
    }
  }
  
  # Case 3: prob_each is specified
  
  if(!is.null(prob_each)){
    if(sum(prob_each)!=1){
      
    }
    m_each <- floor(N*prob_each)
    remainder <- N - sum(m_each)
    m_each <- m_each + complete_ra(N=length(prob_each), m= remainder)
    assign <- sample(rep(condition_names, m_each))
    assign <- clean_condition_names(assign, condition_names)
    return(assign)
  } 
  
  # Case 4: m_each specified
  if(!is.null(m_each)){
    assign <- sample(rep(condition_names, m_each))
    assign <- clean_condition_names(assign, condition_names)
    return(assign)
  }
}



clean_condition_names <- function(assign, condition_names){
  if(all(assign %in% c(0,1))){
    return(as.numeric(assign))
  }else{
    #assign <- condition_names[assign]
    #if(!identical(condition_names, c(0,1))){
    #  assign <- factor(assign, levels = condition_names)
    #}
    
      return(factor(assign, levels = condition_names))
    }
}

