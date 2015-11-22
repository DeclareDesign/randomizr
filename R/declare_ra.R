#' Declare a random assignment procedure.
#'
#' @param N The total number of units in the experimental sample.
#' @param prob If specified, a two-group design is assumed. prob is the probability of assignment to treatment.
#' @param num_arms 
#' @param prob_each 
#' @param m 
#' @param m_each 
#' @param block_var 
#' @param block_m 
#' @param prob_each 
#' @param clust_var 
#' @param ra_type 
#' @param condition_names 
#'
#' @return A random assignment declaration. 
#' @export
#'
#' @examples
declare_ra <- function(N = NULL, prob = NULL, num_arms = NULL, prob_each = NULL, 
                       m = NULL, m_each = NULL,
                       block_var = NULL, block_m = NULL,
                       clust_var = NULL, 
                       condition_names = NULL, ra_type = "complete"){
  
  # Global checks
  
  
  # Determine ra_type
  if(!is.null(block_var) & is.null(clust_var)){
    ra_type <- "blocked"
  }
  if(is.null(block_var) & !is.null(clust_var)){
    ra_type <- "clustered"
  }
  if(!is.null(block_var) & !is.null(clust_var)){
   ra_type <- "blocked_and_clustered"
 }  
  
  if(ra_type=="simple"){
    if(!is.null(m)){stop("You can't specify 'm' when using simple random assignment.")}
    if(!is.null(m_each)){stop("You can't specify 'm_each' when using simple random assignment.")}
    if(!is.null(block_var)){stop("You can't specify 'block_var' when using simple random assignment.")}
    if(!is.null(block_m)){stop("You can't specify 'block_m' when using simple random assignment.")}
    if(!is.null(prob_each)){stop("You can't specify 'prob_each' when using simple random assignment.")}
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' when using simple random assignment.")}
    
    ra_function <- simple_ra(N = N, prob = prob, prob_each = prob_each, 
                             num_arms = num_arms, condition_names = condition_names)
    
    if(is.null(prob_each) & is.null(condition_names) & is.null(num_arms)){
      if(is.null(prob)){
        prob <- 0.5
      }
      condition_names <- c(0, 1)
      probabilities_matrix <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, 
                                      dimnames = list(NULL,  paste0("prob_",condition_names)))
    }
    
    if(is.null(prob_each)){
      if(is.null(num_arms)){
        num_arms <- length(condition_names)
      }
      prob_each <- rep(1/num_arms, num_arms)
    }
    
    if(is.null(num_arms)){
      num_arms <- length(prob_each)
    }
    
    if(is.null(condition_names)){
      condition_names <- paste0("T", 1:num_arms)
    }    
    
    probabilities_matrix <- matrix(rep(prob_each, N), byrow=TRUE, ncol=length(prob_each), 
                                   dimnames = list(NULL, paste0("prob_",condition_names)))
  }
  
  
  
  if(design=="complete"){
    if(!is.null(prob)){stop("You can't specify 'prob' in a complete design")}
    if(!is.null(block_var)){stop("You can't specify 'block_var' in a complete design")}
    if(!is.null(block_m)){stop("You can't specify 'block_m' in a complete design")}
    if(!is.null(prob_each)){stop("You can't specify 'prob_each' in a complete design")}
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' in a complete design")}
    
    
    Z <- complete_ra(N = N, m = m, num_arms = num_arms, m_each = m_each, prob_each = prob_each, condition_names = condition_names)
    if(is.null(m_each) & is.null(condition_names) & is.null(num_arms) & is.null(prob_each)){
      m_floor <- m
      m_ceiling <- m
      
      if(is.null(m)){
        m_floor <- floor(N/2)
        m_ceiling <- ceiling(N/2)
      }
      
      prob <- 0.5*(m_floor/N) + 0.5*(m_ceiling/N)
      condition_names <- c(0, 1)
      prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
      return(prob_mat)
    }
    
    if(!is.null(m_each) & is.null(prob_each)){
      if(is.null(num_arms)){
        num_arms <- length(m_each)
      }
      remainder <-  N%%num_arms
      condition_probs <- (m_each/N)
    }
    
    if(is.null(m_each) & is.null(prob_each)){
      if(is.null(num_arms)){
        num_arms <- length(condition_names)
      }
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <-  N%%num_arms
      condition_probs <- 
        (1-(remainder/num_arms))* (m_each/N) +
        (remainder/num_arms)* ((m_each +1)/N)
    }
    
    
    
    if(!is.null(prob_each)){
      m_each <- floor(N*prob_each)
      remainder <- N - sum(m_each)
      condition_probs <- 
        (1-(remainder/length(prob_each)))* (m_each/N) +
        (remainder/length(prob_each))* ((m_each +1)/N)
    } 
    
    
    if(is.null(num_arms)){
      num_arms <- length(m_each)
    }
    
    if(is.null(condition_names)){
      condition_names <- paste0("T", 1:num_arms)
    }
    
    if(N < num_arms){
      condition_probs <- rep(N/num_arms, num_arms)
    }
    
    prob_mat <- matrix(rep(condition_probs, N), byrow=TRUE, ncol=length(condition_probs), dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
    
  }
  
  
  return_object <- list(ra_function = ra_function,
                        ra_type = ra_type,
                        probabilities_matrix = probabilities_matrix)
  
  class(return_object) <- "ra_declaration"
  return(return_object)
  
}

conduct_ra <- function(ra_declaration){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  return(ra_declaration$ra_function())
}

obtain_ra_probabilities <- function(ra_declaration, assignment){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  
  
  return()
}







