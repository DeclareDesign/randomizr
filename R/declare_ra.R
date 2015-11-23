#' Declare a random assignment procedure.
#'
#' @param N The total number of units in the experimental sample.
#' @param prob If specified, a two-group design is assumed. prob is the probability of assignment to treatment.
#' @param num_arms The total number of treatment arms. If unspecified, num_arms will be determined from the length of m_each or condition_names. 
#' @param prob_each A numeric giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each. 
#' @param m If specified, a two-group design is assumed. m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units are assigned to treatment. If not specified, half of the sample (N/2) will be assigned to treatment (if N is odd, m will be set to either floor(N/2) or ceiling(N/2) with equal probability. m is NULL by default.
#' @param m_each A numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed. 
#' @param block_var A vector of length N indicating which block each unit belongs to.
#' @param block_m A matrix of arm sizes whose number of rows is equal to the number of blocks and whose number of columns is equal to the number of treatment arms. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var). The columns should be in the order of condition_names, if specified. 
#' @param prob_each A vector whose length is equal to the number of treatment conditions. When specified, prob_each assigns the same (within rounding) proportion of each block to each treatment condition, using complete random assignment. prob_each must sum to 1. 
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to. 
#' @param simple A logical indicating if simple random assignment is intended. Is FALSE by default.
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be names T1, T2, T3, etc. 
#'
#' @return A random assignment declaration. 
#' @export
declare_ra <- function(N = NULL, prob = NULL, num_arms = NULL, prob_each = NULL, 
                       m = NULL, m_each = NULL,
                       block_var = NULL, block_m = NULL,
                       clust_var = NULL, 
                       condition_names = NULL, simple = FALSE){
  
  # Global checks
  
  if(simple == FALSE){
    ra_type <- "complete"
  }else{
    ra_type <- "simple"
  }
  
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
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' when using simple random assignment.")}
    
    ra_function <- function(){
      simple_ra(N = N, prob = prob, prob_each = prob_each, 
                num_arms = num_arms, condition_names = condition_names)
    }
    probabilities_matrix <- simple_ra_probabilities(N = N, prob = prob, prob_each = prob_each, 
                                                    num_arms = num_arms, condition_names = condition_names)
  }
  
  if(ra_type=="complete"){
    if(!is.null(block_var)){stop("You can't specify 'block_var' when using complete random assignment.")}
    if(!is.null(block_m)){stop("You can't specify 'block_m' when using complete random assignment.")}
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' when using complete random assignment.")}
    
    ra_function <- function(){
      complete_ra(N = N, m = m, prob = prob, num_arms = num_arms, 
                  m_each = m_each, prob_each = prob_each, 
                  condition_names = condition_names)
    }
    
    probabilities_matrix <- complete_ra_probabilities(N = N, m = m, prob = prob, num_arms = num_arms, 
                                                      m_each = m_each, prob_each = prob_each, 
                                                      condition_names = condition_names)
    
  }
  
  if(ra_type=="blocked"){
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' when using block random assignment.")}
    
    ra_function <- function(){
      block_ra(block_var = block_var, num_arms = num_arms, 
               block_m = block_m, prob_each = prob_each, 
               condition_names = condition_names)
    }
    
    probabilities_matrix <- block_ra_probabilities(block_var = block_var, num_arms = num_arms, 
                                                   block_m = block_m, prob_each = prob_each, 
                                                   condition_names = condition_names)
  }
  
  
  if(ra_type=="clustered"){
    if(!is.null(block_var)){stop("You can't specify 'block_var' when using cluster random assignment.")}
    if(!is.null(block_m)){stop("You can't specify 'block_m' when using cluster random assignment.")}
    
    ra_function <- function(){
      cluster_ra(clust_var = clust_var, m = m, num_arms = num_arms, 
                 m_each = m_each, prob_each = prob_each, 
                 condition_names = condition_names)
    }
    
    probabilities_matrix <- cluster_ra_probabilities(clust_var = clust_var, m = m, num_arms = num_arms, 
                                                     m_each = m_each, prob_each = prob_each, 
                                                     condition_names = condition_names)
    
  }
  
  if(ra_type=="blocked_and_clustered"){
    
    ra_function <- function(){
      block_and_cluster_ra(clust_var = clust_var, block_var = block_var, 
                           num_arms = num_arms, block_m = block_m, 
                           prob_each = prob_each, condition_names = condition_names)
    }
    
    probabilities_matrix <- block_and_cluster_ra_probabilities(clust_var = clust_var, block_var = block_var, 
                                                               num_arms = num_arms, block_m = block_m, 
                                                               prob_each = prob_each, condition_names = condition_names)
    
  }
  
  
  return_object <- list(ra_function = ra_function,
                        ra_type = ra_type,
                        probabilities_matrix = probabilities_matrix)
  
  class(return_object) <- "ra_declaration"
  return(return_object)
  
}

#' @export
conduct_ra <- function(ra_declaration){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  return(ra_declaration$ra_function())
}

#' @export
obtain_condition_probabilities <- function(ra_declaration, assignment){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  
  probabilities_matrix <- ra_declaration$probabilities_matrix
  cond_Z <- paste0("prob_", Z)
  indicies <- sapply(colnames(probabilities_matrix), FUN= x <- function(cond_name, cond_Z){cond_Z == cond_name}, cond_Z=cond_Z)
  cond_probs <- as.vector(t(probabilities_matrix))[as.vector(t(indicies))]
  return(cond_probs)
}







