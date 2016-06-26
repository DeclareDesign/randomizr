#' Declare a random assignment procedure.
#'
#' @param N The total number of units in the experimental sample.
#' @param prob The probability of assignment to treatment. If specified, a two-group design is assumed.
#' @param m If specified, a two-group design is assumed. m is the total number of units to be assigned to treatment. Should only be specified for a two group design in which exactly m of N units are assigned to treatment. If not specified (and no other arguments are specified), half of the sample (N/2) will be assigned to treatment (if N is odd, m will be set to either floor(N/2) or ceiling(N/2) with equal probability). m is NULL by default.
#' @param prob_each A numeric vector giving the probability of assignment to each treatment arm. Must sum to 1. Please note that due to rounding, these probabilities are approximate. For finer control, please use m_each or block_m_each.
#' @param m_each A numeric vector giving the size of each treatment group. Must sum to N. If unspecified, equally sized (rounded) groups will be assumed. 
#' @param block_var A vector of length N indicating which block each unit belongs to.
#' @param block_m Deprecated. Use block_m_each instead.
#' @param block_m_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm. The rows should respect the alphabetical ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_prob_each A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within block. Use only if the probabilities of assignment should vary by block. Each row of block_prob_each must sum to 1. As with prob_each, these probabilities are approximate due to rounding.
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to. 
#' @param simple A logical indicating if simple random assignment is intended. Is FALSE by default.
#' @param num_arms The total number of treatment arms. If unspecified, will be determined from the number of columns of block_m_each or block_prob_each, the length of prob_each, the length of m_each, or the length of condition_names.
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be names T1, T2, T3, etc, except in the binary case in which the assignment variable will be numeric with values 0 and 1.
#'
#' @return A random assignment declaration
#' 
#' @examples 
#' # The declare_ra function is used in three ways:
#' 
#' # 1. To obtain some basic facts about a randomization:
#' declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
#' declaration
#' 
#' # 2. To conduct a random assignment:
#' 
#' Z <- conduct_ra(declaration)
#' table(Z)
#' 
#' # 3. To obtain observed condition probabilities
#' 
#' probs <- obtain_condition_probabilities(declaration, Z)
#' table(probs, Z)
#' 
#' # Simple Random Assignment Declarations
#' 
#' declare_ra(N=100, simple = TRUE)
#' declare_ra(N=100, prob = .4, simple = TRUE)
#' declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4), 
#'            condition_names=c("control", "placebo", "treatment"), simple=TRUE)
#' 
#' # Complete Random Assignment Declarations
#'  
#' declare_ra(N=100)  
#' declare_ra(N=100, m_each = c(30, 70), 
#'            condition_names = c("control", "treatment"))
#' declare_ra(N=100, m_each=c(30, 30, 40))
#'   
#'   
#' # Block Random Assignment Declarations 
#' 
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#  declare_ra(block_var=block_var)
#' 
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declare_ra(block_var=block_var, block_m_each=block_m_each)
#' 
#' 
#' # Cluster Random Assignment Declarations
#' 
#' clust_var <- rep(letters, times=1:26)
#' declare_ra(clust_var=clust_var)
#' declare_ra(clust_var=clust_var, m_each=c(7, 7, 12))
#' 
#' # Blocked and Clustered Random Assignment Declarations 
#' 
#' clust_var <- rep(letters, times=1:26)
#' block_var <- rep(NA, length(clust_var))
#' block_var[clust_var %in% letters[1:5]] <- "block_1"
#' block_var[clust_var %in% letters[6:10]] <- "block_2"
#' block_var[clust_var %in% letters[11:15]] <- "block_3"
#' block_var[clust_var %in% letters[16:20]] <- "block_4"
#' block_var[clust_var %in% letters[21:26]] <- "block_5"
#' 
#' table(block_var, clust_var)
#' 
#' declare_ra(clust_var = clust_var, block_var = block_var)
#' declare_ra(clust_var = clust_var, block_var = block_var, prob_each = c(.2, .5, .3))
#'   
#' @export
declare_ra <- function(N = NULL, 
                       m = NULL, 
                       m_each = NULL,
                       prob_each = NULL, 
                       block_var = NULL, 
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob_each = NULL,
                       clust_var = NULL, 
                       prob = NULL, 
                       simple = FALSE,
                       num_arms = NULL, 
                       condition_names = NULL){
  # Determine ra_type
  if(simple == FALSE){
    ra_type <- "complete"
  }else{
    ra_type <- "simple"
  }
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
    if(!is.null(block_m_each)){stop("You can't specify 'block_m_each' when using simple random assignment.")}
    if(!is.null(clust_var)){stop("You can't specify 'clust_var' when using simple random assignment.")}
    
    ra_function <- function(){
      simple_ra(N = N, prob = prob, prob_each = prob_each, 
                num_arms = num_arms, condition_names = condition_names)
    }
    probabilities_matrix <- simple_ra_probabilities(N = N, prob = prob, prob_each = prob_each, 
                                                    num_arms = num_arms, condition_names = condition_names)
  }
  
  if(ra_type=="complete"){
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
    ra_function <- function(){
      block_ra(block_var = block_var, num_arms = num_arms, 
               block_m = block_m,
               block_m_each = block_m_each, 
               prob = prob,
               prob_each = prob_each, 
               block_prob_each = block_prob_each,
               condition_names = condition_names)
    }
    
    probabilities_matrix <- block_ra_probabilities(block_var = block_var, num_arms = num_arms, 
                                                   block_m = block_m,
                                                   block_m_each = block_m_each, 
                                                   prob = prob,
                                                   prob_each = prob_each, 
                                                   block_prob_each = block_prob_each,
                                                   condition_names = condition_names)
  }
  
  
  if(ra_type=="clustered"){
    ra_function <- function(){
      cluster_ra(clust_var = clust_var, m = m, num_arms = num_arms, 
                 prob = prob,
                 m_each = m_each, prob_each = prob_each, 
                 condition_names = condition_names)
    }
    
    probabilities_matrix <- cluster_ra_probabilities(clust_var = clust_var, m = m, num_arms = num_arms, 
                                                     prob = prob,
                                                     m_each = m_each, prob_each = prob_each, 
                                                     condition_names = condition_names)
    
  }
  
  if(ra_type=="blocked_and_clustered"){
    
    ra_function <- function(){
      block_and_cluster_ra(clust_var = clust_var, block_var = block_var, 
                           num_arms = num_arms, block_m = block_m, block_m_each = block_m_each, 
                           prob = prob,
                           block_prob_each = block_prob_each,
                           prob_each = prob_each, condition_names = condition_names)
    }
    
    probabilities_matrix <- block_and_cluster_ra_probabilities(clust_var = clust_var, block_var = block_var, 
                                                               num_arms = num_arms, block_m = block_m, 
                                                               block_m_each = block_m_each,
                                                               prob = prob,
                                                               block_prob_each = block_prob_each,
                                                               prob_each = prob_each, condition_names = condition_names)
    
  }
  
  # Test
  ra_function()
  
  
  return_object <- list(ra_function = ra_function,
                        ra_type = ra_type,
                        probabilities_matrix = probabilities_matrix,
                        block_var = block_var,
                        clust_var = clust_var)
  
  class(return_object) <- "ra_declaration"
  return(return_object)
  
}

#' Conduct a declared random assignment.
#' 
#' @param ra_declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' 
#' @examples
#' declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
#' Z <- conduct_ra(ra_declaration = declaration)
#' table(Z)
#'
#' @export
conduct_ra <- function(ra_declaration){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  return(ra_declaration$ra_function())
}

#' Obtain the probabilities of units being in the conditions that they are in.
#' 
#' This function is especially useful when units have different probabilties of assignment and the analyst plans to use inverse-probability weights.
#' 
#' @param ra_declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @param assignment A vector of random assignments, often created by \code{\link{conduct_ra}}.
#' 
#' @examples
#' 
#' # Conduct a block random assignment
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declaration <- declare_ra(block_var = block_var, block_m_each = block_m_each)
#' Z <- conduct_ra(ra_declaration = declaration)
#' table(Z, block_var)
#' 
#' observed_probabilities <- 
#'    obtain_condition_probabilities(ra_declaration = declaration, assignment = Z)
#' 
#' # Probabilities in the control group:
#' table(observed_probabilities[Z == 0], block_var[Z == 0])
#' 
#' # Probabilities in the treatment group:
#' table(observed_probabilities[Z == 1], block_var[Z == 1])
#' 
#' @export
obtain_condition_probabilities <- function(ra_declaration, assignment){
  # checks
  if(class(ra_declaration) != "ra_declaration"){
    stop("You must provide a random assignment declaration created by declare_ra().")
  }
  
  probabilities_matrix <- ra_declaration$probabilities_matrix
  cond_Z <- paste0("prob_", assignment)
  indicies <- sapply(colnames(probabilities_matrix), FUN= x <- function(cond_name, cond_Z){cond_Z == cond_name}, cond_Z=cond_Z)
  cond_probs <- as.vector(t(probabilities_matrix))[as.vector(t(indicies))]
  return(cond_probs)
}

#' @export
print.ra_declaration <- function(x, ...){
  Z <- x$ra_function()
  n <- length(Z)
  
  condition_names <- sort(unique(Z))
  num_arms <- length(condition_names)
  constant_probabilities <- all(apply(x$probabilities_matrix, 2, FUN = function(x) {all(x[1] == x)}))
  
  cat("\n")
  if(x$ra_type == "blocked") cat("Random assignment procedure: Block random assignment", "\n")
  if(x$ra_type == "clustered") cat("Random assignment procedure: Cluster random assignment", "\n")
  if(x$ra_type == "simple") cat("Random assignment procedure: Simple random assignment", "\n")
  if(x$ra_type == "blocked_and_clustered") cat("Random assignment procedure: Blocked and clustered random assignment", "\n")
  if(x$ra_type == "complete") cat("Random assignment procedure: Complete random assignment", "\n")
  cat("Number of units:", n, "\n")
  if(!is.null(x$block_var)){cat("Number of blocks:", length(unique(x$block_var)), "\n")}
  if(!is.null(x$clust_var)){cat("Number of clusters:", length(unique(x$clust_var)),"\n" )}
  
  cat("Number of treatment arms:", num_arms,"\n")
  cat("The possible treatment categories are ", paste(condition_names, collapse = " and "), ".", "\n", sep = "")
  
  if(constant_probabilities){
    cat("The probabilities of assignment are constant across units.")
  }else{
    cat("The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.")
  }
}







