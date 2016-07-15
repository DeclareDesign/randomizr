#' Declare a random assignment procedure.
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param block_var A vector of length N that indicates which block each unit belongs to.
#' @param clust_var A vector of length N that indicates which cluster each unit belongs to.
#' @param m Use for a two-arm design in which m units (or clusters) are assigned to treatment and N-m units (or clusters) are assigned to control. (optional)
#' @param m_each Use for a multi-arm design in which the values of m_each determine the number of units (or clusters) assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units (or clusters) should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)
#' @param prob Use for a two-arm design in which either floor(N*prob) or ceiling(N*prob) units (or clusters) are assigned to treatment. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N*prob) units (or clusters) will be assigned to treatment and with probability prob, ceiling(N*prob) units (or clusters) will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilties of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of units assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param block_m Use for a two-arm design in which block_m describes the number of units to assign to treatment within each block. Note that in previous versions of randomizr, block_m behaved like block_m_each.
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of units (or clusters) assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). The columns should be in the order of condition_names, if specified.
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilties of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilites of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(block_var)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param condition_names A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An execption is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param simple logical, defaults to FALSE. If TRUE, simple random assignment is used. When simple = TRUE, please do not specify m, m_each, block_m, or block_m_each.
#' @param balance_load logical, defaults to FALSE. This feature is experimental. If set to TRUE, the function will resolve rounding problems by randomly assigning "remainder" units to each possible treatment condition with equal probability, while ensuring that the total number of units assigned to each condition does not vary greatly from assignment to assignment. However, the true probabiltiies of assignment may be different from the nominal probabilities specified in prob_each or block_prob_each. Please use with caution and perform many tests before using in a real research scenario.
#'
#' @return A list of class "ra_declaration".  The list has five entries:
#'   $ra_function, a function that generates random assignments accroding to the declaration.
#'   $ra_type, a string indicating the type of random assignment used
#'   $probabilities_matrix, a matrix with N rows and num_arms columns, describing each unit's probabilities of assignment to conditions.
#'   $block_var, the blocking variable.
#'   $clust_var, the clustering variable.
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
                       block_var = NULL,
                       clust_var = NULL,
                       m = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_each = NULL,
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob_each = NULL,
                       num_arms = NULL,
                       condition_names = NULL,
                       simple = FALSE,
                       balance_load = FALSE) {
  
  check_inputs <- check_randomizr_arguments(
    N = N,
    block_var = block_var,
    clust_var = clust_var,
    m = m,
    m_each = m_each,
    prob = prob,
    prob_each = prob_each,
    block_m = block_m,
    block_m_each = block_m_each,
    block_prob_each = block_prob_each,
    num_arms = num_arms,
    condition_names = condition_names
  )
  # Determine ra_type
  if (simple == FALSE) {
    ra_type <- "complete"
  } else{
    ra_type <- "simple"
  }
  if (!is.null(block_var) & is.null(clust_var)) {
    ra_type <- "blocked"
  }
  if (is.null(block_var) & !is.null(clust_var)) {
    ra_type <- "clustered"
  }
  if (!is.null(block_var) & !is.null(clust_var)) {
    ra_type <- "blocked_and_clustered"
  }
  
  if (ra_type == "simple" & is.null(clust_var)) {
    if (!is.null(m)) {
      stop("You can't specify 'm' when using simple random assignment.")
    }
    if (!is.null(m_each)) {
      stop("You can't specify 'm_each' when using simple random assignment.")
    }
    if (!is.null(block_var)) {
      stop("You can't specify 'block_var' when using simple random assignment.")
    }
    if (!is.null(block_m_each)) {
      stop("You can't specify 'block_m_each' when using simple random assignment.")
    }
    
    ra_function <- function() {
      simple_ra(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
    }
    probabilities_matrix <-
      simple_ra_probabilities(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
  }
  
  if (ra_type == "complete") {
    ra_function <- function() {
      complete_ra(
        N = N,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
    }
    
    probabilities_matrix <-
      complete_ra_probabilities(
        N = N,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
    
  }
  
  if (ra_type == "blocked") {
    ra_function <- function() {
      block_ra(
        block_var = block_var,
        num_arms = num_arms,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        prob_each = prob_each,
        block_prob_each = block_prob_each,
        condition_names = condition_names,
        balance_load = balance_load
      )
    }
    
    probabilities_matrix <-
      block_ra_probabilities(
        block_var = block_var,
        num_arms = num_arms,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        prob_each = prob_each,
        block_prob_each = block_prob_each,
        condition_names = condition_names,
        balance_load = balance_load
      )
  }
  
  
  if (ra_type == "clustered") {
    ra_function <- function() {
      cluster_ra(
        clust_var = clust_var,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names,
        simple = simple
      )
    }
    
    probabilities_matrix <-
      cluster_ra_probabilities(
        clust_var = clust_var,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names,
        simple = simple
      )
    
  }
  
  if (ra_type == "blocked_and_clustered") {
    ra_function <- function() {
      block_and_cluster_ra(
        clust_var = clust_var,
        block_var = block_var,
        num_arms = num_arms,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        block_prob_each = block_prob_each,
        prob_each = prob_each,
        condition_names = condition_names,
        balance_load = balance_load
      )
    }
    
    probabilities_matrix <-
      block_and_cluster_ra_probabilities(
        clust_var = clust_var,
        block_var = block_var,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        prob_each = prob_each,
        block_prob_each = block_prob_each,
        num_arms = num_arms,
        condition_names = condition_names,
        balance_load = balance_load
      )
    
  }
  
  return_object <- list(
    ra_function = ra_function,
    ra_type = ra_type,
    probabilities_matrix = probabilities_matrix,
    block_var = block_var,
    clust_var = clust_var
  )
  
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
conduct_ra <- function(ra_declaration) {
  # checks
  if (class(ra_declaration) != "ra_declaration") {
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
obtain_condition_probabilities <-
  function(ra_declaration, assignment) {
    # checks
    if (class(ra_declaration) != "ra_declaration") {
      stop("You must provide a random assignment declaration created by declare_ra().")
    }
    
    probabilities_matrix <- ra_declaration$probabilities_matrix
    cond_Z <- paste0("prob_", assignment)
    indicies <-
      sapply(colnames(probabilities_matrix),
             FUN = x <-
               function(cond_name, cond_Z) {
                 cond_Z == cond_name
               },
             cond_Z = cond_Z)
    cond_probs <-
      as.vector(t(probabilities_matrix))[as.vector(t(indicies))]
    return(cond_probs)
  }

#' @export
print.ra_declaration <- function(x, ...) {
  Z <- x$ra_function()
  n <- length(Z)
  
  condition_names <- sort(unique(Z))
  num_arms <- length(condition_names)
  constant_probabilities <-
    all(apply(
      x$probabilities_matrix,
      2,
      FUN = function(x) {
        all(x[1] == x)
      }
    ))
  
  cat("\n")
  if (x$ra_type == "blocked")
    cat("Random assignment procedure: Block random assignment", "\n")
  if (x$ra_type == "clustered")
    cat("Random assignment procedure: Cluster random assignment", "\n")
  if (x$ra_type == "simple")
    cat("Random assignment procedure: Simple random assignment", "\n")
  if (x$ra_type == "blocked_and_clustered")
    cat("Random assignment procedure: Blocked and clustered random assignment",
        "\n")
  if (x$ra_type == "complete")
    cat("Random assignment procedure: Complete random assignment",
        "\n")
  cat("Number of units:", n, "\n")
  if (!is.null(x$block_var)) {
    cat("Number of blocks:", length(unique(x$block_var)), "\n")
  }
  if (!is.null(x$clust_var)) {
    cat("Number of clusters:", length(unique(x$clust_var)), "\n")
  }
  
  cat("Number of treatment arms:", num_arms, "\n")
  cat(
    "The possible treatment categories are ",
    paste(condition_names, collapse = " and "),
    ".",
    "\n",
    sep = ""
  )
  
  if (constant_probabilities) {
    cat("The probabilities of assignment are constant across units.")
  } else{
    cat(
      "The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights."
    )
  }
}
