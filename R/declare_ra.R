#' Declare a random assignment procedure.
#'
#' @param N The number of units. N must be a positive integer. (required)
#' @param blocks A vector of length N that indicates which block each unit belongs to.
#' @param clusters A vector of length N that indicates which cluster each unit belongs to.
#' @param m Use for a two-arm design in which m units (or clusters) are assigned to treatment and N-m units (or clusters) are assigned to control. In a blocked design, exactly m units in each block will be treated. (optional)
#' @param m_each Use for a multi-arm design in which the values of m_each determine the number of units (or clusters) assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units (or clusters) should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)
#' @param prob Use for a two-arm design in which either floor(N*prob) or ceiling(N*prob) units (or clusters) are assigned to treatment. The probability of assignment to treatment is exactly prob because with probability 1-prob, floor(N*prob) units (or clusters) will be assigned to treatment and with probability prob, ceiling(N*prob) units (or clusters) will be assigned to treatment. prob must be a real number between 0 and 1 inclusive. (optional)
#' @param prob_each Use for a multi-arm design in which the values of prob_each determine the probabilities of assignment to each treatment condition. prob_each must be a numeric vector giving the probability of assignment to each condition. All entries must be nonnegative real numbers between 0 and 1 inclusive and the total must sum to 1. Because of integer issues, the exact number of units assigned to each condition may differ (slightly) from assignment to assignment, but the overall probability of assignment is exactly prob_each. (optional)
#' @param block_m Use for a two-arm design in which block_m describes the number of units to assign to treatment within each block. Note that in previous versions of randomizr, block_m behaved like block_m_each.
#' @param block_m_each Use for a multi-arm design in which the values of block_m_each determine the number of units (or clusters) assigned to each condition. block_m_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). The columns should be in the order of conditions, if specified.
#' @param block_prob Use for a two-arm design in which block_prob describes the probability of assignment to treatment within each block. Differs from prob in that the probability of assignment can vary across blocks.
#' @param block_prob_each Use for a multi-arm design in which the values of block_prob_each determine the probabilities of assignment to each treatment condition. block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilities of assignment to treatment within each block. The rows should respect the ordering of the blocks as determined by sort(unique(blocks)). Use only if the probabilities of assignment should vary by block, otherwise use prob_each. Each row of block_prob_each must sum to 1.
#' @param num_arms The number of treatment arms. If unspecified, num_arms will be determined from the other arguments. (optional)
#' @param conditions A character vector giving the names of the treatment groups. If unspecified, the treatment groups will be named 0 (for control) and 1 (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm trial. An exception is a two-group design in which num_arms is set to 2, in which case the condition names are T1 and T2, as in a multi-arm trial with two arms. (optional)
#' @param simple logical, defaults to FALSE. If TRUE, simple random assignment is used. When simple = TRUE, please do not specify m, m_each, block_m, or block_m_each.
#' @param permutation_matrix for custom random assignment procedures.
#' @param check_inputs logical. Defaults to TRUE.
#' @param block_var deprecated
#' @param clust_var deprecated
#' @param condition_names deprecated
#'
#' @return A list of class "declaration".  The list has five entries:
#'   $ra_function, a function that generates random assignments according to the declaration.
#'   $ra_type, a string indicating the type of random assignment used
#'   $probabilities_matrix, a matrix with N rows and num_arms columns, describing each unit's probabilities of assignment to conditions.
#'   $blocks, the blocking variable.
#'   $clusters, the clustering variable.
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
#'            conditions=c("control", "placebo", "treatment"), simple=TRUE)
#'
#' # Complete Random Assignment Declarations
#'
#' declare_ra(N=100)
#' declare_ra(N=100, m_each = c(30, 70),
#'            conditions = c("control", "treatment"))
#' declare_ra(N=100, m_each=c(30, 30, 40))
#'
#'
#' # Block Random Assignment Declarations
#'
#' blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))
#  declare_ra(blocks = blocks)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declare_ra(blocks = blocks, block_m_each = block_m_each)
#'
#'
#' # Cluster Random Assignment Declarations
#'
#' clusters <- rep(letters, times = 1:26)
#' declare_ra(clusters = clusters)
#' declare_ra(clusters = clusters, m_each = c(7, 7, 12))
#'
#' # Blocked and Clustered Random Assignment Declarations
#'
#' clusters <- rep(letters, times=1:26)
#' blocks <- rep(NA, length(clusters))
#' blocks[clusters %in% letters[1:5]] <- "block_1"
#' blocks[clusters %in% letters[6:10]] <- "block_2"
#' blocks[clusters %in% letters[11:15]] <- "block_3"
#' blocks[clusters %in% letters[16:20]] <- "block_4"
#' blocks[clusters %in% letters[21:26]] <- "block_5"
#'
#' table(blocks, clusters)
#'
#' declare_ra(clusters = clusters, blocks = blocks)
#' declare_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
#'
#' @export
declare_ra <- function(N = NULL,
                       blocks = block_var,
                       clusters = clust_var,
                       m = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_each = NULL,
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob = NULL,
                       block_prob_each = NULL,
                       num_arms = NULL,
                       conditions = condition_names,
                       simple = FALSE,
                       permutation_matrix = NULL,
                       check_inputs = TRUE, 
                       block_var = NULL, 
                       clust_var = NULL,
                       condition_names = NULL) {
  input_check <- NULL
  
  warn_deprecated_args(block_var, clust_var)
    
  if (check_inputs & is.null(permutation_matrix)) {
    input_check <- check_randomizr_arguments(
      N = N,
      blocks = blocks,
      clusters = clusters,
      m = m,
      m_each = m_each,
      prob = prob,
      prob_each = prob_each,
      block_m = block_m,
      block_m_each = block_m_each,
      block_prob = block_prob,
      block_prob_each = block_prob_each,
      num_arms = num_arms,
      conditions = conditions
    )
  }
  # Determine ra_type
  if (!is.null(permutation_matrix)){
    ra_type <- "custom"
  } else  if (!is.null(blocks) && !is.null(clusters)) {
    ra_type <- "blocked_and_clustered"
  } else  if (!is.null(clusters)) {
    ra_type <- "clustered"
  } else  if (!is.null(blocks)) {
    ra_type <- "blocked"
    if (simple) stop("You can't specify 'simple' when using blocked assignment")
  } else  if (simple == FALSE) {
    ra_type <- "complete"
  } else {
    ra_type <- "simple"
    
    if (!is.null(m)) {
      stop("You can't specify 'm' when using simple random assignment.")
    }
    if (!is.null(m_each)) {
      stop("You can't specify 'm_each' when using simple random assignment.")
    }
    if (!is.null(block_m_each)) {
      stop("You can't specify 'block_m_each' when using simple random assignment.")
    }
    
  }

  
  if (ra_type == "simple") {
    
    ra_function <- function() {
      simple_ra(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        conditions = conditions,
        check_inputs = check_inputs
      )
    }
    probabilities_matrix <-
      simple_ra_probabilities(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        conditions = conditions,
        check_inputs = check_inputs
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
        conditions = conditions,
        check_inputs = check_inputs
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
        conditions = conditions,
        check_inputs = check_inputs
      )
    
  }
  
  if (ra_type == "blocked") {
    ra_function <- function() {
      block_ra(
        blocks = blocks,
        num_arms = num_arms,
        m = m,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        prob_each = prob_each,
        block_prob = block_prob,
        block_prob_each = block_prob_each,
        conditions = conditions,
        check_inputs = check_inputs
      )
    }
    
    probabilities_matrix <-
      block_ra_probabilities(
        blocks = blocks,
        num_arms = num_arms,
        m = m,
        block_m = block_m,
        block_m_each = block_m_each,
        prob = prob,
        prob_each = prob_each,
        block_prob = block_prob,
        block_prob_each = block_prob_each,
        conditions = conditions,
        check_inputs = check_inputs
      )
  }
  
  
  if (ra_type == "clustered") {
    ra_function <- function() {
      cluster_ra(
        clusters = clusters,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        conditions = conditions,
        simple = simple,
        check_inputs = check_inputs
      )
    }
    
    probabilities_matrix <-
      cluster_ra_probabilities(
        clusters = clusters,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        conditions = conditions,
        simple = simple,
        check_inputs = check_inputs
      )
    
  }
  
  if (ra_type == "blocked_and_clustered") {
    ra_function <- function() {
      block_and_cluster_ra(
        clusters = clusters,
        blocks = blocks,
        num_arms = num_arms,
        prob = prob,
        prob_each = prob_each,
        m = m,
        block_m = block_m,
        block_m_each = block_m_each,
        block_prob = block_prob,
        block_prob_each = block_prob_each,
        conditions = conditions,
        check_inputs = check_inputs
      )
    }
    
    probabilities_matrix <-
      block_and_cluster_ra_probabilities(
        clusters = clusters,
        blocks = blocks,
        prob = prob,
        prob_each = prob_each,
        block_prob = block_prob,
        m = m,
        block_m = block_m,
        block_m_each = block_m_each,
        block_prob_each = block_prob_each,
        num_arms = num_arms,
        conditions = conditions,
        check_inputs = check_inputs
      )
    
  }
  
  if (ra_type == "custom"){
    ra_function <- function(){
      permutation_matrix[,sample(ncol(permutation_matrix), 1)]
    }
    conditions <- sort(unique(c(permutation_matrix)))
    
    probabilities_matrix <- 
      sapply(conditions, 
             FUN = function(x) apply(permutation_matrix, MARGIN = 1, FUN = function(y) mean(y == x)))
    
    colnames(probabilities_matrix) <- paste0("prob_", conditions)
    
  }
  
  
  return_object <- list(
    ra_function = ra_function,
    ra_type = ra_type,
    probabilities_matrix = probabilities_matrix,
    permutation_matrix = permutation_matrix,
    blocks = blocks,
    clusters = clusters,
    original_call = match.call(),
    cleaned_arguments = input_check
  )
  
  class(return_object) <- "ra_declaration"
  return(return_object)
  
}

#' Conduct a random assignment
#'
#' You can either give conduct_ra() an declaration, as created by \code{\link{declare_ra}} or you can specify the other arguments to describe a random assignment procedure.
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @inheritParams declare_ra
#' @examples
#' declaration <- declare_ra(N = 100, m_each = c(30, 30, 40))
#' Z <- conduct_ra(declaration = declaration)
#' table(Z)
#'
#' # equivalent to
#'
#' Z <- conduct_ra(N = 100, m_each = c(30, 30, 40))
#' table(Z)
#'
#' @export
conduct_ra <- function(declaration = NULL,
                       N = NULL,
                       blocks = block_var,
                       clusters = clust_var,
                       m = NULL,
                       m_each = NULL,
                       prob = NULL,
                       prob_each = NULL,
                       block_m = NULL,
                       block_m_each = NULL,
                       block_prob = NULL,
                       block_prob_each = NULL,
                       num_arms = NULL,
                       conditions = condition_names,
                       simple = FALSE,
                       check_inputs = TRUE, 
                       block_var = NULL, 
                       clust_var = NULL,
                       condition_names = NULL) {
  if (!is.null(declaration)) {
    if (class(declaration) != "ra_declaration") {
      stop("You must provide a random assignment declaration created by declare_ra().")
    }
  } else{
    declaration <-
      declare_ra(
        N = N,
        blocks = blocks,
        clusters = clusters,
        m = m,
        m_each = m_each,
        prob = prob,
        prob_each = prob_each,
        block_m = block_m,
        block_m_each = block_m_each,
        block_prob = block_prob,
        block_prob_each = block_prob_each,
        num_arms = num_arms,
        conditions = conditions,
        simple = simple,
        check_inputs = check_inputs
      )
    
  }
  return(declaration$ra_function())
}

#' Obtain the probabilities of units being in the conditions that they are in.
#'
#' You can either give obtain_condition_probabilities() an declaration, as created by \code{\link{declare_ra}} or you can specify the other arguments to describe a random assignment procedure.\cr \cr
#' This function is especially useful when units have different probabilities of assignment and the analyst plans to use inverse-probability weights.
#'
#'
#' @param declaration A random assignment declaration, created by \code{\link{declare_ra}}.
#' @param assignment A vector of random assignments, often created by \code{\link{conduct_ra}}.
#' @inheritParams declare_ra
#'
#' @examples
#'
#' # Conduct a block random assignment
#' blocks <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#' declaration <- declare_ra(blocks = blocks, block_m_each = block_m_each)
#' Z <- conduct_ra(declaration = declaration)
#' table(Z, blocks)
#'
#' observed_probabilities <-
#'    obtain_condition_probabilities(declaration = declaration, assignment = Z)
#'
#'
#' # Probabilities in the control group:
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#'
#' # Probabilities in the treatment group:
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#'
#' # Sometimes it is convenient to skip the declaration step
#' Z <- conduct_ra(blocks = blocks, block_m_each = block_m_each)
#' observed_probabilities <-
#'    obtain_condition_probabilities(assignment = Z,
#'                                   blocks = blocks,
#'                                   block_m_each = block_m_each)
#' table(observed_probabilities[Z == 0], blocks[Z == 0])
#' table(observed_probabilities[Z == 1], blocks[Z == 1])
#'
#' @export
obtain_condition_probabilities <-
  function(declaration = NULL,
           assignment,
           N = NULL,
           blocks = block_var,
           clusters = clust_var,
           m = NULL,
           m_each = NULL,
           prob = NULL,
           prob_each = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           conditions = condition_names,
           simple = FALSE, 
           block_var = NULL, 
           clust_var = NULL,
           condition_names = NULL) {
    # checks
    if (!is.null(declaration)) {
      if (class(declaration) != "ra_declaration") {
        stop("You must provide a random assignment declaration created by declare_ra().")
      }
    } else{
      if (is.null(N)) {
        N <- length(assignment)
      }
      declaration <-
        declare_ra(
          N = N,
          blocks = blocks,
          clusters = clusters,
          m = m,
          m_each = m_each,
          prob = prob,
          prob_each = prob_each,
          block_m = block_m,
          block_m_each = block_m_each,
          block_prob = block_prob,
          block_prob_each = block_prob_each,
          num_arms = num_arms,
          conditions = conditions,
          simple = simple
        )
    }
    
    probabilities_matrix <- declaration$probabilities_matrix
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
  
  conditions <- sort(unique(Z))
  num_arms <- length(conditions)
  constant_probabilities <-
    all(apply(
      x$probabilities_matrix,
      2,
      FUN = function(x) {
        all(x[1] == x)
      }
    ))
  
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
  if (!is.null(x$blocks)) {
    cat("Number of blocks:", length(unique(x$blocks)), "\n")
  }
  if (!is.null(x$clusters)) {
    cat("Number of clusters:", length(unique(x$clusters)), "\n")
  }
  
  cat("Number of treatment arms:", num_arms, "\n")
  cat(
    "The possible treatment categories are ",
    paste(conditions, collapse = " and "),
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
