#' Probabilties of assignment: Simple Random Assignment
#'
#' @inheritParams simple_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # Two Group Designs
#' simple_ra_probabilities(N=100)
#' simple_ra_probabilities(N=100, prob=0.5)
#' simple_ra_probabilities(N=100, prob_each = c(0.3, 0.7),
#'                         condition_names = c("control", "treatment"))
#' # Multi-arm Designs
#' simple_ra_probabilities(N=100, num_arms=3)
#' simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4))
#' simple_ra_probabilities(N=100, prob_each=c(0.3, 0.3, 0.4),
#'                         condition_names=c("control", "placebo", "treatment"))
#' simple_ra_probabilities(N=100, condition_names=c("control", "placebo", "treatment"))
#'
#' @export
simple_ra_probabilities <-
  function(N,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL) {
    clean_inputs <-
      check_randomizr_arguments(
        N = N,
        prob = prob,
        prob_each = prob_each,
        num_arms = num_arms,
        condition_names = condition_names
      )
    
    num_arms <- clean_inputs$num_arms
    condition_names <- clean_inputs$condition_names
    
    # Three easy cases
    
    if (is.null(prob) & is.null(prob_each)) {
      condition_probabilities <- rep(1 / num_arms, num_arms)
    }
    if (!is.null(prob)) {
      condition_probabilities <- c(1 - prob, prob)
    }
    if (!is.null(prob_each)) {
      condition_probabilities <- prob_each
    }
    
    # Build prob_mat
    prob_mat <- matrix(
      rep(condition_probabilities, N),
      byrow = TRUE,
      ncol = length(condition_probabilities),
      dimnames = list(NULL,  paste0("prob_", condition_names))
    )
    return(prob_mat)
    
  }

#' Probabilties of assignment: Complete Random Assignment
#'
#' @inheritParams complete_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#' # 2-arm designs
#' complete_ra_probabilities(N=100)
#' complete_ra_probabilities(N=100, m=50)
#' complete_ra_probabilities(N=100, prob = .3)
#'
#' complete_ra_probabilities(N=100, m_each = c(30, 70),
#'                           condition_names = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' complete_ra_probabilities(N=100, num_arms=3)
#' complete_ra_probabilities(N=100, m_each=c(30, 30, 40))
#'
#' complete_ra_probabilities(N=100, m_each=c(30, 30, 40),
#'                           condition_names=c("control", "placebo", "treatment"))
#'
#' complete_ra_probabilities(N=100, condition_names=c("control", "placebo", "treatment"))
#' complete_ra_probabilities(N=100, prob_each = c(.2, .7, .1))
#'
#' @export
complete_ra_probabilities <- function(N,
                                      m = NULL,
                                      m_each = NULL,
                                      prob = NULL,
                                      prob_each = NULL,
                                      num_arms = NULL,
                                      condition_names = NULL) {
  # Setup: obtain number of arms and condition_names
  
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
  
  
  if (is.null(m_each) &
      is.null(prob_each) & length(condition_names) == 2) {
    if (N == 1) {
      if (is.null(m) & is.null(prob)) {
        prob_mat <- matrix(
          rep(c(.5, .5), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        if (!m %in% c(0, 1)) {
          stop(
            "The number of units assigned to treatment (m) must be less than or equal to the total number of units (N)"
          )
        }
        if (m == 0) {
          prob_mat <- matrix(
            rep(c(1, 0), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
          return(prob_mat)
        }
        if (m == 1) {
          prob_mat <- matrix(
            rep(c(.5, .5), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
          return(prob_mat)
        }
      }
      if (!is.null(prob)) {
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
    }
    
    if (N > 1) {
      if (is.null(m) & is.null(prob)) {
        prob_mat <- matrix(
          rep(c(.5, .5), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(m)) {
        prob <- m / N
        prob_mat <- matrix(
          rep(c(1 - prob, prob), N),
          byrow = TRUE,
          ncol = 2,
          dimnames = list(NULL,  paste0("prob_", condition_names))
        )
        return(prob_mat)
      }
      if (!is.null(prob)) {
        m_floor <- floor(N * prob)
        m_ceiling <- ceiling(N * prob)
        if (m_ceiling == N) {
          m_ceiling <- m_floor
        }
        
        prob_temp <-
          (m_floor / N) * (1 - prob) + (m_ceiling / N) * (prob)
        
        prob_mat <-
          matrix(
            rep(c(1 - prob_temp, prob_temp), N),
            byrow = TRUE,
            ncol = 2,
            dimnames = list(NULL,  paste0("prob_", condition_names))
          )
        return(prob_mat)
      }
    }
  }
  
  if (is.null(prob_each) & is.null(m_each)) {
    condition_probabilities <- rep(1 / num_arms, num_arms)
  }
  
  if (!is.null(prob_each)) {
    condition_probabilities <- prob_each
  }
  
  if (!is.null(m_each)) {
    condition_probabilities <- m_each / N
  }
  
  # Build prob_mat
  prob_mat <- matrix(
    rep(condition_probabilities, N),
    byrow = TRUE,
    ncol = length(condition_probabilities),
    dimnames = list(NULL,  paste0("prob_", condition_names))
  )
  return(prob_mat)
  
}

#' Probabilties of assignment: Block Random Assignment
#'
#' @inheritParams block_ra
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
#' block_ra_probabilities(block_var=block_var)
#'
#' block_m_each <- rbind(c(25, 25),
#'                  c(50, 50),
#'                  c(100, 100))
#'
#' block_ra_probabilities(block_var=block_var, block_m_each=block_m_each)
#'
#' block_m_each <- rbind(c(10, 40),
#'                  c(30, 70),
#'                  c(50, 150))
#'
#' block_ra_probabilities(block_var=block_var, block_m_each=block_m_each,
#'                        condition_names=c("control", "treatment"))
#'
#' block_ra_probabilities(block_var=block_var, num_arms=3)
#'
#' block_m_each <- rbind(c(10, 20, 20),
#'                  c(30, 50, 20),
#'                  c(50, 75, 75))
#' block_ra_probabilities(block_var = block_var, block_m_each = block_m_each)
#'
#' block_ra_probabilities(block_var=block_var, block_m_each=block_m_each,
#'                        condition_names=c("control", "placebo", "treatment"))
#'
#' block_ra_probabilities(block_var=block_var, prob_each=c(.1, .1, .8))
#'
#'
#' @export
block_ra_probabilities <- function(block_var,
                                   prob = NULL,
                                   prob_each = NULL,
                                   block_m = NULL,
                                   block_m_each = NULL,
                                   block_prob_each = NULL,
                                   num_arms = NULL,
                                   condition_names = NULL,
                                   balance_load = FALSE) {
  #if(all(balance_load & is.null(prob) & is.null(prob_each) & is.null(block_prob_each))){
  #  stop("If you use the experimental feature 'balance_load', then you must specify one of prob, prob_each, or block_prob_each.")
  #}
  
  
  check_inputs <- check_randomizr_arguments(
    block_var = block_var,
    prob = prob,
    prob_each = prob_each,
    block_m = block_m,
    block_m_each = block_m_each,
    block_prob_each = block_prob_each,
    num_arms = num_arms,
    condition_names = condition_names
  )
  
  num_arms <- check_inputs$num_arms
  condition_names <- check_inputs$condition_names
  N_per_block <- check_inputs$N_per_block
  
  blocks <- sort(unique(block_var))
  prob_mat <- matrix(
    NA,
    nrow = length(block_var),
    ncol = length(condition_names),
    dimnames = list(NULL,  paste0("prob_", condition_names))
  )
  
  
  # Case 1 use block_m
  
  if (!is.null(block_m)) {
    for (i in 1:length(blocks)) {
      prob_mat[block_var == blocks[i],] <-
        complete_ra_probabilities(N = N_per_block[i],
                                  m = block_m[i],
                                  condition_names =
                                    condition_names)
    }
    return(prob_mat)
  }
  
  # Case 2 use or infer prob_each
  if (is.null(block_m_each) & is.null(block_prob_each)) {
    if (!is.null(prob)) {
      prob_each <- c(1 - prob, prob)
    }
    
    if (is.null(prob_each)) {
      prob_each <- rep(1 / num_arms, num_arms)
    }
    
    if (balance_load) {
      for_sure_allocations <- floor(N_per_block %*% t(prob_each))
      possible_allocations <-
        matrix(1 / num_arms,
               nrow = length(N_per_block),
               ncol = num_arms)
      
      for_sure_probs <-
        sweep(for_sure_allocations, 1, N_per_block, `/`)
      possible_probs <-
        sweep(possible_allocations,
              1,
              (N_per_block - rowSums(for_sure_allocations)) / N_per_block,
              `*`)
      final_probs <- for_sure_probs + possible_probs
      
      for (i in 1:length(blocks)) {
        dimensions <- dim(prob_mat[block_var == blocks[i], , drop = FALSE])
        prob_mat[block_var == blocks[i],] <-
          matrix(
            final_probs[i,],
            nrow = dimensions[1],
            ncol = dimensions[2],
            byrow = TRUE
          )
      }
      return(prob_mat)
    } else{
      for (i in 1:length(blocks)) {
        prob_mat[block_var == blocks[i],] <-
          complete_ra_probabilities(
            N = N_per_block[i],
            prob_each = prob_each,
            condition_names =
              condition_names
          )
      }
      return(prob_mat)
    }
    
  }
  
  # Case 2 use block_m_each
  
  if (!is.null(block_m_each)) {
    for (i in 1:length(blocks)) {
      prob_mat[block_var == blocks[i],] <-
        complete_ra_probabilities(N = N_per_block[i],
                                  m_each = block_m_each[i,],
                                  condition_names =
                                    condition_names)
    }
    return(prob_mat)
  }
  
  
  # Case 3 use block_prob_each
  
  if (!is.null(block_prob_each)) {
    if (balance_load) {
      for_sure_allocations <-
        floor(sweep(block_prob_each, 1, table(block_var), `*`))
      possible_allocations <-
        matrix(1 / num_arms,
               nrow = length(N_per_block),
               ncol = num_arms)
      
      for_sure_probs <-
        sweep(for_sure_allocations, 1, N_per_block, `/`)
      possible_probs <-
        sweep(possible_allocations,
              1,
              (N_per_block - rowSums(for_sure_allocations)) / N_per_block,
              `*`)
      final_probs <- for_sure_probs + possible_probs
      
      for (i in 1:length(blocks)) {
        dimensions <- dim(prob_mat[block_var == blocks[i],])
        prob_mat[block_var == blocks[i],] <-
          matrix(
            final_probs[i,],
            nrow = dimensions[1],
            ncol = dimensions[2],
            byrow = TRUE
          )
      }
      return(prob_mat)
      
    } else{
      for (i in 1:length(blocks)) {
        prob_mat[block_var == blocks[i],] <-
          complete_ra_probabilities(
            N = N_per_block[i],
            prob_each = block_prob_each[i,],
            condition_names =
              condition_names
          )
      }
      return(prob_mat)
    }
  }
  
  
}

#' Probabilties of assignment: Cluster Random Assignment
#'
#' @inheritParams cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' # Two Group Designs
#' clust_var <- rep(letters, times=1:26)
#' cluster_ra_probabilities(clust_var=clust_var)
#'
#' cluster_ra_probabilities(clust_var=clust_var, m=10)
#'
#' cluster_ra_probabilities(clust_var=clust_var, m_each = c(9, 17),
#'                          condition_names = c("control", "treatment"))
#'
#' # Multi-arm Designs
#' cluster_ra_probabilities(clust_var=clust_var, num_arms=3)
#' cluster_ra_probabilities(clust_var=clust_var, m_each=c(7, 7, 12))
#'
#' cluster_ra_probabilities(clust_var=clust_var, m_each=c(7, 7, 12),
#'                          condition_names=c("control", "placebo", "treatment"))
#'
#' cluster_ra_probabilities(clust_var=clust_var,
#'                          condition_names=c("control", "placebo", "treatment"))
#'
#' cluster_ra_probabilities(clust_var=clust_var, prob_each = c(.1, .2, .7))
#'
#'
#'
#' @export
cluster_ra_probabilities <-
  function(clust_var,
           m = NULL,
           m_each = NULL,
           prob = NULL,
           prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL) {
    unique_clus <- unique(clust_var)
    n_clus <- length(unique_clus)
    probs_clus <-
      complete_ra_probabilities(
        N = n_clus,
        m = m,
        prob = NULL,
        num_arms = num_arms,
        m_each = m_each,
        prob_each = prob_each,
        condition_names = condition_names
      )
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    merged <- merged[order(merged$init_order),]
    prob_mat <- as.matrix(merged[, colnames(probs_clus)])
    return(prob_mat)
  }

#' Probabilties of assignment: Blocked and Clustered Random Assignment
#'
#' @inheritParams block_and_cluster_ra
#'
#' @return A matrix of probabilities of assignment
#'
#' @examples
#'
#' clust_var <- rep(letters, times=1:26)
#' block_var <- rep(NA, length(clust_var))
#' block_var[clust_var %in% letters[1:5]] <- "block_1"
#' block_var[clust_var %in% letters[6:10]] <- "block_2"
#' block_var[clust_var %in% letters[11:15]] <- "block_3"
#' block_var[clust_var %in% letters[16:20]] <- "block_4"
#' block_var[clust_var %in% letters[21:26]] <- "block_5"
#'
#'
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var)
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var,
#'                                    num_arms = 3)
#' block_and_cluster_ra_probabilities(clust_var = clust_var,
#'                                    block_var = block_var,
#'                                    prob_each = c(.2, .5, .3))
#'
#' block_m <- rbind(c(2, 3),
#'                  c(1, 4),
#'                  c(3, 2),
#'                  c(2, 3),
#'                  c(5, 1))
#'
#' block_and_cluster_ra_probabilities(clust_var = clust_var, block_var = block_var, block_m_each = block_m_each)
#'
#'
#' @export
block_and_cluster_ra_probabilities <-
  function(block_var,
           clust_var,
           prob = NULL,
           prob_each = NULL,
           block_m = NULL,
           block_m_each = NULL,
           block_prob_each = NULL,
           num_arms = NULL,
           condition_names = NULL,
           balance_load = FALSE) {
    unique_clus <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clus))
    for (i in 1:length(unique_clus)) {
      clust_blocks[i] <- unique(block_var[clust_var == unique_clus[i]])
    }
    
    probs_clus <- block_ra_probabilities(
      block_var = clust_blocks,
      block_m = block_m,
      block_m_each = block_m_each,
      num_arms = num_arms,
      prob_each = prob_each,
      block_prob_each = block_prob_each,
      condition_names = condition_names,
      balance_load = balance_load
    )
    
    merged <-
      merge(
        x = data.frame(clust_var, init_order = 1:length(clust_var)),
        data.frame(clust_var = unique_clus, probs_clus),
        by = "clust_var"
      )
    merged <- merged[order(merged$init_order),]
    prob_mat <- as.matrix(merged[, colnames(probs_clus)])
    return(prob_mat)
  }
