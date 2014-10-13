# randomizr

#' Complete Random Assignment
#'
#' This function assigns exactly m of N units to treatment.
#' @param N the total number of units in the experimental sample
#' @param m the total number treatments to be allocated
#' @keywords random assignment
#' @export
#' @examples
#' Z <- complete_ra(N=100, m=50)
#' table(Z)
complete_ra <- function(N,m){
assign <- ifelse(1:N %in% sample(1:N,m),1,0)
return(assign)
}

#' Simple Random Assignment
#'
#' This function assigns N units to treatment with probability p.  It is a wrapper for rbinom.
#' @param N the total number of units in the experimental sample
#' @param p the probability of assignment
#' @keywords random assignment
#' @export
#' @examples
#' Z <- simple_ra(N=100, prob=0.5)
#' table(Z)
simple_ra <- function(N, prob){
assign <- rbinom(n=N,size=1,prob=prob)
return(assign)
}

#' Block Random Assignment
#'
#' This function assigns a fixed number of units within each block to treatment.
#' @param blockvar A vector of length N that includes the blocking variable
#' @param block_m A vector whose length is equal to the number of blocks and describes the number of units in each block to be assigned to treatment. Put this vector in alphabetical order.
#' @keywords random assignment
#' @export
#' @examples
#' blockvar <- rep(c("A", "B","C"), each=100)
#' block_m <- c(50, 40, 30)
#' Z <- block_random_assignment(blockvar=blockvar, block_m=block_m)
#' table(blockvar, Z)
block_random_assignment <- function(blockvar, block_m){
  blocks <- sort(unique(blockvar))
  assign <- rep(NA, length(blockvar))
  for(i in 1:length(blocks)){
    N_block <- sum(blockvar==blocks[i])
    assign[blockvar==blocks[i]] <- ifelse(1:N_block %in% sample(1:N_block, block_m[i]),1,0)
  }
  return(assign)
}




#' Complete Random Assignment with multiple arms
#'
#' This function assigns a fixed number of units to multiple treatments.
#' @param N the total number of units in the experimental sample
#' @param num_arms the number of treatment arms
#' @param groupsizes A vector of numeric group sizes.  Defaults to NULL in case of equal group sizes.
#' @keywords random assignment
#' @export
#' @examples
#' Z <- complete_ra_multiple_arms(300, 3)
#' table(Z)
#' 
#' Z <- complete_ra_multiple_arms(300, 3, groupsizes=c(100, 50, 150))
#' table(Z)
complete_ra_multiple_arms <- function(N, num_arms, groupsizes=NULL){
  indices <- 1:N
  assign <- rep(NA, N)
  if (is.null(groupsizes)){
    for (i in 1:num_arms){
      chosen  <- sample(indices, (N/num_arms))
      assign[chosen] <- paste0("T",i)
      indices <- indices[!indices %in% chosen]
    }
    return(assign)
  }
  if(sum(groupsizes) !=N){stop('The sum of groupsizes must be equal to "N".')}
  for (i in 1:length(groupsizes)){
    chosen <- sample(indices, groupsizes[i])
    assign[chosen] <- paste0("T",i)
    indices <- indices[!indices %in% chosen]
  } 
  return(assign)
}



