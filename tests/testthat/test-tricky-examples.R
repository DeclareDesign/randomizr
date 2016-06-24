rm(list=ls())


# Compare tricky examples vs. randomizr

#devtools::install_github("acoppock/randomizr")
library(randomizr)
library(testthat)

# Block randomization function
# Allows specification of probabilities for each condition but not separate probs for each block
ra <- function(
  n = 10, 
  block = rep(1,n), 
  probs=.5, 
  treat.name = NULL, 
  seed=NULL){
  
  # Housekeeping
  n <- length(block)
  blocks <- sort(unique(block))
  if(length(probs) == 1) probs <- c(1-probs, probs) # For binary case only one value needs to be given
  if(min(probs)<0) stop("Check negative probs")
  if(max(probs)>1) stop("Check large probs")
  if(sum(probs)!=1) {probs <- (probs/sum(probs)); print("Probs don't sum to 1: rescaling")}
  if(!is.null(seed)) set.seed(seed)
  if(is.null(treat.name)) treat.name <- 0:(length(probs)-1)
  print(cbind(treat.name, probs))
  # if necessary determine randomly total allocated to each group
  new.target <- floor(n*probs)
  add <- round(sum(n*probs - new.target),0)
  if(add!=0) {
    print("Totals in treatment condtions determined stochastically")
    new.target <- new.target + systematic(n*probs - new.target, n = add, mywarning=FALSE)
  }
  
  # ensure min within each block
  bsize <- sapply(blocks, function(j) sum(block==j))
  b.target <- sapply(blocks, function(j) floor(probs*bsize[blocks==j]))
  
  # redistribution
  left <- new.target - apply(b.target, 1, sum)
  add <- adj <- matrix(0, length(probs), length(blocks))
  resids <- sapply(blocks, function(j) (probs*bsize[blocks==j])) - b.target
  qs <- resids
  
  # Distribute row by row and readjust targets
  for(k in 1:length(probs)) {
    add[k,] <-systematic(qs[k,], left[k], mywarning=FALSE)
    adj[k,] <- resids[k,] - add[k,]
    
    # do for each column to readjust targets
    if(k<length(probs)) {for(j in 1:length(blocks)){
      
      if(sum(resids[(k+1):length(probs), j])>0){
        qs[(k+1):length(probs), j] <- resids[(k+1):length(probs), j]*
          (sum(resids[(k+1):length(probs), j])+ sum(adj[1:k,j]))/
          sum(resids[(k+1):length(probs), j])
      }
    }
    }
  }
  
  b.target <- b.target + add
  
  out<-rep(NA, n)
  for(b in 1:length(blocks)) {
    if(sum(b.target[,b])>0){
      LL <- unlist(sapply(1:nrow(b.target), function (k) c(rep2(treat.name[k], b.target[k,b]))))
      LL <- LL[!is.na(LL)]
      out[block==blocks[b]] <- resample(LL)
    }
  }
  
  return(out)
} 

# Helper function: Systematic
# Provide an ordered vector of probabilities (summing to an integer) and 
# systematic will return a systematic randomization. The usual case has each 
# probability less than 1; if greater than 1 then some selection is guaranteed

systematic = function(
  ps, 
  n=1, 
  seed=NULL, 
  mywarning = TRUE, 
  warningtext="(sum(ps)!=n): reweighting ps"){
  
  if(sum(ps)!=n & mywarning) print(warningtext)
  if(!is.null(seed)) set.seed(seed)
  if(min(ps)< -.01 & mywarning){ stop("stopping because value for p < -.01 found")}
  if(min(ps)< 0){ if(mywarning) print("small negative ps found")
    ps <- ps-min(ps)
  } 
  
  
  if(sum(ps)!=n & sum(ps)!=0) ps <- {ps*n/sum(ps)} # reweight ps if needed
  
  k <- length(ps) 
  base <- floor(ps)
  ps <- ps-base
  
  if(sum(ps)==0) {
    add <- rep(0,k)
  } else {
    s <- ((cumsum(ps) +n*runif(1))%%n)
    e <- s - floor(s)
    add <- 1*(e < c(e[k], e[-k]))
  }
  base + add
}

# Examples
systematic(c(0,1))
systematic(c(-.0001,1))
systematic(c(.5,.5))
systematic(ps = c(.5,.5), n = 2, mywarning=FALSE)
systematic(c(0, 1.8, .2))
systematic(c(0, 1.8, .2), n = 2)


# Helper to improve behavior of rep with 0 
rep2 = function(a,b) { 
  if(b==0) {X<- NA
  } else {
    X <- rep(a,times = b)}
  return(X)
}

# Helper to deal with sampling from a scalar: note sample(4) = sample(1:4), but resample(4) --> 4
resample <- function(x, ...) x[sample.int(length(x), ...)] 



# Examples ----------------------------------------------------------------

B <- rep(2:4,9)

# Checks out
table(B, ra(block = B, probs=rep(1/3,3)))
table(B, block_ra(block_var = B, prob_each = rep(1/3,3)))

# Checks out with manual rescale
table(B, ra(block = B, probs=c(.33,.33,.33)))
# randomizr doesn't rescale
expect_error(table(B, block_ra(block_var = B, prob_each = c(.33,.33,.33))))
table(B, block_ra(block_var = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))


# Works
B <- c("A", "B", "A", "D")
# weird, sometimes throws a warning
#Warning message:
#In out[block == blocks[b]] <- resample(LL) :
#  number of items to replace is not a multiple of replacement length
table(B, ra(block = B, probs=c(.33,.33,.33)))
table(B, block_ra(block_var = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))


B <- c("A", "B", "D")
table(B, ra(block = B, probs=c(.33,.33,.33)))
table(B, block_ra(block_var = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))



table(c(B,B), ra(block = c(B,B), probs=c(.43,.33,.33)))
table(c(B,B), block_ra(block_var= c(B,B), prob_each=c(.43,.33,.33)/sum(c(.43,.33,.33))))


B = c(1,2,1,2,1)
table(B, ra(block = B, probs=c(.33,.33,.33)))
table(B, block_ra(block_var = B, prob_each = c(.33,.33,.33)/sum(c(.33,.33,.33))))

# Complete random assignment for factorial
table(ra(n = 16))
table(complete_ra(16))

table(ra(n = 16, probs = .25))
table(complete_ra(16, prob = .25))


# Complete random assignment into 4 categories eg for factorial
n <- 16
table(ra(n = 16, probs = rep(.25, 4), treat.name = c("T00", "T01", "T10", "T11")))
table(complete_ra(16, prob_each = rep(.25, 4), condition_names = c("T00", "T01", "T10", "T11")))


# Block examples
B <- c("A","A","B","B")
table(B, ra(block = B))

table(B, block_ra(block_var = B))


# More complex examples
B <- c(1,1,2,2)
table(ra(block = B, probs=c(.21,.29,.5)))
table(block_ra(block_var =B, prob_each=c(.21,.29,.5)))
# NOTE: both procedures sometimes have no units in a condition!


# Global balance even if within block balance not possible
B <- c(1,1,1,2,2,2)
table(B, ra(block = B, probs=.5))
table(B, block_ra(block_var = B, prob = .5))

B <- c(1,1,1,2,2,2,2)
table(B, ra(block = B, probs=.5))
table(B, block_ra(block_var = B, prob = .5))

B <- c(1,1,1,2,2,2,2,2,2)
table(B, ra(block = B, probs=c(1/3,1/3,1/3)))
table(B, block_ra(block_var = B, prob_each =c(1/3,1/3,1/3)))

B <- c(1,1,1,2,2,2,2,2,2, 3, 4, 4)
table(B, ra(block = B, probs=c(1/6,1/6,1/6, 1/2), treat.name = c("A", "B", "C", "D")))

table(B, block_ra(block_var = B, prob_each =c(1/6,1/6,1/6, 1/2), 
                  condition_names = c("A", "B", "C", "D")))



