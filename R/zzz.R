# For each _ra/_rs function,
# generate a generic at time of build


.pattern <- "(_ra|_rs)(_probabilities)?$"

.invoke <- function(f){
  f <- match.fun(f)
  function(this){
    args <- mget(names(formals(f)), this)
    do.call(f, args)    
  }  
}


for (.f in ls(pattern=.pattern)) {
  
  
  
    .fclass <- sub(.pattern, "", .f)
    .fgeneric <- regmatches(.f, regexpr(.pattern, .f))
    
    if(.fclass %in% c("declare", "draw", "conduct")) next;
    
    .fclass <- switch(.fclass, 
                      block_and_cluster="blocked_and_clustered", 
                      block="blocked", 
                      cluster="clustered", 
                      strata="stratified",
                      strata_and_cluster="stratified_and_clustered",
                      .fclass)
    
    
    .fgeneric <- switch(.fgeneric, 
                        "_ra"="ra_function.ra_", 
                        "_ra_probabilities"="ra_prob.ra_", 
                        "_rs"="rs_function.rs_",
                        "_rs_probabilities"="rs_prob.rs_", 
                        stop("Build error"))
    
    .f2 <- paste0(.fgeneric, .fclass)
    message(.f, " -> ",  .f2)
    
    assign(.f2, .invoke(.f))

  
  
}

rm(.f, .f2, .fclass, .fgeneric, .pattern, .invoke)