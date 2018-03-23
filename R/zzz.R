# For each _ra/_rs function,
# generate a generic at time of build


.pattern <- "_(ra|rs)(_probabilities|)$"

.invoke <- function(f){
  what <- names(formals(match.fun(f)))
  function(this){
    do.call(f, mget(what, this))    
  }  
}


for (.f in ls(pattern=.pattern)) {
  

    .fclass <- sub(.pattern, "", .f)
    .fgeneric <- regmatches(.f, regexec(.pattern, .f))[[1]]
    
    if(.fclass %in% c("declare", "draw", "conduct")) next;
    
    .fclass <- switch(.fclass, 
                      block_and_cluster="blocked_and_clustered", 
                      block="blocked", 
                      cluster="clustered", 
                      strata="stratified",
                      strata_and_cluster="stratified_and_clustered",
                      .fclass)
    
    if(.fgeneric[3] == "") .fgeneric[3] = "_function"

    .f2 <- sprintf("%s%s.%s_%s", .fgeneric[2], .fgeneric[3], .fgeneric[2], .fclass)
    message(.f, " -> ",  .f2)
    
    assign(.f2, .invoke(.f))

  
  
}

rm(.f, .f2, .fclass, .fgeneric, .pattern, .invoke)