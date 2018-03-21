
.pattern <- "(_ra|_ra_probabilities|_rs|_rs_probabilities)$"

for (.f in ls(pattern=.pattern)) {
  
    .fclass <- sub(.pattern, "", .f)
    .fgeneric <- regmatches(.f, regexpr(.pattern, .f))
    
    .fclass <- switch(.fclass, 
                      block_and_cluster="blocked_and_clustered", 
                      block="blocked", 
                      cluster="clustered", 
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

rm(.f, .f2, .fclass, .fgeneric, .pattern)