# # For each _ra/_rs function,
# # generate a generic
# # dump them out to generated_methods.R
# # Need to temporarily export custom_ra
# 
# .pattern <- "_(ra|rs)(_probabilities|)$"
# 
# .invoke <- function(f){
#   what <- names(formals(match.fun(f)))
#   fun <- function(this){
# 
#   }
#   args <- lapply(setNames(nm=what), function(x) call("[[", as.symbol("this"), x) )
# 
#   body(fun) <- as.call(append(as.symbol(f), args))
# 
#   fun
# }
# 
# .out <- new.env(parent = emptyenv())
# 
# for (.f in ls(pattern=.pattern, asNamespace("randomizr"))) {
# 
# 
#     .fclass <- sub(.pattern, "", .f)
#     .fgeneric <- regmatches(.f, regexec(.pattern, .f))[[1]]
# 
#     if(.fclass %in% c("declare", "draw", "conduct")) next;
# 
#     .fclass <- switch(.fclass,
#                       block_and_cluster="blocked_and_clustered",
#                       block="blocked",
#                       cluster="clustered",
#                       strata="stratified",
#                       strata_and_cluster="stratified_and_clustered",
#                       .fclass)
# 
#     if(.fgeneric[3] == "") .fgeneric[3] = "_function"
# 
#     .f2 <- sprintf("%s%s.%s_%s", .fgeneric[2], .fgeneric[3], .fgeneric[2], .fclass)
#     message(.f, " -> ",  .f2)
# 
#     assign(.f2, .invoke(.f), .out)
# 
# 
# 
# }
# 
# dump(ls(.out), file = "R/generated_methods.R", envir = .out)
# 
# rm(.f, .f2, .fclass, .fgeneric, .pattern, .invoke, .out)
