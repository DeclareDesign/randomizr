#' randomizr
#' 
#' Easy-to-Use Tools for Common Forms of Random Assignment and Sampling
#'
#' The 2.0 rewrite introduced this package's first C++ (`src/block_assign.cpp`).
#' Rcpp resolves its own C entry points, `enterRNGScope` among them, only after
#' its namespace has been loaded, and `Imports: Rcpp` in DESCRIPTION does not
#' load it. Without the directive below every call into the new code died on
#' `function 'enterRNGScope' not provided by package 'Rcpp'`, which is what
#' `R CMD check` was reporting as 3 ERRORs. Routine registration is done by hand
#' in `src/onload.c`, so `useDynLib` needs no `.registration = TRUE`.
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stats runif
#' @name randomizr
#' @docType package
NULL
