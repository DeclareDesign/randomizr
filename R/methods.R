###############################################################################
### RS declaration generics

#' @export
`[<-.ra_declaration` <- function(x, i, j, value ) stop("Cannot assign into ra_declaration")

#' @export
`$<-.ra_declaration` <- function(x, name, value ) stop("Cannot assign into ra_declaration")

ra_function <- function(this) UseMethod("ra_function", this)

ra_prob     <- function(this) UseMethod("ra_prob", this)

ra_function.default <- function(this){
  stop("You must provide a random assignment declaration created by declare_ra().")
}


###############################################################################
### RS declaration generics


#' @export
`[<-.rs_declaration` <- function(x, i, j, value ) stop("Cannot assign into rs_declaration")

#' @export
`$<-.rs_declaration` <- function(x, name, value ) stop("Cannot assign into rs_declaration")

rs_function <- function(this) UseMethod("rs_function", this)

rs_prob     <- function(this) UseMethod("rs_prob", this)

rs_function.default <- function(this){
  stop("You must provide an random sampling object created by declare_rs().")
}
