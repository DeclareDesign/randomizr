###############################################################################
### RS declaration generics

#' @export
`[[<-.ra_declaration` <- function(x, i, j, value ) {
  warning("Cannot assign into ra_declaration")
  NextMethod()
}

#' @export
`$<-.ra_declaration` <- function(x, name, value ) {
  warning("Cannot assign into ra_declaration")
  NextMethod()
}

ra_function <- function(this) UseMethod("ra_function", this)

ra_probabilities     <- function(this) UseMethod("ra_probabilities", this)

ra_function.default <- function(this){
  stop("You must provide a random assignment declaration created by declare_ra().")
}


###############################################################################
### RS declaration generics


#' @export
`[[<-.rs_declaration` <- function(x, i, j, value ) {
  warning("Cannot assign into rs_declaration")
  NextMethod()
}
#' @export
`$<-.rs_declaration` <- function(x, name, value ) {
  warning("Cannot assign into rs_declaration")
  NextMethod()
}

rs_function <- function(this) UseMethod("rs_function", this)

rs_probabilities     <- function(this) UseMethod("rs_probabilities", this)

rs_function.default <- function(this){
  stop("You must provide an random sampling object created by declare_rs().")
}
