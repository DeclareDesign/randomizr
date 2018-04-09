#include "Rinternals.h"

// Given a state 'lambda', generate the next - NULL if final state
// Reimplemented from Andrews 1976 p 232 / Hindenberg algorithm
// Rather than incrementing in place, allocate and return a new vector
//   this greatly simplifies PROTECTion and aligns with our access pattern
// We also store in descending amount 
SEXP successor(SEXP lambda) {
  int m = length(lambda);
  int* x = INTEGER(lambda);
  int lambda_m_2 = x[0] - 2;

  for(int j = 1; j < m; j++) {
    if(lambda_m_2 >= x[j]) {
      
      int lambda_j_1 = x[j] + 1;
      
      SEXP yi = PROTECT(allocVector(INTSXP, m));
      int* y = INTEGER(yi);
      
      int s = 0;  
      for(int i = 1; i <= j; i++) {
        y[i] = lambda_j_1;
        s += x[i] - lambda_j_1;
      }
      
      for(int i = j + 1; i < m; i++) {
        y[i] = x[i];
      }
      
      y[0] = x[0] + s;
      
      UNPROTECT(1);
      return yi;
    };
  }
  
  return NULL;
  
}


SEXP randomizr_restrictedparts(SEXP n, SEXP m) {

  PROTECT_INDEX ipx_out;
  PROTECT_INDEX ipx_succ;
  
  SEXP out, succ;
  // Defaults to 1k buffer  
  PROTECT_WITH_INDEX(out = allocVector(VECSXP, 256), &ipx_out); 

  // Initial state is (N,0,0,...,0)
  PROTECT_WITH_INDEX(succ = allocVector(INTSXP, INTEGER(m)[0]), &ipx_succ);
  
  INTEGER(succ)[0] = INTEGER(n)[0];  
  for(int i = 1; i < length(succ); i++){
    INTEGER(succ)[i] = 0;
  }  
  
  int jj; //used at end to slice to correct size
  for (jj = 0; succ != NULL; jj++) {
    
    if(jj == length(out)) {
      //Rprintf("Growing to jj=%d\n", jj);
      REPROTECT(out = lengthgets(out, jj*2), ipx_out);
    }
    
    SET_VECTOR_ELT(out, jj, succ);
    REPROTECT(succ = successor(succ), ipx_succ);
  } 

  //no reason to reallocate here
  //out = lengthgets(out, jj);
  SETLENGTH(out, jj); 
  
  UNPROTECT(2);  
  
  return out;
} 

/** Equivalent R code:
restrictedparts <- function(n,m){
  
  successor <- function(x){
    for(j in 2:m){
      if(x[1] - x[j] >= 2){
        ret <- x
        ret[2:j] <- x[j] + 1
        ret[1] <- sum(x[1:j], -ret[2:j])
        return(ret)
      }
    }
    return(NULL)
  }
  
  out <- list()

  succ <- integer(m) * 0L
  succ[1] <- n
  

  while(!is.null(succ)) {
    out[[length(out) + 1]] <- succ
    succ <- successor(succ)
  }
  
  out
  
  
  
}
*/
