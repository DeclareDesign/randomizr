#include "Rinternals.h"
#include "R_ext/Random.h"


SEXP randomizr_vsample(SEXP pmat) {
  
  double* input = REAL(pmat);

  int *dims = INTEGER(getAttrib(pmat, R_DimSymbol));
  
    
  int i, j;
  int m = dims[0], n = dims[1];
  
  SEXP ret = PROTECT(allocVector(INTSXP, m));
  
  int* out = INTEGER(ret);
  
  for(i = 0; i < m; i++) {
    double r = unif_rand();
    for(j = 0; r > 0 && j < n; j++) {
      r = r - input[j*m + i];
    } 
    
    out[i] = j;
  }

  UNPROTECT(1);  
  
  return ret;
}

/*** R code
  
 i <- max.col(runif(nrow(prob_mat)) <=  t(apply(prob_mat, 1, cumsum)), "first")

*/
