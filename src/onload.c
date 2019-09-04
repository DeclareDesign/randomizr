#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP randomizr_restrictedparts(SEXP, SEXP);
extern SEXP randomizr_vsample(SEXP);



static const R_CallMethodDef CallEntries[] = {
  {"randomizr_restrictedparts", (DL_FUNC) &randomizr_restrictedparts, 2},
  {"randomizr_vsample", (DL_FUNC) &randomizr_vsample, 1},
  {NULL, NULL, 0}
};

void R_init_randomizr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
