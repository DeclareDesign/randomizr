#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP randomizr_restrictedparts(SEXP, SEXP);
extern SEXP randomizr_vsample(SEXP);
extern SEXP _randomizr_block_assign_cpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _randomizr_block_assign_multi_cpp(SEXP, SEXP, SEXP);
extern SEXP _randomizr_cube_two_arm_cpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _randomizr_cube_multi_cpp(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"randomizr_restrictedparts",        (DL_FUNC) &randomizr_restrictedparts,        2},
  {"randomizr_vsample",                (DL_FUNC) &randomizr_vsample,                1},
  {"_randomizr_block_assign_cpp",      (DL_FUNC) &_randomizr_block_assign_cpp,      4},
  {"_randomizr_block_assign_multi_cpp",(DL_FUNC) &_randomizr_block_assign_multi_cpp,3},
  {"_randomizr_cube_two_arm_cpp",      (DL_FUNC) &_randomizr_cube_two_arm_cpp,      4},
  {"_randomizr_cube_multi_cpp",        (DL_FUNC) &_randomizr_cube_multi_cpp,        4},
  {NULL, NULL, 0}
};

void R_init_randomizr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
