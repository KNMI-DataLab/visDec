#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP visDec_timesTwo(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"visDec_timesTwo", (DL_FUNC) &visDec_timesTwo, 1},
  {NULL, NULL, 0}
};

void R_init_visDec(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
