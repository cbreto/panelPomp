#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>

/* pomp/src/probe.c: apply_probe_data (SEXP object, SEXP probes) */
SEXP apply_probe_data(SEXP object, SEXP probes) {
  static SEXP(*fun)(SEXP, SEXP) = NULL;
  if (fun == NULL)
    fun = (SEXP(*)(SEXP, SEXP)) R_GetCCallable("pomp","apply_probe_data");
  return fun(object, probes);
}

/* pomp/src/probe.c: apply_probe_sim (SEXP object, SEXP nsim, SEXP params, SEXP seed, SEXP probes, SEXP datval) */
SEXP apply_probe_sim(SEXP object, SEXP nsim, SEXP params, SEXP seed, SEXP probes, SEXP datval) {
  static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL)
    fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("pomp","apply_probe_sim");
  return fun(object, nsim, params, seed, probes, datval);
}
