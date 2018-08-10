#include <R.h>
#include <R_ext/Rdynload.h>

#include "pomp_defines.h"

static apply_probe_data_t *Apply_Probe_Data = NULL;
static apply_probe_sim_t *Apply_Probe_Sim = NULL;

SEXP apply_probe_data (SEXP object, SEXP probes) {
  return Apply_Probe_Data(object,probes);
}

SEXP apply_probe_sim (SEXP object, SEXP nsim, SEXP params, SEXP seed, SEXP probes, SEXP datval) {
  return Apply_Probe_Sim(object,nsim,params,seed,probes,datval);
}


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  {"apply_probe_data", (DL_FUNC) &apply_probe_data, 2},
  {"apply_probe_sim", (DL_FUNC) &apply_probe_sim, 6},
  {NULL, NULL, 0}
};

void R_init_panelPomp(DllInfo *info)
{
  Apply_Probe_Data = (apply_probe_data_t *) RGetCCallable("pomp","apply_probe_data");
  Apply_Probe_Sim = (apply_probe_sim_t *) RGetCCallable("pomp","apply_probe_sim");
  
  // Register routines
  R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
