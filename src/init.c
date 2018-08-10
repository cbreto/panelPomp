#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "pompAPI.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  {NULL, NULL, 0}
};

void R_init_panelPomp(DllInfo *info)
{
  // Register routines
  R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
  }
