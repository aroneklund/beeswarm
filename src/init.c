#include <stdlib.h> // for NULL
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#include "beeswarm.h"

static R_NativePrimitiveArgType compactSwarm_types[] = {
		REALSXP, INTSXP, INTSXP, INTSXP, REALSXP, REALSXP};

static const R_CMethodDef CEntries[] = {
    {"compactSwarm", (DL_FUNC) &compactSwarm, 6, compactSwarm_types},
    {NULL, NULL, 0, NULL}
};

void R_init_beeswarm(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
