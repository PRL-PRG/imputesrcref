#include "imputesrcref.h"

static const R_CallMethodDef CallEntries[] = {
    {"C_impute_srcrefs",          (DL_FUNC) &C_impute_srcrefs,          2},
    {"C_get_impute_blacklist",    (DL_FUNC) &C_get_impute_blacklist,    1},
    {"C_set_impute_blacklist",    (DL_FUNC) &C_set_impute_blacklist,    2},
    {"C_reset_impute_blacklist",  (DL_FUNC) &C_reset_impute_blacklist,  0},
    {"C_impute_package_srcrefs",  (DL_FUNC) &C_impute_package_srcrefs,  3},
    {"C_source_impute_srcrefs",   (DL_FUNC) &C_source_impute_srcrefs,   7},
    {NULL, NULL, 0}
};

void R_init_imputesrcref(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
