#include "imputesrcref.h"

static const R_CallMethodDef CallEntries[] = {
    {"C_impute_srcrefs",          (DL_FUNC) &C_impute_srcrefs,          3},
    {"C_get_impute_blacklist",    (DL_FUNC) &C_get_impute_blacklist,    1},
    {"C_set_impute_blacklist",    (DL_FUNC) &C_set_impute_blacklist,    2},
    {"C_reset_impute_blacklist",  (DL_FUNC) &C_reset_impute_blacklist,  0},
    {"C_impute_package_srcrefs",  (DL_FUNC) &C_impute_package_srcrefs,  3},
    {"C_source_impute_srcrefs",   (DL_FUNC) &C_source_impute_srcrefs,   7},
    {"C_collect_srcref_sites",    (DL_FUNC) &C_collect_srcref_sites,    1},
    {"C_collect_transparent_srcref_checks", (DL_FUNC) &C_collect_transparent_srcref_checks, 1},
    {"C_assert_transparent_srcref_consistency", (DL_FUNC) &C_assert_transparent_srcref_consistency, 1},
    {NULL, NULL, 0}
};

void R_init_imputesrcref(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
