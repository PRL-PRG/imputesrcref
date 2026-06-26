#include "imputesrcref.h"
#include <string.h>
#include <stdlib.h>

static SEXP specialsxp_cache = NULL;

static const char *rlang_nse_names[] = {
    "expr", "quo", "quos", "enquo", "enquos", "enexpr", "enexprs"
};

static SEXP compute_specialsxp_names(void) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("builtins"), R_BaseEnv));
    SEXP call = PROTECT(Rf_lang1(fn));
    SEXP nms = PROTECT(Rf_eval(call, R_GlobalEnv));
    R_xlen_t n = Rf_xlength(nms);

    int *keep = (int*) R_alloc((size_t) n, sizeof(int));
    int kept = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        const char *nm = CHAR(STRING_ELT(nms, i));
        SEXP sym = Rf_install(nm);
        int err = 0;
        SEXP obj = R_tryEvalSilent(sym, R_BaseEnv, &err);
        if (!err && TYPEOF(obj) == SPECIALSXP) {
            keep[i] = 1;
            kept++;
        } else {
            keep[i] = 0;
        }
    }

    SEXP out = PROTECT(Rf_allocVector(STRSXP, kept));
    int j = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (keep[i]) {
            SET_STRING_ELT(out, j++, STRING_ELT(nms, i));
        }
    }

    UNPROTECT(4);
    return out;
}

static int strvec_cmp(const void *a, const void *b) {
    const char *sa = CHAR(*(SEXP const*) a);
    const char *sb = CHAR(*(SEXP const*) b);
    return strcmp(sa, sb);
}

static SEXP sort_unique_strvec(SEXP x) {
    R_xlen_t n = Rf_xlength(x);
    if (n == 0) return x;
    SEXP *buf = (SEXP*) R_alloc((size_t) n, sizeof(SEXP));
    for (R_xlen_t i = 0; i < n; i++) buf[i] = STRING_ELT(x, i);
    qsort(buf, (size_t) n, sizeof(SEXP), strvec_cmp);
    int u = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        if (i == 0 || strcmp(CHAR(buf[i]), CHAR(buf[u-1])) != 0) {
            buf[u++] = buf[i];
        }
    }
    SEXP out = PROTECT(Rf_allocVector(STRSXP, u));
    for (int i = 0; i < u; i++) SET_STRING_ELT(out, i, buf[i]);
    UNPROTECT(1);
    return out;
}

static SEXP normalize_blacklist(SEXP x, const char *arg) {
    if (x == R_NilValue) return Rf_allocVector(STRSXP, 0);
    if (TYPEOF(x) != STRSXP) {
        Rf_error("`%s` must be NULL or a character vector", arg);
    }
    R_xlen_t n = Rf_xlength(x);
    SEXP keep = PROTECT(Rf_allocVector(STRSXP, n));
    int k = 0;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP s = STRING_ELT(x, i);
        if (s == NA_STRING) continue;
        const char *raw = CHAR(s);
        /* trim whitespace */
        const char *p = raw;
        while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
        const char *q = raw + strlen(raw);
        while (q > p && (q[-1] == ' ' || q[-1] == '\t' || q[-1] == '\n' || q[-1] == '\r')) q--;
        if (q == p) continue;
        int len = (int)(q - p);
        char *buf = (char*) R_alloc((size_t)(len + 1), sizeof(char));
        memcpy(buf, p, (size_t) len);
        buf[len] = '\0';
        SET_STRING_ELT(keep, k++, Rf_mkChar(buf));
    }
    SEXP trimmed = PROTECT(Rf_allocVector(STRSXP, k));
    for (int i = 0; i < k; i++) SET_STRING_ELT(trimmed, i, STRING_ELT(keep, i));
    SEXP res = sort_unique_strvec(trimmed);
    UNPROTECT(2);
    return res;
}

static SEXP user_blacklist(void) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("getOption"), R_BaseEnv));
    SEXP a = PROTECT(Rf_mkString("imputesrcref.wrap_arg_blacklist"));
    SEXP call = PROTECT(Rf_lang2(fn, a));
    SEXP v = PROTECT(Rf_eval(call, R_GlobalEnv));
    SEXP res = normalize_blacklist(v, "imputesrcref.wrap_arg_blacklist");
    UNPROTECT(4);
    return res;
}

static SEXP get_specialsxp(void) {
    if (specialsxp_cache == NULL) {
        SEXP v = compute_specialsxp_names();
        PROTECT(v);
        R_PreserveObject(v);
        UNPROTECT(1);
        specialsxp_cache = v;
    }
    return specialsxp_cache;
}

SEXP imputesrcref_effective_blacklist(void) {
    SEXP special = PROTECT(get_specialsxp());
    SEXP user = PROTECT(user_blacklist());

    R_xlen_t n1 = Rf_xlength(special);
    R_xlen_t n2 = (R_xlen_t) (sizeof(rlang_nse_names) / sizeof(rlang_nse_names[0]));
    R_xlen_t n3 = Rf_xlength(user);

    SEXP combined = PROTECT(Rf_allocVector(STRSXP, n1 + n2 + n3));
    R_xlen_t pos = 0;
    for (R_xlen_t i = 0; i < n1; i++) SET_STRING_ELT(combined, pos++, STRING_ELT(special, i));
    for (R_xlen_t i = 0; i < n2; i++) SET_STRING_ELT(combined, pos++, Rf_mkChar(rlang_nse_names[i]));
    for (R_xlen_t i = 0; i < n3; i++) SET_STRING_ELT(combined, pos++, STRING_ELT(user, i));

    SEXP res = sort_unique_strvec(combined);
    UNPROTECT(3);
    return res;
}

SEXP C_get_impute_blacklist(SEXP include_default) {
    if (TYPEOF(include_default) != LGLSXP || Rf_xlength(include_default) != 1 ||
        LOGICAL(include_default)[0] == NA_LOGICAL) {
        Rf_error("`include_default` must be TRUE or FALSE");
    }
    if (LOGICAL(include_default)[0]) {
        return imputesrcref_effective_blacklist();
    }
    return user_blacklist();
}

static void set_option(const char *name, SEXP value) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("options"), R_BaseEnv));
    SEXP call = PROTECT(Rf_lang2(fn, value));
    SET_TAG(CDR(call), Rf_install(name));
    Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
}

SEXP C_set_impute_blacklist(SEXP functions, SEXP append) {
    if (TYPEOF(append) != LGLSXP || Rf_xlength(append) != 1 ||
        LOGICAL(append)[0] == NA_LOGICAL) {
        Rf_error("`append` must be TRUE or FALSE");
    }
    int do_append = LOGICAL(append)[0];

    SEXP incoming = PROTECT(normalize_blacklist(functions, "functions"));
    SEXP current = R_NilValue;
    if (do_append) {
        current = PROTECT(user_blacklist());
    } else {
        current = PROTECT(Rf_allocVector(STRSXP, 0));
    }

    R_xlen_t n1 = Rf_xlength(current);
    R_xlen_t n2 = Rf_xlength(incoming);
    SEXP combined = PROTECT(Rf_allocVector(STRSXP, n1 + n2));
    R_xlen_t pos = 0;
    for (R_xlen_t i = 0; i < n1; i++) SET_STRING_ELT(combined, pos++, STRING_ELT(current, i));
    for (R_xlen_t i = 0; i < n2; i++) SET_STRING_ELT(combined, pos++, STRING_ELT(incoming, i));
    SEXP next_vals = PROTECT(sort_unique_strvec(combined));

    set_option("imputesrcref.wrap_arg_blacklist", next_vals);

    UNPROTECT(4);
    return next_vals;
}

SEXP C_reset_impute_blacklist(void) {
    set_option("imputesrcref.wrap_arg_blacklist", R_NilValue);
    return Rf_allocVector(STRSXP, 0);
}
