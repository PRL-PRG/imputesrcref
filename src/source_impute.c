#include "imputesrcref.h"
#include <string.h>
#include <stdio.h>

static SEXP base_fun(const char *name) {
    return Rf_findFun(Rf_install(name), R_BaseEnv);
}

static SEXP one_arg_call(SEXP fn, SEXP a1) {
    SEXP c = PROTECT(Rf_lang2(fn, a1));
    SEXP r = Rf_eval(c, R_GlobalEnv);
    UNPROTECT(1);
    return r;
}

static SEXP two_arg_call(SEXP fn, SEXP a1, SEXP a2) {
    SEXP c = PROTECT(Rf_lang3(fn, a1, a2));
    SEXP r = Rf_eval(c, R_GlobalEnv);
    UNPROTECT(1);
    return r;
}

static SEXP ls_env(SEXP envir, int all_names) {
    SEXP fn = PROTECT(base_fun("ls"));
    SEXP a = PROTECT(Rf_ScalarLogical(all_names));
    SEXP call = PROTECT(Rf_lang3(fn, envir, a));
    SET_TAG(CDDR(call), Rf_install("all.names"));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return r;
}

static int exists_in(const char *name, SEXP envir) {
    SEXP fn = PROTECT(base_fun("exists"));
    SEXP nm = PROTECT(Rf_mkString(name));
    SEXP inh = PROTECT(Rf_ScalarLogical(0));
    SEXP call = PROTECT(Rf_lang4(fn, nm, envir, inh));
    SET_TAG(CDDR(call), Rf_install("envir"));
    SET_TAG(CDR(CDDR(call)), Rf_install("inherits"));
    SEXP r = PROTECT(Rf_eval(call, R_GlobalEnv));
    int v = LOGICAL(r)[0] == TRUE;
    UNPROTECT(5);
    return v;
}

static SEXP get_in(const char *name, SEXP envir) {
    SEXP fn = PROTECT(base_fun("get"));
    SEXP nm = PROTECT(Rf_mkString(name));
    SEXP inh = PROTECT(Rf_ScalarLogical(0));
    SEXP call = PROTECT(Rf_lang4(fn, nm, envir, inh));
    SET_TAG(CDDR(call), Rf_install("envir"));
    SET_TAG(CDR(CDDR(call)), Rf_install("inherits"));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(4);
    return r;
}

static int identical_(SEXP a, SEXP b) {
    SEXP fn = PROTECT(base_fun("identical"));
    SEXP qa = PROTECT(Rf_lang2(Rf_install("quote"), a));
    SEXP qb = PROTECT(Rf_lang2(Rf_install("quote"), b));
    SEXP call = PROTECT(Rf_lang3(fn, qa, qb));
    SEXP r = PROTECT(Rf_eval(call, R_GlobalEnv));
    int v = TYPEOF(r) == LGLSXP && LOGICAL(r)[0] == TRUE;
    UNPROTECT(5);
    return v;
}

static void assign_in(const char *name, SEXP value, SEXP envir) {
    SEXP fn = PROTECT(base_fun("assign"));
    SEXP nm = PROTECT(Rf_mkString(name));
    SEXP call = PROTECT(Rf_lang4(fn, nm, value, envir));
    SET_TAG(CDR(CDDR(call)), Rf_install("envir"));
    Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
}

static int binding_is_locked(const char *name, SEXP envir) {
    SEXP fn = PROTECT(base_fun("bindingIsLocked"));
    SEXP nm = PROTECT(Rf_mkString(name));
    SEXP r = PROTECT(two_arg_call(fn, nm, envir));
    int v = TYPEOF(r) == LGLSXP && LOGICAL(r)[0] == TRUE;
    UNPROTECT(3);
    return v;
}

static void unlock_binding_(const char *name, SEXP envir) {
    SEXP fn = PROTECT(base_fun("unlockBinding"));
    SEXP nm = PROTECT(Rf_mkString(name));
    two_arg_call(fn, nm, envir);
    UNPROTECT(2);
}

static void lock_binding_(const char *name, SEXP envir) {
    SEXP fn = PROTECT(base_fun("lockBinding"));
    SEXP nm = PROTECT(Rf_mkString(name));
    two_arg_call(fn, nm, envir);
    UNPROTECT(2);
}

static void emit_warning(const char *txt) {
    SEXP fn = PROTECT(base_fun("warning"));
    SEXP nm = PROTECT(Rf_mkString(txt));
    SEXP cf = PROTECT(Rf_ScalarLogical(0));
    SEXP call = PROTECT(Rf_lang3(fn, nm, cf));
    SET_TAG(CDDR(call), Rf_install("call."));
    Rf_eval(call, R_GlobalEnv);
    UNPROTECT(4);
}

static void emit_message(const char *txt) {
    SEXP fn = PROTECT(base_fun("message"));
    SEXP nm = PROTECT(Rf_mkString(txt));
    one_arg_call(fn, nm);
    UNPROTECT(2);
}

static void sys_source_(SEXP file, SEXP envir, SEXP chdir, SEXP keep_source,
                        SEXP keep_parse_data, SEXP toplevel_env) {
    SEXP fn = PROTECT(base_fun("sys.source"));
    SEXP call = PROTECT(Rf_allocList(7));
    SET_TYPEOF(call, LANGSXP);
    SETCAR(call, fn);
    SEXP c1 = CDR(call); SETCAR(c1, file); SET_TAG(c1, Rf_install("file"));
    SEXP c2 = CDR(c1);   SETCAR(c2, envir); SET_TAG(c2, Rf_install("envir"));
    SEXP c3 = CDR(c2);   SETCAR(c3, chdir); SET_TAG(c3, Rf_install("chdir"));
    SEXP c4 = CDR(c3);   SETCAR(c4, keep_source); SET_TAG(c4, Rf_install("keep.source"));
    SEXP c5 = CDR(c4);   SETCAR(c5, keep_parse_data); SET_TAG(c5, Rf_install("keep.parse.data"));
    SEXP c6 = CDR(c5);   SETCAR(c6, toplevel_env); SET_TAG(c6, Rf_install("toplevel.env"));
    Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
}

static SEXP normalize_path_(SEXP file) {
    SEXP fn = PROTECT(base_fun("normalizePath"));
    SEXP mw = PROTECT(Rf_ScalarLogical(0));
    SEXP call = PROTECT(Rf_lang3(fn, file, mw));
    SET_TAG(CDDR(call), Rf_install("mustWork"));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return r;
}

static int file_exists_(SEXP file) {
    SEXP fn = PROTECT(base_fun("file.exists"));
    SEXP r = PROTECT(one_arg_call(fn, file));
    int v = TYPEOF(r) == LGLSXP && LOGICAL(r)[0] == TRUE;
    UNPROTECT(2);
    return v;
}

static SEXP load_namespace_(const char *pkg) {
    SEXP fn = PROTECT(base_fun("loadNamespace"));
    SEXP nm = PROTECT(Rf_mkString(pkg));
    SEXP r = one_arg_call(fn, nm);
    UNPROTECT(2);
    return r;
}

static int is_primitive_(SEXP x) {
    return TYPEOF(x) == BUILTINSXP || TYPEOF(x) == SPECIALSXP;
}

static SEXP sort_unique_strvec(SEXP x) {
    SEXP unique_fn = PROTECT(base_fun("unique"));
    SEXP u = PROTECT(one_arg_call(unique_fn, x));
    SEXP sort_fn = PROTECT(base_fun("sort"));
    SEXP r = one_arg_call(sort_fn, u);
    UNPROTECT(3);
    return r;
}

typedef struct {
    SEXP fn;
} impute_body_data;

static SEXP impute_body(void *data) {
    impute_body_data *bd = (impute_body_data*) data;
    return C_impute_srcrefs(bd->fn, Rf_ScalarLogical(1));
}

static char err_msg_buf[2048];

static SEXP impute_handler(SEXP cond, void *data) {
    (void) data;
    SEXP cm_fn = PROTECT(base_fun("conditionMessage"));
    SEXP call = PROTECT(Rf_lang2(cm_fn, cond));
    SEXP msg = PROTECT(Rf_eval(call, R_GlobalEnv));
    const char *m = (TYPEOF(msg) == STRSXP && Rf_xlength(msg) >= 1)
                    ? CHAR(STRING_ELT(msg, 0)) : "<error>";
    strncpy(err_msg_buf, m, sizeof(err_msg_buf) - 1);
    err_msg_buf[sizeof(err_msg_buf) - 1] = '\0';
    UNPROTECT(3);
    return R_NilValue;
}

static SEXP try_impute_srcrefs(SEXP fn, int *err, char *msg_out, size_t msg_size) {
    err_msg_buf[0] = '\0';
    impute_body_data bd = { fn };
    SEXP res = R_tryCatchError(impute_body, &bd, impute_handler, NULL);
    if (err_msg_buf[0] != '\0') {
        *err = 1;
        if (msg_out && msg_size > 0) {
            strncpy(msg_out, err_msg_buf, msg_size - 1);
            msg_out[msg_size - 1] = '\0';
        }
        return R_NilValue;
    }
    *err = 0;
    return res;
}

SEXP C_source_impute_srcrefs(SEXP file, SEXP envir, SEXP chdir,
                              SEXP keep_source, SEXP keep_parse_data,
                              SEXP toplevel_env, SEXP all_names) {
    if (TYPEOF(file) != STRSXP || Rf_xlength(file) != 1 ||
        STRING_ELT(file, 0) == NA_STRING || CHAR(STRING_ELT(file, 0))[0] == '\0') {
        Rf_error("`file` must be a single non-empty path string");
    }
    if (!file_exists_(file)) {
        Rf_error("File does not exist: %s", CHAR(STRING_ELT(file, 0)));
    }
    if (TYPEOF(envir) != ENVSXP) {
        Rf_error("`envir` must be an environment");
    }
    int all = (TYPEOF(all_names) == LGLSXP && Rf_xlength(all_names) >= 1)
              ? LOGICAL(all_names)[0] : 1;

    SEXP pre_names = PROTECT(ls_env(envir, all));
    R_xlen_t n_pre = Rf_xlength(pre_names);
    SEXP pre_values = PROTECT(Rf_allocVector(VECSXP, n_pre));
    for (R_xlen_t i = 0; i < n_pre; i++) {
        const char *nm = CHAR(STRING_ELT(pre_names, i));
        if (exists_in(nm, envir)) {
            SET_VECTOR_ELT(pre_values, i, get_in(nm, envir));
        }
    }

    sys_source_(file, envir, chdir, keep_source, keep_parse_data, toplevel_env);

    SEXP post_names = PROTECT(ls_env(envir, all));
    R_xlen_t n_post = Rf_xlength(post_names);

    SEXP imputed = PROTECT(Rf_allocVector(STRSXP, n_post));
    int n_imputed = 0;

    int saved_quiet = imputesrcref_quiet;
    imputesrcref_quiet = 1;

    for (R_xlen_t i = 0; i < n_post; i++) {
        const char *nm = CHAR(STRING_ELT(post_names, i));
        if (!exists_in(nm, envir)) continue;
        SEXP value = PROTECT(get_in(nm, envir));
        if (!Rf_isFunction(value)) { UNPROTECT(1); continue; }

        int in_pre = 0;
        R_xlen_t pre_idx = -1;
        for (R_xlen_t j = 0; j < n_pre; j++) {
            if (strcmp(CHAR(STRING_ELT(pre_names, j)), nm) == 0) {
                in_pre = 1;
                pre_idx = j;
                break;
            }
        }
        int changed = !in_pre || !identical_(value, VECTOR_ELT(pre_values, pre_idx));
        if (!changed) { UNPROTECT(1); continue; }

        if (binding_is_locked(nm, envir)) {
            char msg[512];
            snprintf(msg, sizeof(msg), "Skipping locked binding `%s`", nm);
            emit_warning(msg);
            UNPROTECT(1);
            continue;
        }

        int err = 0;
        SEXP imputed_fn = PROTECT(try_impute_srcrefs(value, &err, NULL, 0));
        if (err) { UNPROTECT(2); continue; }
        assign_in(nm, imputed_fn, envir);
        SET_STRING_ELT(imputed, n_imputed++, STRING_ELT(post_names, i));
        UNPROTECT(2);
    }

    imputesrcref_quiet = saved_quiet;

    SEXP imputed_trim = PROTECT(Rf_allocVector(STRSXP, n_imputed));
    for (int i = 0; i < n_imputed; i++) SET_STRING_ELT(imputed_trim, i, STRING_ELT(imputed, i));

    SEXP final_names = PROTECT(sort_unique_strvec(imputed_trim));
    SEXP normalized = PROTECT(normalize_path_(file));

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 6));
    SEXP nms = PROTECT(Rf_allocVector(STRSXP, 6));
    SET_STRING_ELT(nms, 0, Rf_mkChar("file"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("envir"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("functions"));
    SET_STRING_ELT(nms, 3, Rf_mkChar("count"));
    SET_STRING_ELT(nms, 4, Rf_mkChar("keep.source"));
    SET_STRING_ELT(nms, 5, Rf_mkChar("keep.parse.data"));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    SET_VECTOR_ELT(out, 0, normalized);
    SET_VECTOR_ELT(out, 1, envir);
    SET_VECTOR_ELT(out, 2, final_names);
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger((int) Rf_xlength(final_names)));
    SET_VECTOR_ELT(out, 4, keep_source);
    SET_VECTOR_ELT(out, 5, keep_parse_data);

    UNPROTECT(9);
    return out;
}

SEXP C_impute_package_srcrefs(SEXP package, SEXP include_internal, SEXP verbose) {
    if (TYPEOF(package) != STRSXP || Rf_xlength(package) != 1 ||
        STRING_ELT(package, 0) == NA_STRING || CHAR(STRING_ELT(package, 0))[0] == '\0') {
        Rf_error("`package` must be a single non-empty package name");
    }
    int incl = (TYPEOF(include_internal) == LGLSXP && Rf_xlength(include_internal) >= 1)
                ? LOGICAL(include_internal)[0] : 1;
    int verb = (TYPEOF(verbose) == LGLSXP && Rf_xlength(verbose) >= 1)
                ? LOGICAL(verbose)[0] : 1;

    const char *pkg_name = CHAR(STRING_ELT(package, 0));
    SEXP env = PROTECT(load_namespace_(pkg_name));

    SEXP nms = PROTECT(ls_env(env, incl));
    R_xlen_t nn = Rf_xlength(nms);

    SEXP fn_names = PROTECT(Rf_allocVector(STRSXP, nn));
    int nfn = 0;
    for (R_xlen_t i = 0; i < nn; i++) {
        const char *nm = CHAR(STRING_ELT(nms, i));
        if (!exists_in(nm, env)) continue;
        SEXP obj = PROTECT(get_in(nm, env));
        if (Rf_isFunction(obj) && !is_primitive_(obj)) {
            SET_STRING_ELT(fn_names, nfn++, STRING_ELT(nms, i));
        }
        UNPROTECT(1);
    }
    SEXP fn_trim = PROTECT(Rf_allocVector(STRSXP, nfn));
    for (int i = 0; i < nfn; i++) SET_STRING_ELT(fn_trim, i, STRING_ELT(fn_names, i));
    SEXP final_names = PROTECT(sort_unique_strvec(fn_trim));

    R_xlen_t fn_n = Rf_xlength(final_names);
    SEXP failed = PROTECT(Rf_allocVector(STRSXP, fn_n));
    for (R_xlen_t i = 0; i < fn_n; i++) SET_STRING_ELT(failed, i, NA_STRING);

    int saved_quiet = imputesrcref_quiet;
    imputesrcref_quiet = 1;

    for (R_xlen_t i = 0; i < fn_n; i++) {
        const char *nm = CHAR(STRING_ELT(final_names, i));
        SEXP fn = PROTECT(get_in(nm, env));

        char err_msg[2048];
        err_msg[0] = '\0';
        int err = 0;
        SEXP patched = PROTECT(try_impute_srcrefs(fn, &err, err_msg, sizeof(err_msg)));
        if (err) {
            SET_STRING_ELT(failed, i, Rf_mkChar(err_msg[0] ? err_msg : "error"));
            UNPROTECT(2);
            continue;
        }

        /* impute returned the input unchanged when neither srcref nor parse
           data + deparse fallback were available — nothing to commit. */
        if (patched == fn) {
            SET_STRING_ELT(failed, i, Rf_mkChar("no srcref"));
            UNPROTECT(2);
            continue;
        }

        int locked = binding_is_locked(nm, env);
        if (locked) unlock_binding_(nm, env);
        assign_in(nm, patched, env);
        if (locked) lock_binding_(nm, env);

        UNPROTECT(2);
    }

    imputesrcref_quiet = saved_quiet;

    int patched_cnt = 0;
    for (R_xlen_t i = 0; i < fn_n; i++) {
        if (STRING_ELT(failed, i) == NA_STRING) patched_cnt++;
    }

    SEXP report = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP rnms = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(rnms, 0, Rf_mkChar("package"));
    SET_STRING_ELT(rnms, 1, Rf_mkChar("fn_names"));
    SET_STRING_ELT(rnms, 2, Rf_mkChar("failed"));
    SET_STRING_ELT(rnms, 3, Rf_mkChar("patched_count"));
    Rf_setAttrib(report, R_NamesSymbol, rnms);
    SET_VECTOR_ELT(report, 0, package);
    SET_VECTOR_ELT(report, 1, final_names);
    SET_VECTOR_ELT(report, 2, failed);
    SET_VECTOR_ELT(report, 3, Rf_ScalarInteger(patched_cnt));

    if (verb) {
        char msg[512];
        snprintf(msg, sizeof(msg), "Patched %d/%d function(s) in package `%s`.",
                 patched_cnt, (int) fn_n, pkg_name);
        emit_message(msg);
    }

    UNPROTECT(8);
    return report;
}
