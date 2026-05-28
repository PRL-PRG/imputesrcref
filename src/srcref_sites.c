#include "imputesrcref.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char **strs;
    int n;
    int cap;
} strbuf;

static void strbuf_init(strbuf *b) { b->strs = NULL; b->n = 0; b->cap = 0; }

static void strbuf_push(strbuf *b, const char *s) {
    if (b->n == b->cap) {
        int new_cap = b->cap == 0 ? 16 : b->cap * 2;
        char **new_strs = (char**) R_alloc((size_t) new_cap, sizeof(char*));
        if (b->strs && b->n > 0) memcpy(new_strs, b->strs, (size_t) b->n * sizeof(char*));
        b->strs = new_strs;
        b->cap = new_cap;
    }
    size_t len = strlen(s);
    char *copy = (char*) R_alloc(len + 1, sizeof(char));
    memcpy(copy, s, len + 1);
    b->strs[b->n++] = copy;
}

static SEXP strbuf_to_strsxp(const strbuf *b) {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, b->n));
    for (int i = 0; i < b->n; i++) SET_STRING_ELT(out, i, Rf_mkChar(b->strs[i]));
    UNPROTECT(1);
    return out;
}

static SEXP base_fn(const char *name) {
    return Rf_findFun(Rf_install(name), R_BaseEnv);
}

/* Wrap a SEXP value in `quote(...)` so that when the surrounding call is
   evaluated, the value is passed unchanged instead of being re-evaluated as
   a symbol lookup or function call. */
static SEXP quote_wrap(SEXP x) {
    return Rf_lang2(Rf_install("quote"), x);
}

static SEXP do_deparse_one_line(SEXP expr) {
    SEXP dep_fn = PROTECT(base_fn("deparse"));
    SEXP qx = PROTECT(quote_wrap(expr));
    SEXP w = PROTECT(Rf_ScalarInteger(500));
    SEXP call = PROTECT(Rf_lang3(dep_fn, qx, w));
    SET_TAG(CDDR(call), Rf_install("width.cutoff"));
    SEXP lines = PROTECT(Rf_eval(call, R_GlobalEnv));

    SEXP paste_fn = PROTECT(base_fn("paste"));
    SEXP sep = PROTECT(Rf_mkString(" "));
    SEXP call2 = PROTECT(Rf_lang3(paste_fn, lines, sep));
    SET_TAG(CDDR(call2), Rf_install("collapse"));
    SEXP txt = PROTECT(Rf_eval(call2, R_GlobalEnv));

    SEXP gsub_fn = PROTECT(base_fn("gsub"));
    SEXP pat = PROTECT(Rf_mkString("[[:space:]]+"));
    SEXP rep = PROTECT(Rf_mkString(" "));
    SEXP call3 = PROTECT(Rf_lang4(gsub_fn, pat, rep, txt));
    SEXP result = Rf_eval(call3, R_GlobalEnv);
    UNPROTECT(13);
    return result;
}

static void format_srcref_tuple(SEXP sr, char *buf, size_t buf_size) {
    if (sr == R_NilValue || TYPEOF(sr) != INTSXP) {
        snprintf(buf, buf_size, "NA");
        return;
    }
    R_xlen_t n = Rf_xlength(sr);
    int *p = INTEGER(sr);
    int pos = snprintf(buf, buf_size, "(");
    for (R_xlen_t i = 0; i < n; i++) {
        if (pos < 0 || (size_t) pos >= buf_size) break;
        int w = snprintf(buf + pos, buf_size - pos, "%s%d",
                          i == 0 ? "" : ",", p[i]);
        if (w < 0) break;
        pos += w;
    }
    if (pos >= 0 && (size_t) pos < buf_size) {
        snprintf(buf + pos, buf_size - pos, ")");
    }
}

static SEXP do_srcref_to_text(SEXP sr) {
    if (TYPEOF(sr) != INTSXP || Rf_xlength(sr) < 4) {
        return Rf_ScalarString(NA_STRING);
    }
    SEXP srcfile = Rf_getAttrib(sr, Rf_install("srcfile"));
    if (srcfile == R_NilValue) return Rf_ScalarString(NA_STRING);

    int *p = INTEGER(sr);
    int sl = p[0], sc = p[1], el = p[2], ec = p[3];

    SEXP gsl_fn = PROTECT(base_fn("getSrcLines"));
    SEXP a = PROTECT(Rf_ScalarInteger(sl));
    SEXP b = PROTECT(Rf_ScalarInteger(el));
    SEXP call = PROTECT(Rf_lang4(gsl_fn, srcfile, a, b));
    int err = 0;
    SEXP lines = PROTECT(R_tryEval(call, R_GlobalEnv, &err));
    if (err) {
        UNPROTECT(5);
        return Rf_ScalarString(NA_STRING);
    }
    R_xlen_t n = Rf_xlength(lines);
    if (n == 0) {
        UNPROTECT(5);
        return Rf_ScalarString(NA_STRING);
    }

    SEXP new_lines = PROTECT(Rf_duplicate(lines));
    if (n == 1) {
        const char *l = CHAR(STRING_ELT(new_lines, 0));
        int len = (int) strlen(l);
        int s = sc, e = ec;
        if (s < 1) s = 1;
        if (e > len) e = len;
        int slen = e - s + 1;
        if (slen < 0) slen = 0;
        char *buf = (char*) R_alloc((size_t)(slen + 1), sizeof(char));
        if (slen > 0) memcpy(buf, l + s - 1, (size_t) slen);
        buf[slen] = '\0';
        SET_STRING_ELT(new_lines, 0, Rf_mkChar(buf));
    } else {
        const char *l0 = CHAR(STRING_ELT(new_lines, 0));
        int len0 = (int) strlen(l0);
        int s = sc;
        if (s < 1) s = 1;
        int slen0 = len0 - s + 1;
        if (slen0 < 0) slen0 = 0;
        char *buf0 = (char*) R_alloc((size_t)(slen0 + 1), sizeof(char));
        if (slen0 > 0) memcpy(buf0, l0 + s - 1, (size_t) slen0);
        buf0[slen0] = '\0';
        SET_STRING_ELT(new_lines, 0, Rf_mkChar(buf0));

        const char *ln = CHAR(STRING_ELT(new_lines, n - 1));
        int lenN = (int) strlen(ln);
        int e = ec;
        if (e > lenN) e = lenN;
        int slenN = e;
        if (slenN < 0) slenN = 0;
        char *bufN = (char*) R_alloc((size_t)(slenN + 1), sizeof(char));
        if (slenN > 0) memcpy(bufN, ln, (size_t) slenN);
        bufN[slenN] = '\0';
        SET_STRING_ELT(new_lines, n - 1, Rf_mkChar(bufN));
    }

    SEXP paste_fn = PROTECT(base_fn("paste"));
    SEXP nl = PROTECT(Rf_mkString("\n"));
    SEXP call_p = PROTECT(Rf_lang3(paste_fn, new_lines, nl));
    SET_TAG(CDDR(call_p), Rf_install("collapse"));
    SEXP r = Rf_eval(call_p, R_GlobalEnv);
    UNPROTECT(9);
    return r;
}

static SEXP do_canonicalize_text(SEXP text) {
    if (TYPEOF(text) != STRSXP || Rf_xlength(text) < 1) {
        return Rf_ScalarString(NA_STRING);
    }
    if (STRING_ELT(text, 0) == NA_STRING) {
        return Rf_ScalarString(NA_STRING);
    }
    SEXP parse_fn = PROTECT(base_fn("parse"));
    SEXP keep = PROTECT(Rf_ScalarLogical(0));
    SEXP call = PROTECT(Rf_lang3(parse_fn, text, keep));
    SET_TAG(CDR(call), Rf_install("text"));
    SET_TAG(CDDR(call), Rf_install("keep.source"));
    int err = 0;
    SEXP parsed = R_tryEval(call, R_GlobalEnv, &err);
    if (err) { UNPROTECT(3); return Rf_ScalarString(NA_STRING); }
    PROTECT(parsed);
    if (Rf_xlength(parsed) != 1) {
        UNPROTECT(4);
        return Rf_ScalarString(NA_STRING);
    }
    SEXP single = VECTOR_ELT(parsed, 0);
    SEXP res = do_deparse_one_line(single);
    UNPROTECT(4);
    return res;
}

static SEXP do_transparent_srcref(SEXP node) {
    SEXP sr = Rf_getAttrib(node, Rf_install("srcref"));
    if (sr == R_NilValue) return R_NilValue;
    SEXP sr_list;
    int allocated_list = 0;
    if (TYPEOF(sr) == VECSXP) {
        sr_list = sr;
    } else {
        sr_list = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(sr_list, 0, sr);
        allocated_list = 1;
    }
    R_xlen_t n = Rf_xlength(sr_list);
    if (n < 2) {
        if (allocated_list) UNPROTECT(1);
        return R_NilValue;
    }
    SEXP a = VECTOR_ELT(sr_list, 0);
    SEXP b = VECTOR_ELT(sr_list, 1);

    SEXP id_fn = PROTECT(base_fn("identical"));
    SEXP qa = PROTECT(quote_wrap(a));
    SEXP qb = PROTECT(quote_wrap(b));
    SEXP call = PROTECT(Rf_lang3(id_fn, qa, qb));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    int eq = TYPEOF(r) == LGLSXP && LOGICAL(r)[0] == TRUE;
    UNPROTECT(4);
    if (allocated_list) UNPROTECT(1);
    return eq ? a : R_NilValue;
}

static SEXP do_strip_transparent_braces(SEXP expr) {
    if (TYPEOF(expr) != LANGSXP) return expr;

    if (imputesrcref_is_braced(expr)) {
        SEXP cdr = CDR(expr);
        if (cdr != R_NilValue) {
            SEXP tr_sr = do_transparent_srcref(expr);
            if (tr_sr != R_NilValue) {
                return do_strip_transparent_braces(CAR(cdr));
            }
        }
    }

    int n = 0;
    for (SEXP it = expr; it != R_NilValue; it = CDR(it)) n++;

    SEXP out = PROTECT(Rf_allocList(n));
    SET_TYPEOF(out, LANGSXP);
    SEXP src = expr;
    SEXP dst = out;
    while (src != R_NilValue) {
        SEXP new_v = PROTECT(do_strip_transparent_braces(CAR(src)));
        SETCAR(dst, new_v);
        SET_TAG(dst, TAG(src));
        UNPROTECT(1);
        src = CDR(src);
        dst = CDR(dst);
    }
    UNPROTECT(1);
    return out;
}

/* Path manipulation helpers --- writes into provided buffer and returns the
   new length (excluding terminator).  If the suffix would overflow, leaves
   the buffer unchanged and returns the original length. */
static int path_push(char *path, int path_len, int path_cap, const char *suffix) {
    int slen = (int) strlen(suffix);
    if (path_len + slen + 1 > path_cap) return path_len;
    memcpy(path + path_len, suffix, (size_t) slen);
    path[path_len + slen] = '\0';
    return path_len + slen;
}

static void emit_line(strbuf *b, const char *path, const char *dep,
                      const char *sr1, const char *sr2) {
    int needed = snprintf(NULL, 0, "path=%s node=%s sr1=%s sr2=%s",
                          path, dep, sr1, sr2);
    if (needed < 0) return;
    char *buf = (char*) R_alloc((size_t)(needed + 1), sizeof(char));
    snprintf(buf, (size_t)(needed + 1), "path=%s node=%s sr1=%s sr2=%s",
             path, dep, sr1, sr2);
    strbuf_push(b, buf);
}

static void emit_check(strbuf *lines, strbuf *failures, int *checked,
                       const char *path, int ok,
                       const char *sr_text_encoded,
                       const char *expr_text_encoded) {
    *checked = *checked + 1;
    int needed = snprintf(NULL, 0, "check path=%s ok=%s sr_text=%s expr_text=%s",
                          path, ok ? "TRUE" : "FALSE",
                          sr_text_encoded, expr_text_encoded);
    if (needed < 0) return;
    char *buf = (char*) R_alloc((size_t)(needed + 1), sizeof(char));
    snprintf(buf, (size_t)(needed + 1), "check path=%s ok=%s sr_text=%s expr_text=%s",
             path, ok ? "TRUE" : "FALSE", sr_text_encoded, expr_text_encoded);
    strbuf_push(lines, buf);
    if (!ok) strbuf_push(failures, path);
}

static SEXP encode_string_quote(SEXP s_strsxp) {
    SEXP fn = PROTECT(base_fn("encodeString"));
    SEXP q = PROTECT(Rf_mkString("\""));
    SEXP call = PROTECT(Rf_lang3(fn, s_strsxp, q));
    SET_TAG(CDDR(call), Rf_install("quote"));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return r;
}

/* ---- collect_srcref_sites walker ---- */
static void walk_sites(SEXP node, char *path, int path_len, int path_cap, strbuf *b) {
    if (TYPEOF(node) == LANGSXP) {
        if (imputesrcref_is_braced(node)) {
            SEXP sr = Rf_getAttrib(node, Rf_install("srcref"));
            if (sr != R_NilValue) {
                SEXP dep = PROTECT(do_deparse_one_line(node));
                const char *dep_str = (TYPEOF(dep) == STRSXP && Rf_xlength(dep) >= 1)
                                       ? CHAR(STRING_ELT(dep, 0)) : "";

                char sr1_buf[256], sr2_buf[256];
                if (TYPEOF(sr) == VECSXP) {
                    R_xlen_t n = Rf_xlength(sr);
                    if (n >= 1) format_srcref_tuple(VECTOR_ELT(sr, 0), sr1_buf, sizeof(sr1_buf));
                    else snprintf(sr1_buf, sizeof(sr1_buf), "NA");
                    if (n >= 2) format_srcref_tuple(VECTOR_ELT(sr, 1), sr2_buf, sizeof(sr2_buf));
                    else snprintf(sr2_buf, sizeof(sr2_buf), "NA");
                } else {
                    format_srcref_tuple(sr, sr1_buf, sizeof(sr1_buf));
                    snprintf(sr2_buf, sizeof(sr2_buf), "NA");
                }
                emit_line(b, path, dep_str, sr1_buf, sr2_buf);
                UNPROTECT(1);
            }
        }
        int i = 1;
        for (SEXP it = node; it != R_NilValue; it = CDR(it), i++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", i);
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_sites(CAR(it), path, new_len, path_cap, b);
            path[path_len] = '\0';
        }
        return;
    }
    if (TYPEOF(node) == EXPRSXP || TYPEOF(node) == VECSXP) {
        R_xlen_t n = Rf_xlength(node);
        for (R_xlen_t j = 0; j < n; j++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", (int)(j + 1));
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_sites(VECTOR_ELT(node, j), path, new_len, path_cap, b);
            path[path_len] = '\0';
        }
        return;
    }
    if (TYPEOF(node) == LISTSXP) {
        int i = 1;
        for (SEXP it = node; it != R_NilValue; it = CDR(it), i++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", i);
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_sites(CAR(it), path, new_len, path_cap, b);
            path[path_len] = '\0';
        }
    }
}

/* ---- collect_transparent_srcref_checks walker ---- */
static void walk_checks(SEXP node, char *path, int path_len, int path_cap,
                        strbuf *lines, strbuf *failures, int *checked) {
    if (TYPEOF(node) == LANGSXP) {
        if (imputesrcref_is_braced(node)) {
            SEXP tr_sr = do_transparent_srcref(node);
            if (tr_sr != R_NilValue && CDR(node) != R_NilValue) {
                PROTECT(tr_sr);
                SEXP sr_text = PROTECT(do_srcref_to_text(tr_sr));
                SEXP sr_canon = PROTECT(do_canonicalize_text(sr_text));
                SEXP stripped = PROTECT(do_strip_transparent_braces(CADR(node)));
                SEXP expr_text = PROTECT(do_deparse_one_line(stripped));

                int ok = 0;
                if (TYPEOF(sr_canon) == STRSXP && Rf_xlength(sr_canon) >= 1 &&
                    STRING_ELT(sr_canon, 0) != NA_STRING &&
                    TYPEOF(expr_text) == STRSXP && Rf_xlength(expr_text) >= 1) {
                    const char *a = CHAR(STRING_ELT(sr_canon, 0));
                    const char *b = CHAR(STRING_ELT(expr_text, 0));
                    ok = strcmp(a, b) == 0;
                }

                SEXP sr_enc = PROTECT(encode_string_quote(sr_canon));
                SEXP exp_enc = PROTECT(encode_string_quote(expr_text));
                const char *sa = (TYPEOF(sr_enc) == STRSXP && Rf_xlength(sr_enc) >= 1)
                                  ? CHAR(STRING_ELT(sr_enc, 0)) : "NA";
                const char *eb = (TYPEOF(exp_enc) == STRSXP && Rf_xlength(exp_enc) >= 1)
                                  ? CHAR(STRING_ELT(exp_enc, 0)) : "NA";
                emit_check(lines, failures, checked, path, ok, sa, eb);
                UNPROTECT(7);
            }
        }
        int i = 1;
        for (SEXP it = node; it != R_NilValue; it = CDR(it), i++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", i);
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_checks(CAR(it), path, new_len, path_cap, lines, failures, checked);
            path[path_len] = '\0';
        }
        return;
    }
    if (TYPEOF(node) == EXPRSXP || TYPEOF(node) == VECSXP) {
        R_xlen_t n = Rf_xlength(node);
        for (R_xlen_t j = 0; j < n; j++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", (int)(j + 1));
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_checks(VECTOR_ELT(node, j), path, new_len, path_cap, lines, failures, checked);
            path[path_len] = '\0';
        }
        return;
    }
    if (TYPEOF(node) == LISTSXP) {
        int i = 1;
        for (SEXP it = node; it != R_NilValue; it = CDR(it), i++) {
            char suffix[32];
            snprintf(suffix, sizeof(suffix), "/%d", i);
            int new_len = path_push(path, path_len, path_cap, suffix);
            walk_checks(CAR(it), path, new_len, path_cap, lines, failures, checked);
            path[path_len] = '\0';
        }
    }
}

static SEXP call_formals(SEXP fn) {
    SEXP f = PROTECT(base_fn("formals"));
    SEXP call = PROTECT(Rf_lang2(f, fn));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return r;
}

static SEXP call_body(SEXP fn) {
    SEXP f = PROTECT(base_fn("body"));
    SEXP call = PROTECT(Rf_lang2(f, fn));
    SEXP r = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return r;
}

static int is_function_kind(SEXP x) {
    return TYPEOF(x) == CLOSXP || TYPEOF(x) == BUILTINSXP || TYPEOF(x) == SPECIALSXP;
}

/* Common helper: build path "formals/<name>" for each formal with non-missing
   default, or "body" for the body — calls the given walker on each. */
typedef void (*walker_fn)(SEXP, char *, int, int, void*);

static void walk_function_struct_sites(SEXP fn, char *path, int path_cap, strbuf *b) {
    SEXP fmls = PROTECT(call_formals(fn));
    int idx = 1;
    for (SEXP cell = fmls; cell != R_NilValue; cell = CDR(cell), idx++) {
        SEXP val = CAR(cell);
        if (val == R_MissingArg) continue;
        SEXP tag = TAG(cell);
        const char *nm;
        char idx_str[32];
        if (tag != R_NilValue && TYPEOF(tag) == SYMSXP &&
            CHAR(PRINTNAME(tag))[0] != '\0') {
            nm = CHAR(PRINTNAME(tag));
        } else {
            snprintf(idx_str, sizeof(idx_str), "%d", idx);
            nm = idx_str;
        }
        int plen = snprintf(path, path_cap, "formals/%s", nm);
        if (plen < 0 || plen >= path_cap) plen = path_cap - 1;
        walk_sites(val, path, plen, path_cap, b);
    }
    UNPROTECT(1);
    int plen = snprintf(path, path_cap, "body");
    SEXP body = PROTECT(call_body(fn));
    walk_sites(body, path, plen, path_cap, b);
    UNPROTECT(1);
}

static void walk_function_struct_checks(SEXP fn, char *path, int path_cap,
                                        strbuf *lines, strbuf *failures, int *checked) {
    SEXP fmls = PROTECT(call_formals(fn));
    int idx = 1;
    for (SEXP cell = fmls; cell != R_NilValue; cell = CDR(cell), idx++) {
        SEXP val = CAR(cell);
        if (val == R_MissingArg) continue;
        SEXP tag = TAG(cell);
        const char *nm;
        char idx_str[32];
        if (tag != R_NilValue && TYPEOF(tag) == SYMSXP &&
            CHAR(PRINTNAME(tag))[0] != '\0') {
            nm = CHAR(PRINTNAME(tag));
        } else {
            snprintf(idx_str, sizeof(idx_str), "%d", idx);
            nm = idx_str;
        }
        int plen = snprintf(path, path_cap, "formals/%s", nm);
        if (plen < 0 || plen >= path_cap) plen = path_cap - 1;
        walk_checks(val, path, plen, path_cap, lines, failures, checked);
    }
    UNPROTECT(1);
    int plen = snprintf(path, path_cap, "body");
    SEXP body = PROTECT(call_body(fn));
    walk_checks(body, path, plen, path_cap, lines, failures, checked);
    UNPROTECT(1);
}

SEXP C_collect_srcref_sites(SEXP expr_or_fn) {
    strbuf b; strbuf_init(&b);
    enum { PATH_CAP = 8192 };
    char path[PATH_CAP];
    if (is_function_kind(expr_or_fn)) {
        walk_function_struct_sites(expr_or_fn, path, PATH_CAP, &b);
    } else {
        int plen = snprintf(path, PATH_CAP, "root");
        walk_sites(expr_or_fn, path, plen, PATH_CAP, &b);
    }
    return strbuf_to_strsxp(&b);
}

SEXP C_collect_transparent_srcref_checks(SEXP expr_or_fn) {
    strbuf lines; strbuf_init(&lines);
    strbuf failures; strbuf_init(&failures);
    int checked = 0;
    enum { PATH_CAP = 8192 };
    char path[PATH_CAP];
    if (is_function_kind(expr_or_fn)) {
        walk_function_struct_checks(expr_or_fn, path, PATH_CAP, &lines, &failures, &checked);
    } else {
        int plen = snprintf(path, PATH_CAP, "root");
        walk_checks(expr_or_fn, path, plen, PATH_CAP, &lines, &failures, &checked);
    }

    SEXP lines_sx = PROTECT(strbuf_to_strsxp(&lines));
    SEXP failures_sx = PROTECT(strbuf_to_strsxp(&failures));
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP nms = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(nms, 0, Rf_mkChar("checked"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("ok"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("failures"));
    SET_STRING_ELT(nms, 3, Rf_mkChar("lines"));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(checked));
    SET_VECTOR_ELT(out, 1, Rf_ScalarLogical(failures.n == 0));
    SET_VECTOR_ELT(out, 2, failures_sx);
    SET_VECTOR_ELT(out, 3, lines_sx);
    UNPROTECT(4);
    return out;
}

SEXP C_assert_transparent_srcref_consistency(SEXP expr_or_fn) {
    SEXP checks = PROTECT(C_collect_transparent_srcref_checks(expr_or_fn));
    int ok = LOGICAL(VECTOR_ELT(checks, 1))[0];
    if (!ok) {
        SEXP fails = VECTOR_ELT(checks, 2);
        R_xlen_t nf = Rf_xlength(fails);
        size_t total = 64;
        for (R_xlen_t i = 0; i < nf; i++) total += strlen(CHAR(STRING_ELT(fails, i))) + 2;
        char *buf = (char*) R_alloc(total, sizeof(char));
        int pos = snprintf(buf, total, "Transparent srcref text mismatch at: ");
        for (R_xlen_t i = 0; i < nf; i++) {
            if (i > 0) pos += snprintf(buf + pos, total - (size_t) pos, ", ");
            pos += snprintf(buf + pos, total - (size_t) pos, "%s", CHAR(STRING_ELT(fails, i)));
        }
        UNPROTECT(1);
        Rf_error("%s", buf);
    }
    UNPROTECT(1);
    return checks;
}
