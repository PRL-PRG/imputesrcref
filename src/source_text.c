#include "imputesrcref.h"
#include <string.h>

int imputesrcref_quiet = 0;

int imputesrcref_visual_col_to_byte_col(const char *line, int visual_col) {
    int vis = 1;
    int i;
    int len = (int) strlen(line);
    for (i = 0; i < len; i++) {
        if (vis >= visual_col) return i + 1;
        if (line[i] == '\t') {
            vis = ((vis - 1) / 8 + 1) * 8 + 1;
        } else {
            vis++;
        }
    }
    return len;
}

static SEXP call_getSrcLines(SEXP srcfile, int start_line, int end_line) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("getSrcLines"), R_BaseEnv));
    SEXP a = PROTECT(Rf_ScalarInteger(start_line));
    SEXP b = PROTECT(Rf_ScalarInteger(end_line));
    SEXP call = PROTECT(Rf_lang4(fn, srcfile, a, b));
    SEXP res = R_tryEval(call, R_GlobalEnv, NULL);
    UNPROTECT(4);
    if (res == NULL) return R_NilValue;
    return res;
}

static SEXP call_srcfilecopy(const char *name, const char *txt) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("srcfilecopy"), R_BaseEnv));
    SEXP a = PROTECT(Rf_mkString(name));
    SEXP b = PROTECT(Rf_mkString(txt));
    SEXP call = PROTECT(Rf_lang3(fn, a, b));
    SEXP res = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(4);
    return res;
}

static SEXP call_deparse_500(SEXP x) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("deparse"), R_BaseEnv));
    SEXP w = PROTECT(Rf_ScalarInteger(500));
    SEXP call = PROTECT(Rf_lang3(fn, x, w));
    SET_TAG(CDDR(call), Rf_install("width.cutoff"));
    SEXP res = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return res;
}

static int get_option_logical(const char *name, int dflt) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("getOption"), R_BaseEnv));
    SEXP a = PROTECT(Rf_mkString(name));
    SEXP b = PROTECT(Rf_ScalarLogical(dflt));
    SEXP call = PROTECT(Rf_lang3(fn, a, b));
    SEXP res = PROTECT(Rf_eval(call, R_GlobalEnv));
    int out = dflt;
    if (TYPEOF(res) == LGLSXP && Rf_xlength(res) >= 1) {
        int v = LOGICAL(res)[0];
        if (v != NA_LOGICAL) out = (v ? 1 : 0);
    }
    UNPROTECT(5);
    return out;
}

static void emit_message(const char *txt) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("message"), R_BaseEnv));
    SEXP a = PROTECT(Rf_mkString(txt));
    SEXP call = PROTECT(Rf_lang2(fn, a));
    Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
}

static SEXP paste_lines(SEXP lines) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("paste"), R_BaseEnv));
    SEXP sep = PROTECT(Rf_mkString("\n"));
    SEXP call = PROTECT(Rf_lang3(fn, lines, sep));
    SET_TAG(CDDR(call), Rf_install("collapse"));
    SEXP res = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return res;
}

SEXP imputesrcref_source_text(SEXP fn) {
    SEXP sr = Rf_getAttrib(fn, Rf_install("srcref"));

    if (sr == R_NilValue) {
        int allow = get_option_logical("imputesrcref.allow_deparse_fallback", 0);
        if (!allow) {
            /* `imputesrcref_quiet` only suppresses the per-function warning
               during batch processing; it does NOT override the user's
               deparse-fallback preference. If the user enabled fallback they
               have already accepted that line numbers for no-srcref functions
               may shift to deparsed-source coordinates. */
            if (!imputesrcref_quiet) {
                emit_message(
                    "Function has no srcref metadata and deparse fallback is disabled; "
                    "no changes were made. Set "
                    "options(imputesrcref.allow_deparse_fallback = TRUE) to enable "
                    "deparse-based fallback.");
            }
            return R_NilValue;
        }
        SEXP dep = PROTECT(call_deparse_500(fn));
        SEXP collapsed = PROTECT(paste_lines(dep));
        const char *txt = CHAR(STRING_ELT(collapsed, 0));
        SEXP srcfile = PROTECT(call_srcfilecopy("<deparse>", txt));

        SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
        SEXP nms = PROTECT(Rf_allocVector(STRSXP, 4));
        SET_STRING_ELT(nms, 0, Rf_mkChar("text"));
        SET_STRING_ELT(nms, 1, Rf_mkChar("srcfile"));
        SET_STRING_ELT(nms, 2, Rf_mkChar("line_offset"));
        SET_STRING_ELT(nms, 3, Rf_mkChar("first_col_offset"));
        Rf_setAttrib(out, R_NamesSymbol, nms);
        SET_VECTOR_ELT(out, 0, collapsed);
        SET_VECTOR_ELT(out, 1, srcfile);
        SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(0));
        SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(0));
        UNPROTECT(5);
        return out;
    }

    SEXP srcfile = Rf_getAttrib(sr, Rf_install("srcfile"));
    if (srcfile == R_NilValue) Rf_error("Function srcref is missing srcfile");

    if (TYPEOF(sr) != INTSXP) {
        sr = PROTECT(Rf_coerceVector(sr, INTSXP));
    } else {
        PROTECT(sr);
    }

    int *p = INTEGER(sr);
    int n = (int) Rf_xlength(sr);
    int candidates[3][4];
    int ncand = 0;
    if (n >= 8) {
        candidates[ncand][0] = p[6]; candidates[ncand][1] = p[7];
        candidates[ncand][2] = p[4]; candidates[ncand][3] = p[5]; ncand++;
    }
    if (n >= 6) {
        candidates[ncand][0] = p[0]; candidates[ncand][1] = p[2];
        candidates[ncand][2] = p[4]; candidates[ncand][3] = p[5]; ncand++;
    }
    if (n >= 4) {
        candidates[ncand][0] = p[0]; candidates[ncand][1] = p[2];
        candidates[ncand][2] = p[1]; candidates[ncand][3] = p[3]; ncand++;
    }

    int chosen_start_line = 0, chosen_end_line = 0, chosen_start_col = 0, chosen_end_col = 0;
    SEXP lines = R_NilValue;

    for (int ci = 0; ci < ncand; ci++) {
        int sl = candidates[ci][0];
        int el = candidates[ci][1];
        int sc = candidates[ci][2];
        int ec = candidates[ci][3];
        if (sl == NA_INTEGER || el == NA_INTEGER || sc == NA_INTEGER || ec == NA_INTEGER) continue;
        if (sl <= 0 || el <= 0 || sc <= 0 || ec <= 0) continue;
        if (el < sl) continue;
        if (el == sl && ec < sc) continue;

        SEXP attempt = call_getSrcLines(srcfile, sl, el);
        if (attempt == R_NilValue || TYPEOF(attempt) != STRSXP || Rf_xlength(attempt) == 0) continue;

        chosen_start_line = sl;
        chosen_end_line = el;
        chosen_start_col = sc;
        chosen_end_col = ec;
        lines = attempt;
        break;
    }

    UNPROTECT(1);

    if (lines == R_NilValue) Rf_error("Could not read source lines for function");

    PROTECT(lines);
    R_xlen_t nlines = Rf_xlength(lines);
    SEXP new_lines = PROTECT(Rf_duplicate(lines));
    if (nlines == 1) {
        const char *l = CHAR(STRING_ELT(new_lines, 0));
        int byte_sc = imputesrcref_visual_col_to_byte_col(l, chosen_start_col);
        int byte_ec = imputesrcref_visual_col_to_byte_col(l, chosen_end_col);
        int len = (int) strlen(l);
        if (byte_sc < 1) byte_sc = 1;
        if (byte_ec > len) byte_ec = len;
        int slen = byte_ec - byte_sc + 1;
        if (slen < 0) slen = 0;
        char *buf = (char*) R_alloc((size_t)(slen + 1), sizeof(char));
        if (slen > 0) memcpy(buf, l + byte_sc - 1, (size_t) slen);
        buf[slen] = '\0';
        SET_STRING_ELT(new_lines, 0, Rf_mkChar(buf));
    } else {
        const char *l0 = CHAR(STRING_ELT(new_lines, 0));
        int byte_sc = imputesrcref_visual_col_to_byte_col(l0, chosen_start_col);
        int len0 = (int) strlen(l0);
        if (byte_sc < 1) byte_sc = 1;
        int slen0 = len0 - byte_sc + 1;
        if (slen0 < 0) slen0 = 0;
        char *buf0 = (char*) R_alloc((size_t)(slen0 + 1), sizeof(char));
        if (slen0 > 0) memcpy(buf0, l0 + byte_sc - 1, (size_t) slen0);
        buf0[slen0] = '\0';
        SET_STRING_ELT(new_lines, 0, Rf_mkChar(buf0));

        const char *ln = CHAR(STRING_ELT(new_lines, nlines - 1));
        int byte_ec = imputesrcref_visual_col_to_byte_col(ln, chosen_end_col);
        int lenN = (int) strlen(ln);
        if (byte_ec > lenN) byte_ec = lenN;
        int slenN = byte_ec;
        if (slenN < 0) slenN = 0;
        char *bufN = (char*) R_alloc((size_t)(slenN + 1), sizeof(char));
        if (slenN > 0) memcpy(bufN, ln, (size_t) slenN);
        bufN[slenN] = '\0';
        SET_STRING_ELT(new_lines, nlines - 1, Rf_mkChar(bufN));
    }

    SEXP collapsed = PROTECT(paste_lines(new_lines));

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP nms = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(nms, 0, Rf_mkChar("text"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("srcfile"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("line_offset"));
    SET_STRING_ELT(nms, 3, Rf_mkChar("first_col_offset"));
    Rf_setAttrib(out, R_NamesSymbol, nms);
    SET_VECTOR_ELT(out, 0, collapsed);
    SET_VECTOR_ELT(out, 1, srcfile);
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(chosen_start_line - 1));
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(chosen_start_col - 1));

    UNPROTECT(5);
    return out;
}
