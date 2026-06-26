#include "imputesrcref.h"
#include <string.h>

static SEXP cell_at(SEXP x, int i) {
    while (i > 1 && x != R_NilValue) {
        x = CDR(x);
        i--;
    }
    return x;
}

static int call_length_cons(SEXP x) {
    int n = 0;
    while (x != R_NilValue) {
        n++;
        x = CDR(x);
    }
    return n;
}

static const char *call_op_name(SEXP expr) {
    if (TYPEOF(expr) != LANGSXP) return "";
    SEXP h = CAR(expr);
    if (TYPEOF(h) != SYMSXP) return "";
    return CHAR(PRINTNAME(h));
}

static int find_forcond_id(int node_id, parse_ctx *ctx) {
    if (node_id < 0 || node_id > ctx->max_id) return -1;
    int offs = ctx->all_children_offset[node_id];
    int cnt = ctx->all_children_count[node_id];
    for (int i = 0; i < cnt; i++) {
        int row = ctx->all_children_data[offs + i];
        if (strcmp(CHAR(STRING_ELT(ctx->token, row)), "forcond") == 0) {
            return ctx->id[row];
        }
    }
    return -1;
}

static int first_expr_under(int parent_id, parse_ctx *ctx) {
    if (parent_id < 0 || parent_id > ctx->max_id) return -1;
    int cnt = ctx->expr_children_count[parent_id];
    if (cnt == 0) return -1;
    int offs = ctx->expr_children_offset[parent_id];
    return ctx->id[ctx->expr_children_data[offs]];
}

static int node_has_token(int node_id, const char *token, parse_ctx *ctx) {
    if (node_id < 0 || node_id > ctx->max_id) return 0;
    int offs = ctx->all_children_offset[node_id];
    int cnt = ctx->all_children_count[node_id];
    for (int i = 0; i < cnt; i++) {
        int row = ctx->all_children_data[offs + i];
        if (strcmp(CHAR(STRING_ELT(ctx->token, row)), token) == 0) return 1;
    }
    return 0;
}

static int *expr_children_ids(int node_id, parse_ctx *ctx, int *out_count) {
    if (node_id < 0 || node_id > ctx->max_id) { *out_count = 0; return NULL; }
    int cnt = ctx->expr_children_count[node_id];
    int offs = ctx->expr_children_offset[node_id];
    int *arr = (int*) R_alloc((size_t)(cnt > 0 ? cnt : 1), sizeof(int));
    for (int i = 0; i < cnt; i++) {
        arr[i] = ctx->id[ctx->expr_children_data[offs + i]];
    }
    *out_count = cnt;
    return arr;
}

static int callee_blacklisted(SEXP arg, parse_ctx *ctx) {
    if (TYPEOF(arg) != LANGSXP) return 0;
    SEXP head = CAR(arg);
    if (TYPEOF(head) != SYMSXP) return 0;
    return imputesrcref_in_strvec(ctx->arg_wrap_blacklist, CHAR(PRINTNAME(head)));
}

static SEXP recurse_and_maybe_wrap(SEXP value_expr, int child_id, parse_ctx *ctx, int wrap) {
    PROTECT(value_expr);
    SEXP transformed = PROTECT(imputesrcref_transform_expr(value_expr, child_id, ctx));
    int blacklisted = callee_blacklisted(value_expr, ctx);
    SEXP result;
    if (wrap && !imputesrcref_is_braced(transformed) && !blacklisted) {
        SEXP sr = PROTECT(imputesrcref_node_srcref(child_id, ctx));
        result = imputesrcref_wrap_brace(transformed, sr);
        UNPROTECT(1);
    } else {
        result = transformed;
    }
    UNPROTECT(2);
    return result;
}

typedef struct {
    int *indices;
    int *ids;
    int n;
    int valid;
} mapping_t;

static mapping_t map_generic_indices(SEXP expr, int *child_ids, int n_ids, int drop_first) {
    mapping_t m;
    m.valid = 0;
    m.indices = NULL;
    m.ids = NULL;
    m.n = 0;

    if (drop_first && n_ids > 0) {
        child_ids++;
        n_ids--;
    }

    int n_parts = call_length_cons(expr);

    int *present = (int*) R_alloc((size_t) n_parts, sizeof(int));
    int n_present = 0;
    SEXP it = expr;
    for (int i = 1; i <= n_parts; i++, it = CDR(it)) {
        if (!imputesrcref_is_missing_arg(CAR(it))) {
            present[n_present++] = i;
        }
    }

    if (n_ids == n_present) {
        m.indices = present;
        m.n = n_present;
        m.ids = child_ids;
        m.valid = 1;
        return m;
    }
    if (n_present >= 1 && n_ids == n_present - 1) {
        m.indices = (int*) R_alloc((size_t)(n_present - 1), sizeof(int));
        memcpy(m.indices, present + 1, sizeof(int) * (size_t)(n_present - 1));
        m.n = n_present - 1;
        m.ids = child_ids;
        m.valid = 1;
        return m;
    }
    if (n_ids == n_parts) {
        int *idx = (int*) R_alloc((size_t) n_parts, sizeof(int));
        for (int i = 0; i < n_parts; i++) idx[i] = i + 1;
        m.indices = idx;
        m.n = n_parts;
        m.ids = child_ids;
        m.valid = 1;
        return m;
    }
    if (n_ids == n_parts - 1 && n_parts >= 2) {
        int *idx = (int*) R_alloc((size_t)(n_parts - 1), sizeof(int));
        for (int i = 0; i < n_parts - 1; i++) idx[i] = i + 2;
        m.indices = idx;
        m.n = n_parts - 1;
        m.ids = child_ids;
        m.valid = 1;
        return m;
    }
    return m;
}

static int is_logical_op_name(const char *op) {
    return strcmp(op, "&&") == 0 || strcmp(op, "||") == 0 ||
           strcmp(op, "&") == 0 || strcmp(op, "|") == 0;
}

static int is_assign_op_name(const char *op) {
    return strcmp(op, "<-") == 0 || strcmp(op, "<<-") == 0 ||
           strcmp(op, "=") == 0 || strcmp(op, "->") == 0 || strcmp(op, "->>") == 0;
}

SEXP imputesrcref_transform_expr(SEXP expr, int node_id, parse_ctx *ctx) {
    if (TYPEOF(expr) != LANGSXP) return expr;

    SEXP out = PROTECT(Rf_duplicate(expr));

    /* Already-injected transparent braces may not exist in the original parse
       data. Recurse into the child with the same node_id. */
    if (imputesrcref_is_braced(out) && !node_has_token(node_id, "'{'", ctx)) {
        int n = call_length_cons(out);
        if (n >= 2) {
            SEXP slot2 = cell_at(out, 2);
            SEXP newval = PROTECT(imputesrcref_transform_expr(CAR(slot2), node_id, ctx));
            SETCAR(slot2, newval);
            UNPROTECT(1);
        }
        UNPROTECT(1);
        return out;
    }

    const char *op = call_op_name(out);
    int n_ids;
    int *child_ids = expr_children_ids(node_id, ctx, &n_ids);

    if (strcmp(op, "function") == 0) {
        SEXP fmls = CADR(out);
        int cursor = 0;

        if (fmls != R_NilValue) {
            SEXP cell = fmls;
            while (cell != R_NilValue) {
                SEXP val = CAR(cell);
                /* Skip only true "missing" defaults: R_MissingArg (the empty
                   symbol). Legitimate NULL defaults (function(x = NULL)) still
                   appear as expr nodes in parse data and must advance the
                   cursor, otherwise subsequent defaults are mis-paired with
                   prior expr children. */
                if (val != R_MissingArg) {
                    if (cursor >= n_ids) Rf_error("Parse mapping mismatch for function formals");
                    int cid = child_ids[cursor++];
                    SEXP newv = PROTECT(imputesrcref_transform_expr(val, cid, ctx));
                    if (!imputesrcref_is_braced(newv) && TYPEOF(newv) == LANGSXP) {
                        SEXP sr = PROTECT(imputesrcref_node_srcref(cid, ctx));
                        newv = imputesrcref_wrap_brace(newv, sr);
                        UNPROTECT(1);
                        PROTECT(newv);
                        SETCAR(cell, newv);
                        UNPROTECT(2);
                    } else {
                        SETCAR(cell, newv);
                        UNPROTECT(1);
                    }
                }
                cell = CDR(cell);
            }
        }

        if (cursor >= n_ids) Rf_error("Parse mapping mismatch for function body");
        int body_id = child_ids[cursor];
        SEXP body_cell = cell_at(out, 3);
        SEXP body_val = CAR(body_cell);
        SEXP body_new = PROTECT(imputesrcref_transform_expr(body_val, body_id, ctx));
        if (!imputesrcref_is_braced(body_new) && TYPEOF(body_new) != LANGSXP) {
            SEXP sr = PROTECT(imputesrcref_node_srcref(body_id, ctx));
            body_new = imputesrcref_wrap_brace(body_new, sr);
            UNPROTECT(1);
            PROTECT(body_new);
            SETCAR(body_cell, body_new);
            UNPROTECT(2);
        } else {
            SETCAR(body_cell, body_new);
            UNPROTECT(1);
        }
        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "if") == 0) {
        int n_parts = call_length_cons(out);
        if (n_ids < 2) {
            if (n_parts >= 2) {
                SEXP c2 = cell_at(out, 2);
                SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c2), node_id, ctx));
                SETCAR(c2, v);
                UNPROTECT(1);
            }
            if (n_parts >= 3) {
                SEXP c3 = cell_at(out, 3);
                SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c3), node_id, ctx));
                SETCAR(c3, v);
                UNPROTECT(1);
            }
            if (n_parts >= 4) {
                SEXP c4 = cell_at(out, 4);
                SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c4), node_id, ctx));
                SETCAR(c4, v);
                UNPROTECT(1);
            }
            UNPROTECT(1);
            return out;
        }

        /* condition */
        SEXP c2 = cell_at(out, 2);
        SEXP cond_new = PROTECT(imputesrcref_transform_expr(CAR(c2), child_ids[0], ctx));
        if (!imputesrcref_is_braced(cond_new) && !imputesrcref_is_logical_op_call(cond_new)) {
            SEXP sr = PROTECT(imputesrcref_node_srcref(child_ids[0], ctx));
            cond_new = imputesrcref_wrap_brace(cond_new, sr);
            UNPROTECT(1);
            PROTECT(cond_new);
            SETCAR(c2, cond_new);
            UNPROTECT(2);
        } else {
            SETCAR(c2, cond_new);
            UNPROTECT(1);
        }

        /* then */
        SEXP c3 = cell_at(out, 3);
        SEXP then_v = recurse_and_maybe_wrap(CAR(c3), child_ids[1], ctx, 1);
        PROTECT(then_v);
        SETCAR(c3, then_v);
        UNPROTECT(1);

        /* else */
        if (n_parts >= 4 && n_ids >= 3) {
            SEXP c4 = cell_at(out, 4);
            SEXP else_v = recurse_and_maybe_wrap(CAR(c4), child_ids[2], ctx, 1);
            PROTECT(else_v);
            SETCAR(c4, else_v);
            UNPROTECT(1);
        }

        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "while") == 0) {
        if (n_ids < 2) Rf_error("Parse mapping mismatch for while expression");
        SEXP c2 = cell_at(out, 2);
        SEXP cond_new = PROTECT(imputesrcref_transform_expr(CAR(c2), child_ids[0], ctx));
        if (!imputesrcref_is_braced(cond_new) && !imputesrcref_is_logical_op_call(cond_new)) {
            SEXP sr = PROTECT(imputesrcref_node_srcref(child_ids[0], ctx));
            cond_new = imputesrcref_wrap_brace(cond_new, sr);
            UNPROTECT(1);
            PROTECT(cond_new);
            SETCAR(c2, cond_new);
            UNPROTECT(2);
        } else {
            SETCAR(c2, cond_new);
            UNPROTECT(1);
        }
        SEXP c3 = cell_at(out, 3);
        SEXP body_v = recurse_and_maybe_wrap(CAR(c3), child_ids[1], ctx, 1);
        PROTECT(body_v);
        SETCAR(c3, body_v);
        UNPROTECT(1);
        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "for") == 0) {
        int forcond_id = find_forcond_id(node_id, ctx);
        int seq_id = -1;
        if (forcond_id >= 0) {
            seq_id = first_expr_under(forcond_id, ctx);
        }

        int body_start = 0;
        if (n_ids > 0 && seq_id >= 0 && child_ids[0] == seq_id) {
            body_start = 1;
        }

        int body_ids_count = n_ids - body_start;
        if (seq_id < 0 || body_ids_count < 1) {
            Rf_error("Parse mapping mismatch for for expression");
        }

        SEXP c3 = cell_at(out, 3);
        SEXP seq_v = recurse_and_maybe_wrap(CAR(c3), seq_id, ctx, 1);
        PROTECT(seq_v);
        SETCAR(c3, seq_v);
        UNPROTECT(1);

        SEXP c4 = cell_at(out, 4);
        SEXP body_v = recurse_and_maybe_wrap(CAR(c4), child_ids[body_start], ctx, 1);
        PROTECT(body_v);
        SETCAR(c4, body_v);
        UNPROTECT(1);

        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "repeat") == 0) {
        if (n_ids < 1) Rf_error("Parse mapping mismatch for repeat expression");
        SEXP c2 = cell_at(out, 2);
        SEXP body_v = recurse_and_maybe_wrap(CAR(c2), child_ids[0], ctx, 1);
        PROTECT(body_v);
        SETCAR(c2, body_v);
        UNPROTECT(1);
        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "switch") == 0) {
        int *ids = child_ids;
        int n = n_ids;
        if (n > 0) { ids++; n--; }

        int cursor = 0;
        SEXP cell = CDR(out);
        int parts_i = 2;
        while (cell != R_NilValue) {
            SEXP arg = CAR(cell);
            if (!imputesrcref_is_missing_arg(arg)) {
                if (cursor >= n) break;
                int cid = ids[cursor++];
                SEXP v = recurse_and_maybe_wrap(arg, cid, ctx, 1);
                PROTECT(v);
                SETCAR(cell, v);
                UNPROTECT(1);
            }
            cell = CDR(cell);
            parts_i++;
        }
        (void) parts_i;

        UNPROTECT(1);
        return out;
    }

    if (is_logical_op_name(op)) {
        int n_parts = call_length_cons(out);
        if (n_ids < 2) {
            if (n_ids == 1) {
                SEXP c2 = cell_at(out, 2);
                SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c2), child_ids[0], ctx));
                SETCAR(c2, v);
                UNPROTECT(1);
                if (n_parts >= 3) {
                    SEXP c3 = cell_at(out, 3);
                    SEXP v3 = PROTECT(imputesrcref_transform_expr(CAR(c3), node_id, ctx));
                    SETCAR(c3, v3);
                    UNPROTECT(1);
                }
            } else {
                if (n_parts >= 2) {
                    SEXP c2 = cell_at(out, 2);
                    SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c2), node_id, ctx));
                    SETCAR(c2, v);
                    UNPROTECT(1);
                }
                if (n_parts >= 3) {
                    SEXP c3 = cell_at(out, 3);
                    SEXP v = PROTECT(imputesrcref_transform_expr(CAR(c3), node_id, ctx));
                    SETCAR(c3, v);
                    UNPROTECT(1);
                }
            }
            UNPROTECT(1);
            return out;
        }
        SEXP c2 = cell_at(out, 2);
        SEXP v2 = recurse_and_maybe_wrap(CAR(c2), child_ids[0], ctx, 1);
        PROTECT(v2);
        SETCAR(c2, v2);
        UNPROTECT(1);
        SEXP c3 = cell_at(out, 3);
        SEXP v3 = recurse_and_maybe_wrap(CAR(c3), child_ids[1], ctx, 1);
        PROTECT(v3);
        SETCAR(c3, v3);
        UNPROTECT(1);
        UNPROTECT(1);
        return out;
    }

    /* Generic call. */
    mapping_t m = map_generic_indices(out, child_ids, n_ids, 0);
    if (!m.valid) {
        UNPROTECT(1);
        return out;
    }

    int op_blacklisted = (op[0] != '\0') && imputesrcref_in_strvec(ctx->arg_wrap_blacklist, op);
    int wrap_generic_args = node_has_token(node_id, "'('", ctx) &&
                            strcmp(op, "(") != 0 &&
                            !op_blacklisted &&
                            ctx->wrap_call_args;

    int is_assign_op = (op[0] != '\0') && is_assign_op_name(op);
    int lhs_slot = (strcmp(op, "->") == 0 || strcmp(op, "->>") == 0) ? 3 : 2;

    for (int k = 0; k < m.n; k++) {
        int i = m.indices[k];
        int cid = m.ids[k];
        SEXP cell = cell_at(out, i);
        SEXP arg = CAR(cell);
        if (imputesrcref_is_missing_arg(arg)) continue;

        int is_assign_lhs = is_assign_op && i == lhs_slot;
        int saved_wrap = ctx->wrap_call_args;
        if (is_assign_lhs && ctx->wrap_call_args) ctx->wrap_call_args = 0;
        SEXP transformed = PROTECT(imputesrcref_transform_expr(arg, cid, ctx));
        if (is_assign_lhs && saved_wrap) ctx->wrap_call_args = saved_wrap;

        int slot_callee_bl = callee_blacklisted(arg, ctx);
        int do_wrap = wrap_generic_args && i > 1 && !is_assign_lhs &&
                      TYPEOF(arg) == LANGSXP &&
                      !imputesrcref_is_braced(transformed) &&
                      !imputesrcref_is_unquote_call(arg) &&
                      !imputesrcref_is_call_named(arg, ":=") &&
                      !imputesrcref_is_unary_arith_call(arg) &&
                      !slot_callee_bl;
        if (do_wrap) {
            SEXP sr = PROTECT(imputesrcref_node_srcref(cid, ctx));
            transformed = imputesrcref_wrap_brace(transformed, sr);
            UNPROTECT(1);
            PROTECT(transformed);
            SETCAR(cell, transformed);
            UNPROTECT(2);
        } else {
            SETCAR(cell, transformed);
            UNPROTECT(1);
        }
    }

    UNPROTECT(1);
    return out;
}

static SEXP call_parse(const char *text) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("parse"), R_BaseEnv));
    SEXP txt = PROTECT(Rf_mkString(text));
    SEXP keep = PROTECT(Rf_ScalarLogical(1));
    SEXP call = PROTECT(Rf_lang3(fn, txt, keep));
    SET_TAG(CDR(call), Rf_install("text"));
    SET_TAG(CDDR(call), Rf_install("keep.source"));
    int err = 0;
    /* Use the silent variant: the first parse attempt is expected to fail for
       functions defined inside a call (e.g. setMethod) whose extracted source
       has `else` starting a line. The caller retries wrapped in `(...)`, so the
       failure is normal control flow and must not be printed to stderr.
       R_tryEval (non-silent) would emit the error via the C error handler,
       which suppressMessages()/suppressWarnings() on the R side cannot catch. */
    SEXP res = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(4);
    if (err) return R_NilValue;
    return res;
}

static SEXP call_getParseData(SEXP parsed) {
    SEXP fn = PROTECT(Rf_findFun(Rf_install("getParseData"), R_BaseNamespace));
    SEXP call = PROTECT(Rf_lang2(fn, parsed));
    int err = 0;
    SEXP res = R_tryEvalSilent(call, R_GlobalEnv, &err);
    UNPROTECT(2);
    if (err) return R_NilValue;
    return res;
}

static SEXP call_formals(SEXP fn) {
    SEXP f = PROTECT(Rf_findFun(Rf_install("formals"), R_BaseEnv));
    SEXP call = PROTECT(Rf_lang2(f, fn));
    SEXP res = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return res;
}

static SEXP call_body(SEXP fn) {
    SEXP f = PROTECT(Rf_findFun(Rf_install("body"), R_BaseEnv));
    SEXP call = PROTECT(Rf_lang2(f, fn));
    SEXP res = Rf_eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return res;
}

SEXP C_impute_srcrefs(SEXP fn, SEXP wrap_call_args_sxp, SEXP quiet_sxp) {
    if (TYPEOF(fn) != CLOSXP) {
        Rf_error("`fn` must be a function");
    }
    if (TYPEOF(wrap_call_args_sxp) != LGLSXP || Rf_xlength(wrap_call_args_sxp) != 1 ||
        LOGICAL(wrap_call_args_sxp)[0] == NA_LOGICAL) {
        Rf_error("`wrap_call_args` must be TRUE or FALSE");
    }
    if (TYPEOF(quiet_sxp) != LGLSXP || Rf_xlength(quiet_sxp) != 1 ||
        LOGICAL(quiet_sxp)[0] == NA_LOGICAL) {
        Rf_error("`quiet` must be TRUE or FALSE");
    }
    int wrap_call_args = LOGICAL(wrap_call_args_sxp)[0];
    int quiet = LOGICAL(quiet_sxp)[0];

    SEXP fn_attrs = PROTECT(Rf_duplicate(ATTRIB(fn)));

    /* `quiet` only suppresses the "no srcref" message emitted by
       source_text. OR with the ambient flag so a `quiet = TRUE` call inside an
       already-quiet batch op never un-quiets it; restore immediately after. */
    int saved_quiet = imputesrcref_quiet;
    if (quiet) imputesrcref_quiet = 1;
    SEXP src = PROTECT(imputesrcref_source_text(fn));
    imputesrcref_quiet = saved_quiet;
    if (src == R_NilValue) {
        UNPROTECT(2);
        return fn;
    }

    SEXP text_sxp = VECTOR_ELT(src, 0);
    SEXP srcfile = VECTOR_ELT(src, 1);
    int line_offset = INTEGER(VECTOR_ELT(src, 2))[0];
    int first_col_offset = INTEGER(VECTOR_ELT(src, 3))[0];

    const char *txt = CHAR(STRING_ELT(text_sxp, 0));
    SEXP parsed = PROTECT(call_parse(txt));
    int paren_wrapped = 0;

    if (parsed == R_NilValue) {
        UNPROTECT(1);  /* parsed (NilValue) */
        int len = (int) strlen(txt);
        char *buf = (char*) R_alloc((size_t)(len + 3), sizeof(char));
        buf[0] = '(';
        memcpy(buf + 1, txt, (size_t) len);
        buf[len + 1] = ')';
        buf[len + 2] = '\0';
        parsed = PROTECT(call_parse(buf));
        if (parsed == R_NilValue) Rf_error("Could not parse function source");
        paren_wrapped = 1;
    }

    SEXP pd = PROTECT(call_getParseData(parsed));
    if (pd == R_NilValue || TYPEOF(pd) != VECSXP) {
        Rf_error("Could not obtain parse data for function");
    }

    /* find expr_rows / root id */
    SEXP nms = Rf_getAttrib(pd, R_NamesSymbol);
    SEXP token_col = R_NilValue;
    SEXP id_col = R_NilValue;
    SEXP parent_col = R_NilValue;
    R_xlen_t nc = Rf_xlength(nms);
    for (R_xlen_t i = 0; i < nc; i++) {
        const char *cn = CHAR(STRING_ELT(nms, i));
        if (strcmp(cn, "token") == 0) token_col = VECTOR_ELT(pd, i);
        else if (strcmp(cn, "id") == 0) id_col = VECTOR_ELT(pd, i);
        else if (strcmp(cn, "parent") == 0) parent_col = VECTOR_ELT(pd, i);
    }
    if (token_col == R_NilValue || id_col == R_NilValue || parent_col == R_NilValue) {
        Rf_error("Parse data missing required columns");
    }
    R_xlen_t pd_n = Rf_xlength(token_col);
    if (pd_n == 0) Rf_error("Parse data does not contain expression nodes");

    int *id_arr = INTEGER(id_col);
    int *par_arr = INTEGER(parent_col);

    int root_id = -1;
    int root_count = 0;
    for (R_xlen_t i = 0; i < pd_n; i++) {
        if (strcmp(CHAR(STRING_ELT(token_col, i)), "expr") == 0 && par_arr[i] == 0) {
            root_id = id_arr[i];
            root_count++;
        }
    }
    if (root_count != 1) {
        Rf_error("Expected exactly one top-level expression for function source");
    }

    if (paren_wrapped) {
        /* descend into inner expr child */
        int inner = -1;
        int inner_count = 0;
        for (R_xlen_t i = 0; i < pd_n; i++) {
            if (strcmp(CHAR(STRING_ELT(token_col, i)), "expr") == 0 && par_arr[i] == root_id) {
                inner = id_arr[i];
                inner_count++;
            }
        }
        if (inner_count != 1) Rf_error("Expected exactly one expression inside paren wrapper");
        root_id = inner;
        first_col_offset -= 1;
    }

    SEXP blacklist = PROTECT(imputesrcref_effective_blacklist());

    parse_ctx ctx;
    imputesrcref_build_ctx(&ctx, pd, srcfile, line_offset, first_col_offset, blacklist, wrap_call_args);

    /* Source lines for visual->byte column conversion in node_srcref. Absolute
       line k maps to lines[k - (line_offset + 1)]. */
    SEXP src_lines = VECTOR_ELT(src, 4);
    if (TYPEOF(src_lines) == STRSXP) {
        ctx.abs_lines = src_lines;
        ctx.n_abs_lines = (int) Rf_xlength(src_lines);
        ctx.abs_lines_start = line_offset + 1;
    }

    /* Build fn_expr = call("function", formals(fn), body(fn)) */
    SEXP fmls = PROTECT(call_formals(fn));
    SEXP bdy = PROTECT(call_body(fn));
    SEXP fnsym = PROTECT(Rf_install("function"));
    SEXP fn_expr = PROTECT(Rf_allocList(3));
    SET_TYPEOF(fn_expr, LANGSXP);
    SETCAR(fn_expr, fnsym);
    SETCAR(CDR(fn_expr), fmls);
    SETCAR(CDDR(fn_expr), bdy);

    SEXP transformed = PROTECT(imputesrcref_transform_expr(fn_expr, root_id, &ctx));

    /* Apply to fn: out <- fn; formals(out) <- transformed[[2]]; body(out) <- transformed[[3]] */
    SEXP out = PROTECT(Rf_duplicate(fn));
    SEXP new_formals = CADR(transformed);
    SEXP new_body = CADDR(transformed);

    if (new_formals != R_NilValue) {
        SET_FORMALS(out, new_formals);
    }
    SET_BODY(out, new_body);

    /* Reapply original attributes */
    SEXP attr_iter = fn_attrs;
    while (attr_iter != R_NilValue) {
        SEXP tag = TAG(attr_iter);
        SEXP val = CAR(attr_iter);
        Rf_setAttrib(out, tag, val);
        attr_iter = CDR(attr_iter);
    }

    UNPROTECT(11);
    return out;
}
