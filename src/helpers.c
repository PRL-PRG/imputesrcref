#include "imputesrcref.h"
#include <string.h>
#include <stdlib.h>

int imputesrcref_is_call_named(SEXP expr, const char *name) {
    if (TYPEOF(expr) != LANGSXP) return 0;
    SEXP head = CAR(expr);
    if (TYPEOF(head) != SYMSXP) return 0;
    return strcmp(CHAR(PRINTNAME(head)), name) == 0;
}

int imputesrcref_is_braced(SEXP expr) {
    return imputesrcref_is_call_named(expr, "{");
}

int imputesrcref_is_missing_arg(SEXP x) {
    return TYPEOF(x) == SYMSXP && CHAR(PRINTNAME(x))[0] == '\0';
}

int imputesrcref_is_logical_op_call(SEXP expr) {
    if (TYPEOF(expr) != LANGSXP) return 0;
    SEXP head = CAR(expr);
    if (TYPEOF(head) != SYMSXP) return 0;
    const char *nm = CHAR(PRINTNAME(head));
    return strcmp(nm, "&&") == 0 || strcmp(nm, "||") == 0 ||
           strcmp(nm, "&") == 0 || strcmp(nm, "|") == 0;
}

int imputesrcref_is_unquote_call(SEXP expr) {
    if (TYPEOF(expr) != LANGSXP) return 0;
    R_xlen_t n = Rf_xlength(expr);
    if (n != 2) return 0;
    SEXP head = CAR(expr);
    if (TYPEOF(head) != SYMSXP) return 0;
    const char *nm = CHAR(PRINTNAME(head));
    if (strcmp(nm, "!") == 0) {
        SEXP inner = CADR(expr);
        if (TYPEOF(inner) != LANGSXP) return 0;
        if (Rf_xlength(inner) != 2) return 0;
        SEXP ihead = CAR(inner);
        return TYPEOF(ihead) == SYMSXP && strcmp(CHAR(PRINTNAME(ihead)), "!") == 0;
    }
    if (strcmp(nm, "(") == 0) {
        return imputesrcref_is_unquote_call(CADR(expr));
    }
    return 0;
}

int imputesrcref_is_unary_arith_call(SEXP expr) {
    if (TYPEOF(expr) != LANGSXP) return 0;
    if (Rf_xlength(expr) != 2) return 0;
    SEXP head = CAR(expr);
    if (TYPEOF(head) != SYMSXP) return 0;
    const char *nm = CHAR(PRINTNAME(head));
    return strcmp(nm, "-") == 0 || strcmp(nm, "+") == 0;
}

int imputesrcref_in_strvec(SEXP vec, const char *name) {
    if (TYPEOF(vec) != STRSXP) return 0;
    R_xlen_t n = Rf_xlength(vec);
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP s = STRING_ELT(vec, i);
        if (s != NA_STRING && strcmp(CHAR(s), name) == 0) return 1;
    }
    return 0;
}

static int get_int_col(SEXP df, const char *name, int **out) {
    SEXP nms = Rf_getAttrib(df, R_NamesSymbol);
    R_xlen_t n = Rf_xlength(nms);
    for (R_xlen_t i = 0; i < n; i++) {
        if (strcmp(CHAR(STRING_ELT(nms, i)), name) == 0) {
            SEXP col = VECTOR_ELT(df, i);
            if (TYPEOF(col) != INTSXP) Rf_error("Column `%s` is not integer", name);
            *out = INTEGER(col);
            return (int) Rf_xlength(col);
        }
    }
    Rf_error("Column `%s` not found in parse data", name);
    return -1;
}

static SEXP get_str_col(SEXP df, const char *name) {
    SEXP nms = Rf_getAttrib(df, R_NamesSymbol);
    R_xlen_t n = Rf_xlength(nms);
    for (R_xlen_t i = 0; i < n; i++) {
        if (strcmp(CHAR(STRING_ELT(nms, i)), name) == 0) {
            SEXP col = VECTOR_ELT(df, i);
            if (TYPEOF(col) != STRSXP) Rf_error("Column `%s` is not character", name);
            return col;
        }
    }
    Rf_error("Column `%s` not found in parse data", name);
    return R_NilValue;
}

typedef struct {
    int *id_arr;
    int *line1_arr;
    int *col1_arr;
} sort_ctx;

static sort_ctx *g_sort_ctx;

static int cmp_expr_child(const void *a, const void *b) {
    int ia = *(const int*) a;
    int ib = *(const int*) b;
    int ra = g_sort_ctx->id_arr[ia];
    int rb = g_sort_ctx->id_arr[ib];
    (void) ra; (void) rb;
    int la = g_sort_ctx->line1_arr[ia];
    int lb = g_sort_ctx->line1_arr[ib];
    if (la != lb) return la - lb;
    int ca = g_sort_ctx->col1_arr[ia];
    int cb = g_sort_ctx->col1_arr[ib];
    if (ca != cb) return ca - cb;
    return g_sort_ctx->id_arr[ia] - g_sort_ctx->id_arr[ib];
}

void imputesrcref_build_ctx(parse_ctx *ctx, SEXP pd, SEXP srcfile,
                            int line_offset, int first_col_offset,
                            SEXP arg_wrap_blacklist, int wrap_call_args) {
    ctx->pd = pd;
    ctx->srcfile = srcfile;
    ctx->line_offset = line_offset;
    ctx->first_col_offset = first_col_offset;
    ctx->abs_lines = R_NilValue;
    ctx->n_abs_lines = 0;
    ctx->abs_lines_start = 1;
    ctx->arg_wrap_blacklist = arg_wrap_blacklist;
    ctx->wrap_call_args = wrap_call_args;

    int n1 = get_int_col(pd, "id", &ctx->id);
    int n2 = get_int_col(pd, "parent", &ctx->parent);
    int n3 = get_int_col(pd, "line1", &ctx->line1);
    int n4 = get_int_col(pd, "col1", &ctx->col1);
    int n5 = get_int_col(pd, "line2", &ctx->line2);
    int n6 = get_int_col(pd, "col2", &ctx->col2);
    if (!(n1 == n2 && n1 == n3 && n1 == n4 && n1 == n5 && n1 == n6)) {
        Rf_error("Parse data columns have differing lengths");
    }
    ctx->nrow = n1;
    ctx->token = get_str_col(pd, "token");

    int max_id = 0;
    for (int i = 0; i < ctx->nrow; i++) {
        if (ctx->id[i] > max_id) max_id = ctx->id[i];
    }
    ctx->max_id = max_id;

    ctx->id_to_pdrow = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    for (int i = 0; i <= max_id + 1; i++) ctx->id_to_pdrow[i] = -1;
    for (int i = 0; i < ctx->nrow; i++) {
        if (ctx->id[i] >= 0 && ctx->id[i] <= max_id) {
            ctx->id_to_pdrow[ctx->id[i]] = i;
        }
    }

    int *all_counts = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    int *expr_counts = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    for (int i = 0; i <= max_id + 1; i++) { all_counts[i] = 0; expr_counts[i] = 0; }

    int *is_expr_row = (int*) R_alloc((size_t) ctx->nrow, sizeof(int));
    for (int i = 0; i < ctx->nrow; i++) {
        is_expr_row[i] = (strcmp(CHAR(STRING_ELT(ctx->token, i)), "expr") == 0) ? 1 : 0;
    }

    for (int i = 0; i < ctx->nrow; i++) {
        int p = ctx->parent[i];
        if (p >= 0 && p <= max_id) {
            all_counts[p]++;
            if (is_expr_row[i]) expr_counts[p]++;
        }
    }

    ctx->all_children_offset = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    ctx->all_children_count = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    ctx->expr_children_offset = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));
    ctx->expr_children_count = (int*) R_alloc((size_t)(max_id + 2), sizeof(int));

    int all_total = 0, expr_total = 0;
    for (int i = 0; i <= max_id + 1; i++) {
        ctx->all_children_offset[i] = all_total;
        ctx->expr_children_offset[i] = expr_total;
        ctx->all_children_count[i] = 0;
        ctx->expr_children_count[i] = 0;
        all_total += all_counts[i];
        expr_total += expr_counts[i];
    }

    ctx->all_children_data = (int*) R_alloc((size_t)(all_total > 0 ? all_total : 1), sizeof(int));
    ctx->expr_children_data = (int*) R_alloc((size_t)(expr_total > 0 ? expr_total : 1), sizeof(int));

    for (int i = 0; i < ctx->nrow; i++) {
        int p = ctx->parent[i];
        if (p < 0 || p > max_id) continue;
        int pos = ctx->all_children_offset[p] + ctx->all_children_count[p]++;
        ctx->all_children_data[pos] = i;
        if (is_expr_row[i]) {
            int epos = ctx->expr_children_offset[p] + ctx->expr_children_count[p]++;
            ctx->expr_children_data[epos] = i;
        }
    }

    sort_ctx sctx;
    sctx.id_arr = ctx->id;
    sctx.line1_arr = ctx->line1;
    sctx.col1_arr = ctx->col1;
    g_sort_ctx = &sctx;
    for (int p = 0; p <= max_id; p++) {
        int cnt = ctx->expr_children_count[p];
        if (cnt > 1) {
            qsort(ctx->expr_children_data + ctx->expr_children_offset[p],
                  (size_t) cnt, sizeof(int), cmp_expr_child);
        }
    }
    g_sort_ctx = NULL;
}

SEXP imputesrcref_node_srcref(int node_id, parse_ctx *ctx) {
    if (node_id < 0 || node_id > ctx->max_id) Rf_error("Missing parse node id %d", node_id);
    int row = ctx->id_to_pdrow[node_id];
    if (row < 0) Rf_error("Missing parse node id %d", node_id);
    int rl1 = ctx->line1[row];
    int rl2 = ctx->line2[row];

    int l1 = rl1 + ctx->line_offset;
    int l2 = rl2 + ctx->line_offset;
    /* Visual (tab-expanded) columns in the original-file coordinate system. */
    int c1 = ctx->col1[row] + (rl1 == 1 ? ctx->first_col_offset : 0);
    int c2 = ctx->col2[row] + (rl2 == 1 ? ctx->first_col_offset : 0);

    /* Byte offsets for the srcref byte slots (2 and 4). R stores bytes there
       and visual columns in slots 5/6; `as.character.srcref` (used by deparse,
       getSrcref and covr) slices source by the byte slots. The parse data only
       gives visual columns, so convert against the absolute source line. `l1`
       and `c1` are already absolute coordinates, so this works uniformly for
       the paren-wrapped and deparse-fallback cases. Fall back to the visual
       value when the line is unavailable. */
    int b1 = c1, b2 = c2;
    if (ctx->abs_lines != R_NilValue) {
        int idx1 = l1 - ctx->abs_lines_start;
        int idx2 = l2 - ctx->abs_lines_start;
        if (idx1 >= 0 && idx1 < ctx->n_abs_lines) {
            b1 = imputesrcref_visual_col_to_byte_col(
                CHAR(STRING_ELT(ctx->abs_lines, idx1)), c1);
        }
        if (idx2 >= 0 && idx2 < ctx->n_abs_lines) {
            b2 = imputesrcref_visual_col_to_byte_col(
                CHAR(STRING_ELT(ctx->abs_lines, idx2)), c2);
        }
    }

    SEXP sr = PROTECT(Rf_allocVector(INTSXP, 8));
    int *p = INTEGER(sr);
    p[0] = l1; p[1] = b1; p[2] = l2; p[3] = b2;
    p[4] = c1; p[5] = c2; p[6] = l1; p[7] = l2;
    Rf_setAttrib(sr, Rf_install("srcfile"), ctx->srcfile);
    SEXP cls = PROTECT(Rf_mkString("srcref"));
    Rf_setAttrib(sr, R_ClassSymbol, cls);
    UNPROTECT(2);
    return sr;
}

SEXP imputesrcref_wrap_brace(SEXP expr, SEXP sr) {
    SEXP brace = PROTECT(Rf_install("{"));
    SEXP out = PROTECT(Rf_lang2(brace, expr));
    SEXP srlist = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(srlist, 0, sr);
    SET_VECTOR_ELT(srlist, 1, sr);
    Rf_setAttrib(out, Rf_install("srcref"), srlist);
    UNPROTECT(3);
    return out;
}
