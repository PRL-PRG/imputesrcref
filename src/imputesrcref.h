#ifndef IMPUTESRCREF_H
#define IMPUTESRCREF_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

typedef struct parse_ctx {
    SEXP pd;
    int nrow;
    int *id;
    int *parent;
    int *line1;
    int *col1;
    int *line2;
    int *col2;
    SEXP token;
    int max_id;
    int *id_to_pdrow;
    int *all_children_offset;
    int *all_children_count;
    int *all_children_data;
    int *expr_children_offset;
    int *expr_children_count;
    int *expr_children_data;
    SEXP srcfile;
    int line_offset;
    int first_col_offset;
    /* Untrimmed source lines spanned by the function, used to convert the
       parse data's visual (tab-expanded) columns into byte offsets for the
       srcref byte slots. `abs_lines[k]` is original-file line
       `abs_lines_start + k`. */
    SEXP abs_lines;
    int n_abs_lines;
    int abs_lines_start;
    SEXP arg_wrap_blacklist;
    int wrap_call_args;
} parse_ctx;

void imputesrcref_build_ctx(parse_ctx *ctx, SEXP pd, SEXP srcfile,
                            int line_offset, int first_col_offset,
                            SEXP arg_wrap_blacklist, int wrap_call_args);

int imputesrcref_is_call_named(SEXP expr, const char *name);
int imputesrcref_is_braced(SEXP expr);
int imputesrcref_is_missing_arg(SEXP x);
int imputesrcref_is_logical_op_call(SEXP expr);
int imputesrcref_is_unquote_call(SEXP expr);
int imputesrcref_is_unary_arith_call(SEXP expr);
int imputesrcref_in_strvec(SEXP vec, const char *name);

SEXP imputesrcref_node_srcref(int node_id, parse_ctx *ctx);
SEXP imputesrcref_wrap_brace(SEXP expr, SEXP sr);
SEXP imputesrcref_transform_expr(SEXP expr, int node_id, parse_ctx *ctx);

SEXP imputesrcref_source_text(SEXP fn);

extern int imputesrcref_quiet;

int imputesrcref_visual_col_to_byte_col(const char *line, int visual_col);

SEXP imputesrcref_effective_blacklist(void);

SEXP C_impute_srcrefs(SEXP fn, SEXP wrap_call_args, SEXP quiet);
SEXP C_get_impute_blacklist(SEXP include_default);
SEXP C_set_impute_blacklist(SEXP functions, SEXP append);
SEXP C_reset_impute_blacklist(void);
SEXP C_impute_package_srcrefs(SEXP package, SEXP include_internal, SEXP verbose);
SEXP C_source_impute_srcrefs(SEXP file, SEXP envir, SEXP chdir,
                              SEXP keep_source, SEXP keep_parse_data,
                              SEXP toplevel_env, SEXP all_names);

SEXP C_collect_srcref_sites(SEXP expr_or_fn);
SEXP C_collect_transparent_srcref_checks(SEXP expr_or_fn);
SEXP C_assert_transparent_srcref_consistency(SEXP expr_or_fn);

#endif
