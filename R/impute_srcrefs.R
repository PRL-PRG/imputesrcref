#' Impute transparent srcrefs for injected braces in a function AST.
#'
#' Traverses a function's AST and wraps unbraced expressions in targeted
#' positions with `{ ... }` while attaching transparent srcrefs to the injected
#' brace calls. The srcref assigned to an injected brace matches the span of the
#' wrapped expression so that source mapping stays aligned with original code.
#'
#' Covered constructs include:
#' - `if` / `else`
#' - `for`, `while`, `repeat`
#' - `switch`
#' - logical operators (`&&`, `||`, `&`, `|`)
#' - function defaults and function bodies
#' - function call arguments (optional; call expressions only)
#'
#' @param fn A function.
#' @param wrap_call_args If `TRUE` (default), wrap generic function-call
#'   arguments that are call expressions.
#' @param quiet If `TRUE`, suppress the informational message emitted when a
#'   function has no srcref metadata and deparse fallback is disabled. Useful
#'   for callers (such as coverage tools) that process many functions and
#'   handle the no-srcref case themselves. Defaults to `FALSE`.
#'
#' @return A function with transformed body/formals and preserved function-level
#'   attributes (including srcref/srcfile metadata when present).
#'
#' @details
#' For functions without srcref metadata, deparse-based fallback is disabled by
#' default. To allow fallback, set
#' `options(impuresrcref.allow_deparse_fallback = TRUE)`.
#'
#' Generic call argument wrapping skips blacklisted callee names. By default the
#' blacklist includes primitive `SPECIALSXP` calls from `builtins()`. Use
#' [set_impute_blacklist()] / [reset_impute_blacklist()] to customize.
#'
#' @examples
#' options(keep.source = TRUE)
#' f <- eval(parse(text = "function(x, y) if (x && y) f() else g()", keep.source = TRUE)[[1]])
#' g <- impute_srcrefs(f)
#' g
#' @export
impute_srcrefs <- function(fn, wrap_call_args = TRUE, quiet = FALSE) {
  .Call(C_impute_srcrefs, fn, wrap_call_args, quiet)
}
