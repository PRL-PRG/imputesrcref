#' @useDynLib imputesrcref, .registration = TRUE
NULL

collect_srcref_sites <- function(expr_or_fn) {
  .Call(C_collect_srcref_sites, expr_or_fn)
}

collect_transparent_srcref_checks <- function(expr_or_fn) {
  .Call(C_collect_transparent_srcref_checks, expr_or_fn)
}

assert_transparent_srcref_consistency <- function(expr_or_fn) {
  invisible(.Call(C_assert_transparent_srcref_consistency, expr_or_fn))
}

write_srcref_sites <- function(expr_or_fn, path) {
  lines <- collect_srcref_sites(expr_or_fn)
  writeLines(lines, con = path, useBytes = TRUE)
  invisible(lines)
}
