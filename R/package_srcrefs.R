#' Check parse data for a package and impute srcrefs on all its functions.
#'
#' @param package Package name.
#' @param include_internal If `TRUE`, inspect all namespace functions.
#' @param verbose If `TRUE`, print a patch summary.
#'
#' @return Invisibly returns a list with:
#' - `package`: package name
#' - `fn_names`: inspected function names
#' - `failed`: failure messages (`NA` for successfully patched functions)
#' - `patched_count`: number of patched functions
#'
#' @export
impute_package_srcrefs <- function(package, include_internal = TRUE, verbose = TRUE) {
  invisible(.Call(C_impute_package_srcrefs, package, include_internal, verbose))
}
