#' Manage call blacklist for generic argument wrapping
#'
#' By default, [impute_srcrefs()] skips argument wrapping for primitive
#' `SPECIALSXP` calls discovered from [builtins()]. These functions inspect and
#' modify the user-configured portion of that blacklist.
#'
#' - `get_impute_blacklist()` returns the blacklist, optionally including the
#'   built-in defaults.
#' - `set_impute_blacklist()` adds or replaces user entries, stored in
#'   `options(imputesrcref.wrap_arg_blacklist = ...)`.
#' - `reset_impute_blacklist()` clears user entries.
#'
#' @param include_default If `TRUE`, include built-in `SPECIALSXP` names.
#' @param functions Character vector of call names. `NULL` clears user entries.
#' @param append If `TRUE` append to existing user entries; otherwise replace.
#'
#' @return
#' `get_impute_blacklist()` returns a sorted unique character vector of call
#' names. `set_impute_blacklist()` invisibly returns the current
#' user-configured entries. `reset_impute_blacklist()` invisibly returns an
#' empty character vector.
#'
#' @examples
#' head(get_impute_blacklist())
#' set_impute_blacklist(c("str_c", "paste"))
#' get_impute_blacklist(include_default = FALSE)
#' reset_impute_blacklist()
#' @rdname impute_blacklist
#' @export
get_impute_blacklist <- function(include_default = TRUE) {
  .Call(C_get_impute_blacklist, include_default)
}

#' @rdname impute_blacklist
#' @export
set_impute_blacklist <- function(functions, append = TRUE) {
  invisible(.Call(C_set_impute_blacklist, functions, append))
}

#' @rdname impute_blacklist
#' @export
reset_impute_blacklist <- function() {
  invisible(.Call(C_reset_impute_blacklist))
}
