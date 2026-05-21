#' Get call names blacklisted from generic argument wrapping.
#'
#' By default, [impute_srcrefs()] skips argument wrapping for primitive
#' `SPECIALSXP` calls discovered from [builtins()]. This getter can return only
#' user-configured entries, or the effective blacklist including defaults.
#'
#' @param include_default If `TRUE`, include built-in `SPECIALSXP` names.
#'
#' @return A sorted unique character vector of call names.
#'
#' @examples
#' head(get_impute_blacklist())
#' set_impute_blacklist(c("str_c", "paste"))
#' get_impute_blacklist(include_default = FALSE)
#' reset_impute_blacklist()
#' @export
get_impute_blacklist <- function(include_default = TRUE) {
  .Call(C_get_impute_blacklist, include_default)
}

#' Set user call names blacklisted from generic argument wrapping.
#'
#' User entries are stored in `options(imputesrcref.wrap_arg_blacklist = ...)`.
#'
#' @param functions Character vector of call names. `NULL` clears user entries.
#' @param append If `TRUE` append to existing user entries; otherwise replace.
#'
#' @return Invisibly returns current user-configured blacklist entries.
#' @export
set_impute_blacklist <- function(functions, append = TRUE) {
  invisible(.Call(C_set_impute_blacklist, functions, append))
}

#' Reset user call names blacklisted from generic argument wrapping.
#'
#' Clears user entries configured via [set_impute_blacklist()].
#'
#' @return Invisibly returns an empty character vector.
#' @export
reset_impute_blacklist <- function() {
  invisible(.Call(C_reset_impute_blacklist))
}
