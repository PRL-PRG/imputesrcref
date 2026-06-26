#' Source an R file and impute srcrefs on loaded functions.
#'
#' Sources a file with [base::sys.source()] and then rewrites function bindings
#' that were created or changed by that source call via [impute_srcrefs()].
#' This is the recommended entry point for source files because it can enforce
#' both source retention and parse-data retention.
#'
#' @param file Path to an R source file.
#' @param envir Environment where the file is sourced.
#' @param chdir Passed to [base::sys.source()].
#' @param keep.source Passed to [base::sys.source()]. Defaults to `TRUE`.
#' @param keep.parse.data Passed to [base::sys.source()]. Defaults to `TRUE`.
#' @param toplevel.env Passed to [base::sys.source()].
#' @param all.names Include hidden bindings when scanning for functions.
#'
#' @return Invisibly returns a list with:
#' - `file`: normalized file path
#' - `functions`: names of patched functions
#' - `count`: number of patched functions
#' - `keep.source` / `keep.parse.data`: effective source flags
#'
#' @examples
#' \dontrun{
#' tf <- tempfile(fileext = ".R")
#' writeLines("f <- function(x, y) if (x && y) f() else g()", tf)
#' env <- new.env(parent = baseenv())
#' source_impute_srcrefs(tf, envir = env)
#' env$f
#' }
#' @export
source_impute_srcrefs <- function(
  file,
  envir = parent.frame(),
  chdir = FALSE,
  keep.source = TRUE,
  keep.parse.data = TRUE,
  toplevel.env = as.environment(envir),
  all.names = TRUE
) {
  invisible(.Call(C_source_impute_srcrefs, file, envir, chdir,
                  keep.source, keep.parse.data, toplevel.env, all.names))
}
