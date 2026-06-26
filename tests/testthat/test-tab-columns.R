# Regression tests for tab-indented source. getParseData() reports visual
# (tab-expanded) columns, but R srcref byte slots (2 and 4) must hold byte
# offsets so that as.character.srcref() / getSrcref() (used by deparse and
# covr) extract the correct text. node_srcref converts visual -> byte.

make_fn <- function(code) {
  eval(parse(text = code, keep.source = TRUE)[[1L]], envir = new.env(parent = baseenv()))
}

# Collect (srcref, wrapped-expression) pairs for every injected transparent
# brace in a function body.
brace_pairs <- function(fn) {
  out <- list()
  walk <- function(node) {
    if (is.call(node)) {
      if (is.symbol(node[[1L]]) && identical(as.character(node[[1L]]), "{") &&
          length(node) >= 2L) {
        sr <- attr(node, "srcref", exact = TRUE)
        if (!is.null(sr)) {
          sr_list <- if (is.list(sr)) sr else list(sr)
          if (length(sr_list) >= 2L && identical(sr_list[[1L]], sr_list[[2L]])) {
            out[[length(out) + 1L]] <<- list(
              sr = sr_list[[1L]],
              expr = node[[2L]]
            )
          }
        }
      }
      for (i in seq_along(node)) walk(node[[i]])
    }
  }
  walk(body(fn))
  out
}

test_that("tab-indented source yields byte-accurate srcref slots", {
  # Tab + spaces before the call, so visual columns differ from byte columns.
  code <- "function(e1, e2) {\n\t  out <- zoo(e, index(e2), attr(e2, \"f\"))\n\tout\n}"
  fn <- make_fn(code)
  g <- imputesrcref::impute_srcrefs(fn)

  pairs <- brace_pairs(g)
  expect_gt(length(pairs), 0L)

  for (pr in pairs) {
    sr <- pr$sr
    # Byte slots (2,4) must differ from visual slots (5,6) on the tab line for
    # at least the deeper calls; more importantly, R's own extractor must
    # return the wrapped expression verbatim.
    extracted <- paste(as.character(sr), collapse = "\n")
    expected <- paste(deparse(pr$expr), collapse = "\n")
    expect_identical(
      extracted, expected,
      info = sprintf("srcref slots %s", paste(as.integer(sr), collapse = ","))
    )
  }
})

test_that("at least one tab-line brace has byte slot < visual slot", {
  code <- "function(x) {\n\t\tg(h(x))\n}"
  fn <- make_fn(code)
  g <- imputesrcref::impute_srcrefs(fn)
  pairs <- brace_pairs(g)
  expect_gt(length(pairs), 0L)

  # The two leading tabs make visual column (slot 5) strictly greater than the
  # byte column (slot 2) for content on that line.
  has_offset <- any(vapply(pairs, function(pr) pr$sr[[2L]] < pr$sr[[5L]], logical(1)))
  expect_true(has_offset)
})

test_that("space-indented source keeps byte slots equal to visual slots", {
  code <- "function(x, y) if (x && y) f() else g()"
  fn <- make_fn(code)
  g <- imputesrcref::impute_srcrefs(fn)
  for (pr in brace_pairs(g)) {
    expect_identical(pr$sr[[2L]], pr$sr[[5L]])
    expect_identical(pr$sr[[4L]], pr$sr[[6L]])
  }
})
