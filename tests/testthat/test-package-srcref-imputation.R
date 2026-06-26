# Whole-package srcref imputation over a corpus of popular packages.
#
# These are heavy integration tests: they patch every function in each
# package's namespace and verify correctness invariants. They require the
# packages to be installed with srcref retention (install from source with
# `--with-keep.source`), so they are gated behind FULL_TEST=1 and degrade
# gracefully when a package is absent or was installed without srcref.
#
#   FULL_TEST=1 Rscript -e "testthat::test_dir('tests/testthat')"

corpus_packages <- c(
  "data.table",
  "dplyr",
  "fs",
  "ggplot2",
  "glue",
  "jsonlite",
  "stringr",
  "zoo"
)

# Reasons a function may legitimately be left unpatched. Anything else counts
# as a real failure that the test should surface.
skippable_failures <- c("missing parse data", "no srcref")

# How many patched functions to deep-check per package (idempotence + srcref
# text consistency). Bounded so the suite stays reasonably fast on large
# namespaces such as ggplot2 / dplyr.
sample_size <- 40L

for (pkg in corpus_packages) {
  local({
    p <- pkg

    test_that(sprintf("whole-package srcref imputation is correct: %s", p), {
      skip_if_not(
        identical(Sys.getenv("FULL_TEST", unset = ""), "1"),
        "Set FULL_TEST=1 to run."
      )
      skip_if_not_installed(p)

      res <- imputesrcref::impute_package_srcrefs(
        p,
        include_internal = TRUE,
        verbose = FALSE
      )

      expect_gt(length(res$fn_names), 0L)

      # Every inspected function must either patch cleanly or be skipped for a
      # legitimate reason. A failure with any other message is a real bug.
      real_failures <- res$failed[
        !is.na(res$failed) & !(res$failed %in% skippable_failures)
      ]
      expect_equal(
        length(real_failures),
        0L,
        info = sprintf(
          "%s: unexpected failures: %s",
          p,
          paste(unique(real_failures), collapse = "; ")
        )
      )

      # If the package was installed without srcref retention there is nothing
      # to patch; skip the per-function correctness checks rather than fail.
      skip_if(
        res$patched_count == 0L,
        sprintf("%s installed without srcref retention; nothing patched", p)
      )

      # Deep-check a sample of patched functions. `impute_package_srcrefs` has
      # already replaced the namespace bindings with their imputed versions, so
      # `get()` returns the patched function.
      ns <- loadNamespace(p)
      patched_idx <- which(is.na(res$failed))
      sample_idx <- patched_idx[seq_len(min(sample_size, length(patched_idx)))]

      for (i in sample_idx) {
        nm <- res$fn_names[[i]]
        fn <- get(nm, envir = ns, inherits = FALSE)
        if (!is.function(fn) || is.primitive(fn)) {
          next
        }

        # Re-imputation must be a no-op: the transform is idempotent.
        again <- imputesrcref::impute_srcrefs(fn, quiet = TRUE)
        expect_identical(
          body(again), body(fn),
          info = sprintf("%s::%s is not idempotent (body)", p, nm)
        )
        expect_identical(
          formals(again), formals(fn),
          info = sprintf("%s::%s is not idempotent (formals)", p, nm)
        )

        # Every injected transparent brace must carry a srcref whose source
        # text matches the wrapped expression. This is the line-accuracy
        # guarantee: a wrong line/column would make the text mismatch.
        checks <- imputesrcref:::collect_transparent_srcref_checks(fn)
        expect_true(
          isTRUE(checks$ok),
          info = sprintf(
            "%s::%s srcref text mismatch at: %s",
            p, nm, paste(checks$failures, collapse = ", ")
          )
        )
      }
    })
  })
}
