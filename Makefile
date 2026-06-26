R = R
RSCRIPT = Rscript

# Library holding the corpus packages installed WITH srcref retention, used by
# the FULL_TEST whole-package tests. Prepended to .libPaths() for `test-full`
# and `corpus-install`. Override with: make test-full CORPUS_LIB=/path/to/lib
CORPUS_LIB ?= $(HOME)/Rlib_test

# Packages exercised by the corpus tests (must be installed with srcref).
CORPUS_PKGS = 'data.table','dplyr','fs','ggplot2','glue','jsonlite','stringr','zoo'

.PHONY: install test test-full corpus-install clean

install:
	$(R) CMD INSTALL .

# Fast suite: blacklist + snapshot tests. Corpus tests skip (no FULL_TEST).
test:
	$(RSCRIPT) -e "testthat::test_dir('tests/testthat', load_package = 'source')"

# Full suite including the slow whole-package corpus tests. Requires the corpus
# installed with srcref retention (run `make corpus-install` once first).
test-full:
	FULL_TEST=1 $(RSCRIPT) -e ".libPaths(c('$(CORPUS_LIB)', .libPaths())); testthat::test_dir('tests/testthat', load_package = 'source')"

# Install the corpus packages from source with srcref retained into CORPUS_LIB.
corpus-install:
	mkdir -p "$(CORPUS_LIB)"
	$(RSCRIPT) -e ".libPaths(c('$(CORPUS_LIB)', .libPaths())); install.packages(c($(CORPUS_PKGS)), lib='$(CORPUS_LIB)', INSTALL_opts='--with-keep.source', type='source', dependencies=c('Depends','Imports','LinkingTo'))"

clean:
	rm -rf *.tar.gz *.Rcheck
