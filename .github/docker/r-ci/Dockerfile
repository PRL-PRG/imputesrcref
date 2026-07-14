ARG R_VERSION=4.5.2
FROM rocker/r-ver:${R_VERSION}

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    git \
    make \
    gcc \
    g++ \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libuv1-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "options(\
  repos = c(CRAN = 'https://cloud.r-project.org'),\
  keep.source.pkgs = TRUE,\
  keep.parse.data.pkgs = TRUE,\
  Ncpus = max(1L, parallel::detectCores() - 1L)\
); pkgs <- c('ggplot2', 'dplyr', 'stringr', 'testthat'); install.packages(\
  pkgs,\
  dependencies = NA,\
  INSTALL_opts = c('--with-keep.source', '--with-keep.parse.data')\
); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing)) stop('Failed to install package(s): ', paste(missing, collapse = ', '), call. = FALSE)"

WORKDIR /work
