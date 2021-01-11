FROM r-base:3.6.0

MAINTAINER Pol Castellano-Escuder <polcaes@gmail.com>

LABEL authors = "polcaes@gmail.com" \
      description = "Docker image containing the fobitools R package latest version from GitHub"

# Install fobitools dependencies

## CRAN

RUN R -e "install.packages(c('clisymbols',
                             'crayon',
                             'dplyr',
                             'magrittr',
                             'ontologyIndex',
                             'purrr',
                             'RecordLinkage',
                             'sigora',
                             'stringr',
                             'textclean',
                             'tictoc',
                             'tidyr',
                             'vroom'), repos='http://cran.rstudio.com/')"

# Install fobitools from GitHub

RUN installGithub.r "pcastellanoescuder/fobitools"

COPY fobitools_*.tar.gz /app.tar.gz
RUN remotes::install_local('/app.tar.gz')
CMD R -e 'library(fobitools)'