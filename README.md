
# `fobitools` development version <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- [![Build Status](https://travis-ci.com/pcastellanoescuder/fobitools.svg?branch=devel)](https://travis-ci.com/pcastellanoescuder/fobitools) -->
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pcastellanoescuder/fobitools?branch=devel&svg=true)](https://ci.appveyor.com/project/pcastellanoescuder/fobitools)
[![Actions
Status](https://github.com/pcastellanoescuder/fobitools/workflows/R-CMD-check/badge.svg?branch=devel)](https://github.com/pcastellanoescuder/fobitools/actions)
[![Bioc devel
status](https://bioconductor.org/shields/build/devel/bioc/fobitools.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/fobitools/)
<!-- [![Codecov test coverage](https://codecov.io/gh/pcastellanoescuder/fobitools/branch/master/graph/badge.svg)](https://codecov.io/gh/pcastellanoescuder/fobitools?branch=master) -->
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/pcastellanoescuder/fobitools/badge)](https://www.codefactor.io/repository/github/pcastellanoescuder/fobitools) -->
<!-- [![Last Commit](https://img.shields.io/github/last-commit/pcastellanoescuder/fobitools.svg)](https://github.com/pcastellanoescuder/fobitools/commits/devel) -->
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://img.shields.io/badge/DOI-https%3A%2F%2Fdoi.org%2F10.1093%2Fdataba%2Fbaaa033-blue)](https://doi.org/10.1093/databa/baaa033)

<!-- badges: end -->

This package provides a set of tools for interacting with [FOBI
(Food-Biomarker
Ontology)](https://github.com/pcastellanoescuder/FoodBiomarkerOntology).
A collection of basic manipulation tools for biological significance
analysis, graphs, and text mining strategies for annotating nutritional
data.

  - Graph visualization of FOBI
  - Parse FOBI information from OBO to a readable table format
  - Compound ID conversion (among metabolite names, FOBI, ChemSpider,
    KEGG, PubChemCID, InChIKey, InChICode, and HMDB IDs)
  - Biological significance analysis via a classical over representation
    analysis
      - **Chemical class enrichment analysis**: Over representation
        analysis using FOBI chemical classes as sets
      - **Food enrichment analysis**: Over representation analysis using
        FOBI food groups as sets
  - Text mining function for annotating free-text dietary data

`fobitools` also offers a Shiny app version called
[fobitoolsGUI](https://github.com/pcastellanoescuder/fobitoolsGUI). This
application implements most of `fobitools` functions in an user-friendly
web interface and it’s available at
<http://webapps.nutrimetabolomics.com/fobitoolsGUI>.

# Installation

To install GitHub devel version:

``` r
# install.packages("devtools")
devtools::install_github("pcastellanoescuder/fobitools", ref = "devel")
```

## Citation

**Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart,
Cristina Andrés-Lacueva, Alex Sánchez-Pla**, *FOBI: an ontology to
represent food intake data and associate it with metabolomic data*,
Database, Volume 2020, 2020, baaa033. DOI:
<https://doi.org/10.1093/databa/baaa033>

# Code of Conduct

Please note that the ‘fobitools’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
