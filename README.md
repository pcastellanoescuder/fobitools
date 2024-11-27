
# `fobitools` <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Actions
Status](https://github.com/pcastellanoescuder/fobitools/workflows/R-CMD-check/badge.svg)](https://github.com/pcastellanoescuder/fobitools/actions)
[![Bioc release
status](https://bioconductor.org/shields/build/release/bioc/fobitools.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/fobitools/)
[![Bioc devel
status](https://bioconductor.org/shields/build/devel/bioc/fobitools.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/fobitools/)
[![Last
Commit](https://img.shields.io/github/last-commit/pcastellanoescuder/fobitools.svg)](https://github.com/pcastellanoescuder/fobitools/commits/main)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://img.shields.io/badge/DOI-https%3A%2F%2Fdoi.org%2F10.1093%2Fdataba%2Fbaaa033-blue)](https://doi.org/10.1093/databa/baaa033)

<!-- badges: end -->

This package provides a set of tools for interacting with [FOBI
(Food-Biomarker
Ontology)](https://github.com/pcastellanoescuder/FoodBiomarkerOntology).
A collection of basic manipulation tools for biological significance
analysis, graph visualization and text mining strategies for annotating
nutritional data are provided here:

- Graph visualization of FOBI
- Parse FOBI information from OBO to a readable table format
- Compound ID conversion (among metabolite names, FOBI, ChemSpider,
  KEGG, PubChemCID, InChIKey, InChICode and HMDB IDs)
- Biological significance analysis via ORA and MSEA methods:
  - Chemical class enrichment analysis: ORA and MSEA using FOBI chemical
    classes as metabolite sets
  - Food enrichment analysis: ORA and MSEA using FOBI food groups as
    metabolite sets
- Text mining algorithm for annotating free-text dietary data

`fobitools` also offers a Shiny app version called
[fobitoolsGUI](https://github.com/pcastellanoescuder/fobitoolsGUI). This
application implements most of `fobitools` functions in an user-friendly
web interface and it is available at
<http://webapps.nutrimetabolomics.com/fobitoolsGUI>.

# Installation

To install Bioconductor version use:

``` r
# install.packages("BiocManager")
BiocManager::install("fobitools")
```

If you need the GitHub version (not recommended unless you know what you
are doing), use:

``` r
# install.packages("devtools")
devtools::install_github("pcastellanoescuder/fobitools")
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
