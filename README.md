# CoDEC <img src="man/figures/logo.svg" align="right" height="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of the R package {codec} is to support [CoDEC](https://geomarker.io/codec) data
infrastructure through:

- curating metadata for tabular data in R:
  `vignette("curating-metadata")`
- reading and writing tabular-data-resources:
  `vignette("reading-writing-tdr")`
- defining the CoDEC tabular-data-resource specifications:
  `vignette("codec-specs")`
- providing tools to check CoDEC tabular-data-resources and create an
  interactive data catalog: `vignette("data")`

### Installation

You can install the development version of codec from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("geomarker-io/codec")
