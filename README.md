# CoDEC <img src="man/figures/logo.svg" align="right" height="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of the R package {codec} is to support [CoDEC](https://geomarker.io/codec) data
infrastructure through

- defining the CoDEC tabular-data-resource (TDR) specifications: `vignette("specs")`
- providing tools to check TDRs against the CoDEC specifications
- creation and hosting of an interactive data catalog and explorer: `vignette("data")`
- providing an R-based interface for retreiving CoDEC data (`codec_data()`)

### Installation

You can install the development version of codec from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("geomarker-io/codec")
