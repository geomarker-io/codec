# CoDEC <img src="man/figures/logo.svg" align="right" height="200" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/codec/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of the R package {codec} is to support [CoDEC](https://geomarker.io/codec) data
infrastructure through

- defining the CoDEC data specifications
- creation and hosting of an interactive data catalog and explorer
- providing developer tools to add new CoDEC data resources
- providing an R-based interface for retreiving CoDEC data stored online

### Installation

You can install the latest version of codec from
[R-universe](https://geomarker-io.r-universe.dev/codec) in R with:

```r
install.packages("codec", repos = c("https://geomarker-io.r-universe.dev", "https://cloud.r-project.org"))
```

### Usage

See https://geomarker.io/codec/reference/index.html for reference pages on included functions.

### Codebase overview for developers

{codec} is a regular R package, so most functionality lives under `R/` and is documented in `man/` and on the pkgdown site. A few other directories are useful when getting up to speed:

- `inst/codec_catalog/` contains a Shiny app that lists available data packages and allows downloads.
- `inst/codec_data/` holds scripts that fetch outside datasets and convert them into CoDEC-compliant packages.
- `tests/testthat/` includes unit tests for geography helpers and interpolation utilities.
- `justfile` defines recipes to build the pkgdown site, release data packages, and export the catalog.

Reviewing these components is a good way to learn how new CoDEC datasets or features fit into the package.
