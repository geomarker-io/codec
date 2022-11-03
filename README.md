# CODECtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of CODECtools is to support CODEC data infrastructure through:

- [x] capturing, creating, and accessing metadata for tabular data in R
- [x] importing and exporting tabular data resources (CSV file) with metadata (YAML file)
- [ ] defining CODEC data format specifications (as vignette in package)
- [ ] providing tools and workflows to validate CODEC data and contribute it to CODECdata repository
- [ ] creation of online data catalog

## Installation

You can install the development version of CODECtools from [GitHub](https://github.com/) with:

```
# install.packages("devtools")
devtools::install_github("geomarker-io/CODECtools")
```

#### Related Projects

- [{frictionless}](https://docs.ropensci.org/frictionless/index.html) is an R package for reading and writing frictionless data *packages*
- [Frictionless Web Tool](https://create.frictionlessdata.io/) for creating and validating frictionless data packages
- [Frictionless Framework](https://framework.frictionlessdata.io/docs/guides/introduction) for python
- [Frictionless Validation](https://repository.frictionlessdata.io/) are github actions for validation, visual reports, and markdown badges
- [CSV on the Web](https://w3c.github.io/csvw/syntax/) is a standard for tabular data in CSV files including metadata; the [{csvrw} package](https://robsteranium.github.io/csvwr/index.html) provides R bindings 
