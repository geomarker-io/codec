
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CODECtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of CODECtools is to support CODEC data infrastructure through:

-   capturing, creating, and accessing metadata for tabular data in R
-   importing and exporting tabular data resources (CSV file) with
    metadata (YAML file)
-   defining CODEC data format specifications (as vignette in package)
-   providing tools and workflows to validate CODEC data and contribute
    it to CODECdata repository
-   creation of online data catalog

## Installation

You can install the development version of CODECtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("geomarker-io/CODECtools")
```

## A Simple Metadata Example

``` r
library(CODECtools)
```

We will create a simple dataset here for this example:

``` r
d <-
  tibble::tibble(
    id = c("A01", "A02", "A03"),
    date = as.Date(c("2022-07-25", "2018-07-10", "2013-08-15")),
    measure = c(12.8, 13.9, 15.6),
    rating = factor(c("good", "best", "best"), levels = c("good", "better", "best")),
    ranking = as.integer(c(14, 17, 19)),
    impt = c(FALSE, TRUE, TRUE)
  )
```

When creating a tabular dataset in R, data-specific metadata (i.e.,
“descriptors”) can be stored in the attributes of the R object (e.g., a
data.frame or tibble).

``` r
d <- add_attrs(d, name = "mydata", title = "My Data", license = "MIT", url = "https://geomarker.io/CODECtools")
```

Note that this doesn’t change any of the data values. In R, an object’s
attributes are stored with it as a list. Some attributes (`?attributes`)
are treated specially by R (e.g., `class`, `names`, `row.names`,
`comment`) and usually shouldn’t be modified. Although *all* attributes
(including the ones we added above) are available as a list
(`?attributes`), we can use a function to extract only the attributes
that represent metadata descriptors as a tibble.

``` r
get_descriptors(d) |>
  knitr::kable()
```

| name    | value                             |
|:--------|:----------------------------------|
| name    | mydata                            |
| title   | My Data                           |
| license | MIT                               |
| url     | <https://geomarker.io/CODECtools> |

Similarly, we can add column-specific attributes (i.e., “schema”). These
metadata functions follow the tidy design principles, making it simple
to expressively and concisely add metadata using pipes:

``` r
d <-
  d |>
  add_col_attrs(id, title = "Identifier", description = "unique identifier") |>
  add_col_attrs(date, title = "Date", description = "date of observation") |>
  add_col_attrs(measure, title = "Measure", description = "measured quantity") |>
  add_col_attrs(rating, title = "Rating", description = "ordered ranking of observation") |>
  add_col_attrs(ranking, title = "Ranking", description = "rank of the observation") |>
  add_col_attrs(impt, title = "Important", description = "true if this observation is important")
```

Automagically add `name`, `type` and `enum` schema to each column in the
data [based on their
class](link%20to%20where%20this%20is%20documented%20--%20vignette?)

``` r
d <- add_type_attrs(d)
```

Like for descriptors, there is a helper function to retrieve schema as a
tibble. This is returned as a list with a separate tibble for each
column:

``` r
get_schema(d) |>
  knitr::kable()
```

| name        | value             |
|:------------|:------------------|
| title       | Identifier        |
| description | unique identifier |
| type        | string            |

| name        | value               |
|:------------|:--------------------|
| title       | Date                |
| description | date of observation |
| type        | date                |

| name        | value             |
|:------------|:------------------|
| title       | Measure           |
| description | measured quantity |
| type        | number            |

| name        | value                          |
|:------------|:-------------------------------|
| title       | Rating                         |
| description | ordered ranking of observation |
| constraints | c(“good”, “better”, “best”)    |
| type        | string                         |

| name        | value                   |
|:------------|:------------------------|
| title       | Ranking                 |
| description | rank of the observation |
| type        | integer                 |

| name        | value                                 |
|:------------|:--------------------------------------|
| title       | Important                             |
| description | true if this observation is important |
| type        | boolean                               |

Once our metadata is set correctly, we can save our tabular data
resource as a YAML file:

But still need to add path….

``` r
save_tdr(d)

## yaml::yaml.load_file("tabular-data-resource.yaml")
```

We also need to save our CSV file using `readr::write_csv()` since it
takes care of all of the CSV defaults for us?? (so we don’t have to
specify those in the schema – just assume the defaults always)

This allows others to read in a CSV file in R and use the metadata file
to properly define the classes (and levels) of the columns:

, saving it to disk, and reading it back into R with the metadata:
