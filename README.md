
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
d <- d |>
  add_attrs(
    name = "mydata",
    title = "My Data",
    license = "MIT",
    url = "https://geomarker.io/CODECtools"
  )
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
| url     | <https://geomarker.io/CODECtools> |
| license | MIT                               |

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

Automatically add `name`, `type` and `enum` schema to each column in the
data based on their class:

``` r
d <- add_type_attrs(d)
```

Like for descriptors, there is a helper function to retrieve schema as a
tibble. Specify `bind = TRUE` to combine the list of column-specific
attribute data frames for each column into one wider data frame:

``` r
get_schema(d, bind = TRUE) |>
  knitr::kable()
```

| col     | name    | title      | description                           | type    | constraints         |
|:--------|:--------|:-----------|:--------------------------------------|:--------|:--------------------|
| id      | id      | Identifier | unique identifier                     | string  | NULL                |
| date    | date    | Date       | date of observation                   | date    | NULL                |
| measure | measure | Measure    | measured quantity                     | number  | NULL                |
| rating  | rating  | Rating     | ordered ranking of observation        | string  | good , better, best |
| ranking | ranking | Ranking    | rank of the observation               | integer | NULL                |
| impt    | impt    | Important  | true if this observation is important | boolean | NULL                |

Once metadata is set in the tibble’s attributes, we can save the tabular
data resource as a CSV file with an accompanying
tabular-data-resource.yaml:

``` r
write_tdr_csv(d)
#> ✔ created '/Users/broeg1/code/CODECtools/mydata'/
#> ✔ wrote data to '/Users/broeg1/code/CODECtools/mydata/mydata.csv'
#> ✔ wrote metadata to '/Users/broeg1/code/CODECtools/mydata/tabular-data-resource.yaml'
```

The `name` attribute of the supplied tibble is used as the name of a
newly created folder *and* CSV file containing the data. Metadata
extracted from the supplied tibble’s attributes is saved in a
`tabular-data-resource.yaml` file that lives alongside the data file in
the newly created directory:

``` r
fs::dir_tree("mydata")
#> mydata
#> ├── mydata.csv
#> └── tabular-data-resource.yaml
```

We can then read this tabular-data-package back into R and restore its
attributes, as well as its column classes:

``` r
mydata <- read_tdr_csv("mydata")
#> ✔ read in data from 'mydata/mydata.csv'
#> Warning: Outer names are only allowed for unnamed scalar atomic inputs
mydata
#> # A tibble: 3 × 7
#>   id    date       measure rating ranking impt  field
#>   <chr> <date>       <dbl> <fct>    <int> <lgl> <chr>
#> 1 A01   2022-07-25    12.8 good        14 FALSE A01  
#> 2 A02   2018-07-10    13.9 best        17 TRUE  A02  
#> 3 A03   2013-08-15    15.6 best        19 TRUE  A03
```
