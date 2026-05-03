# Convert a data frame, name, and description into a CoDEC table

**CoDEC Specifications:**

1.  The data must include a census tract identifier column (i.e.,
    `census_tract_id_2010` or `census_tract_id_2020`). The column must
    contain 11-digit GEOID identifiers for every census tract in
    Hamilton County, OH.

2.  Data must be structured in a tidy format such that each row is an
    observation for a specific census tract at a specific year (and
    month). This means that the data includes a year column (`year`), an
    integer year representing the vintage of the data (e.g. `2021`). The
    data can optionally include a month column (`month`), an integer
    month of the year.

3.  The name must only contain lower case alphanumeric characters, `-`,
    or `_`

4.  The description should be markdown text and the first line must
    contain the title of the CoDEC data table as a level one header
    (e.g., `# My Community Data`). Titles must be less than 80
    characters.

## Usage

``` r
as_codec_tbl(x, name, description = character())
```

## Arguments

- x:

  data.frame or tibble meeting CoDEC data specifications above

- name:

  name of CoDEC table

- description:

  markdown text describing the CoDEC table

## Value

a codec_tbl object

## Examples

``` r
tibble::tibble(
  census_tract_id_2020 = cincy_census_geo("tract", "2020")$geoid,
  n_things = 823,
  year = 2024L
) |>
  as_codec_tbl(
    name = "n_things",
    paste0(
      "# Number of Things\n",
      "Number of things were averaged by census tract ",
      "using the survey from 2024"
    )
  )
#> # A tibble: 226 × 3
#>    census_tract_id_2020 n_things  year
#>    <chr>                   <dbl> <int>
#>  1 39061005200               823  2024
#>  2 39061026200               823  2024
#>  3 39061023901               823  2024
#>  4 39061023701               823  2024
#>  5 39061010500               823  2024
#>  6 39061020501               823  2024
#>  7 39061021900               823  2024
#>  8 39061021102               823  2024
#>  9 39061003700               823  2024
#> 10 39061003800               823  2024
#> # ℹ 216 more rows
```
