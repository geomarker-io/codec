# Cincy census tracts and block groups

Read tract and block group ("bg") geographies from the online Census
TIGER/Line files into R.

## Usage

``` r
cincy_census_geo(
  geography = c("tract", "bg"),
  vintage = as.character(2024:2013),
  packaged = TRUE
)
```

## Arguments

- geography:

  which type of cincy census geography to return

- vintage:

  a character vector of a year corresponding to the vintage of
  TIGER/Line data

- packaged:

  logical; use the data included with the package instead of
  (down)loading from the source data?

## Value

a simple features object with a geographic identifier column (`geoid`)
and a geometry column (`s2_geography`)

## Details

Compressed shapefiles are downloaded from TIGER into an R user data
directory and will be cached for use across other R sessions (see
[`?dpkg::stow`](http://geomarker.io/codec/reference/stow.md) for more
details).

## Examples

``` r
cincy_census_geo("tract", "2020")
#> Simple feature collection with 226 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02153 xmax: -84.25633 ymax: 39.31206
#> Geodetic CRS:  WGS 84
#> # A tibble: 226 × 2
#>    geoid                                                            s2_geography
#>    <chr>                                                      <MULTIPOLYGON [°]>
#>  1 39061005200 (((-84.44284 39.14481, -84.44274 39.1448, -84.44126 39.14469, -8…
#>  2 39061026200 (((-84.82018 39.12086, -84.82015 39.12061, -84.82013 39.12041, -…
#>  3 39061023901 (((-84.36623 39.2497, -84.36565 39.24954, -84.36539 39.24947, -8…
#>  4 39061023701 (((-84.4088 39.20398, -84.40861 39.20397, -84.40654 39.20382, -8…
#>  5 39061010500 (((-84.69515 39.11356, -84.69376 39.11198, -84.69068 39.10849, -…
#>  6 39061020501 (((-84.71736 39.24415, -84.7171 39.24294, -84.71647 39.24154, -8…
#>  7 39061021900 (((-84.57231 39.20906, -84.57206 39.2088, -84.57038 39.20708, -8…
#>  8 39061021102 (((-84.69025 39.12426, -84.68968 39.12428, -84.68907 39.1242, -8…
#>  9 39061003700 (((-84.49035 39.12595, -84.4881 39.1258, -84.4879 39.12578, -84.…
#> 10 39061003800 (((-84.48376 39.14202, -84.48368 39.14177, -84.48359 39.14158, -…
#> # ℹ 216 more rows
cincy_census_geo("bg", "2020")
#> Simple feature collection with 678 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02153 xmax: -84.25633 ymax: 39.31206
#> Geodetic CRS:  WGS 84
#> # A tibble: 678 × 2
#>    geoid                                                            s2_geography
#>    <chr>                                                           <POLYGON [°]>
#>  1 390610234002 ((-84.44907 39.18265, -84.44782 39.1826, -84.44663 39.18253, -8…
#>  2 390610233003 ((-84.45327 39.19248, -84.45189 39.19238, -84.44937 39.19221, -…
#>  3 390610239023 ((-84.36462 39.23666, -84.35873 39.23576, -84.359 39.23292, -84…
#>  4 390610234001 ((-84.45455 39.18374, -84.45453 39.18351, -84.45451 39.18331, -…
#>  5 390610232103 ((-84.42632 39.21717, -84.42624 39.21714, -84.42566 39.21705, -…
#>  6 390610235225 ((-84.37768 39.22546, -84.37679 39.22557, -84.37626 39.22564, -…
#>  7 390610236001 ((-84.39897 39.20947, -84.39885 39.20947, -84.39546 39.20917, -…
#>  8 390610236003 ((-84.40499 39.21031, -84.40497 39.21022, -84.40494 39.21014, -…
#>  9 390610236004 ((-84.42186 39.21725, -84.42111 39.21723, -84.41939 39.21719, -…
#> 10 390610237011 ((-84.39489 39.20311, -84.3915 39.2028, -84.38788 39.20249, -84…
#> # ℹ 668 more rows
```
