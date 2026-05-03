# Cincy ZIP Code Tabulation Areas

Read ZIP Code Tabulation Areas (ZCTAs) geographies from the online
Census TIGER/Line files into R.

## Usage

``` r
cincy_zcta_geo(vintage = as.character(2024:2013), packaged = TRUE)
```

## Arguments

- vintage:

  a character vector of a year corresponding to the vintage of
  TIGER/Line data

- packaged:

  logical; use the data included with the package instead of
  (down)loading from the source data?

## Value

a simple features object with a geographic identifier column (`geoid`)
and a geometry column (`s2_geography`)

## Examples

``` r
cincy_zcta_geo("2020")
#> Simple feature collection with 55 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02153 xmax: -84.25364 ymax: 39.31774
#> Geodetic CRS:  WGS 84
#> # A tibble: 55 × 2
#>    geoid                                                            s2_geography
#>    <chr>                                                      <MULTIPOLYGON [°]>
#>  1 45214 (((-84.5776 39.12704, -84.57744 39.12706, -84.57669 39.12693, -84.5754…
#>  2 45208 (((-84.46471 39.13139, -84.46451 39.13149, -84.4636 39.13091, -84.4641…
#>  3 45236 (((-84.43599 39.21383, -84.43431 39.21381, -84.4337 39.21382, -84.4338…
#>  4 45247 (((-84.71736 39.24415, -84.7171 39.24294, -84.71647 39.24154, -84.716 …
#>  5 45030 (((-84.82016 39.22722, -84.82015 39.22046, -84.82015 39.21871, -84.820…
#>  6 45225 (((-84.57444 39.15123, -84.57144 39.15097, -84.57159 39.15005, -84.571…
#>  7 45252 (((-84.66512 39.29226, -84.66489 39.29185, -84.66425 39.29124, -84.663…
#>  8 45205 (((-84.59682 39.10452, -84.59541 39.1044, -84.59507 39.10417, -84.5947…
#>  9 45220 (((-84.53908 39.15065, -84.53906 39.15054, -84.53893 39.15014, -84.538…
#> 10 45244 (((-84.4016 39.10921, -84.40148 39.10909, -84.40137 39.10895, -84.4011…
#> # ℹ 45 more rows
```
