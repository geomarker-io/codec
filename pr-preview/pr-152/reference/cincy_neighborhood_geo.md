# Cincy neighborhood geographies

CAGIS data (see
[`install_cagis_data()`](http://geomarker.io/codec/reference/install_cagis_data.md))
provides community council boundaries, but these boundaries can overlap
and do not align with census geographies or ZIP codes. By default, the
statistical neighborhood approximations are instead returned, which are
calculated by aggregating census tracts into 50 matching neighborhoods.

## Usage

``` r
cincy_neighborhood_geo(
  geography = c("statistical_neighborhood_approximations", "community_council"),
  packaged = TRUE
)
```

## Arguments

- geography:

  which type of cincy neighborhood geography to return

- packaged:

  logical; use the data included with the package instead of
  (down)loading from the source data?

## Value

a simple features object with a geographic identifier column (`geoid`)
and a geometry column (`s2_geography`)

## Examples

``` r
cincy_neighborhood_geo()
#> Simple feature collection with 50 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.71216 ymin: 39.05206 xmax: -84.36954 ymax: 39.22101
#> Geodetic CRS:  WGS 84
#> # A tibble: 50 × 2
#>    geoid                                                            s2_geography
#>    <chr>                                                      <MULTIPOLYGON [°]>
#>  1 South Fairmount (((-84.57299 39.13404, -84.57322 39.13188, -84.57328 39.1313…
#>  2 Roselawn        (((-84.45447 39.18302, -84.45447 39.18307, -84.45462 39.1830…
#>  3 Mt. Lookout     (((-84.41474 39.13997, -84.41493 39.13998, -84.41501 39.1392…
#>  4 Westwood        (((-84.56772 39.15794, -84.56827 39.15795, -84.56888 39.1580…
#>  5 Mt. Adams       (((-84.49349 39.10796, -84.4936 39.10808, -84.49359 39.10824…
#>  6 Millvale        (((-84.54839 39.13769, -84.54846 39.13777, -84.54849 39.1378…
#>  7 North Fairmount (((-84.56189 39.14178, -84.56229 39.14177, -84.5628 39.1418,…
#>  8 Sayler Park     (((-84.68073 39.10714, -84.68074 39.10715, -84.68075 39.1071…
#>  9 Carthage        (((-84.47734 39.20546, -84.47914 39.20546, -84.47934 39.2049…
#> 10 Pendleton       (((-84.5002 39.11306, -84.50034 39.11308, -84.50048 39.1131,…
#> # ℹ 40 more rows
```
