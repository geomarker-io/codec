# Coerce a CoDEC data table into a simple features object

The name of the census tract column in the CoDEC data table is used to
add the appropriate census tract s2 geography column.

## Usage

``` r
codec_as_sf(x)
```

## Arguments

- x:

  a CoDEC data table

## Value

a simple features object with a geometry column (`s2_geography`) in
addition to the columns in `x`

## Details

Tract identifiers do not change across decennial censuses, but the
digital representation of their boundaries may be improved over time.
Here, data tables using 2010 tract identifiers use the TIGER/Line 2019
tract shapefiles and data tables using 2020 tract identifiers use the
TIGER/Line 2020 tract shapefiles.

## Examples

``` r
codec_as_sf(codec_read("property_code_enforcements"))
#> Simple feature collection with 2034 features and 3 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02153 xmax: -84.25633 ymax: 39.31206
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,034 × 4
#>    census_tract_id_2020                s2_geography  year n_property_code_enfo…¹
#>    <chr>                         <MULTIPOLYGON [°]> <int>                  <int>
#>  1 39061005200          (((-84.44284 39.14481, -84…  2017                     64
#>  2 39061005200          (((-84.44284 39.14481, -84…  2018                     71
#>  3 39061005200          (((-84.44284 39.14481, -84…  2019                     71
#>  4 39061005200          (((-84.44284 39.14481, -84…  2020                     54
#>  5 39061005200          (((-84.44284 39.14481, -84…  2021                     60
#>  6 39061005200          (((-84.44284 39.14481, -84…  2022                     83
#>  7 39061005200          (((-84.44284 39.14481, -84…  2023                    413
#>  8 39061005200          (((-84.44284 39.14481, -84…  2024                     87
#>  9 39061005200          (((-84.44284 39.14481, -84…  2025                      0
#> 10 39061026200          (((-84.82018 39.12086, -84…  2017                      0
#> # ℹ 2,024 more rows
#> # ℹ abbreviated name: ¹​n_property_code_enforcements
```
