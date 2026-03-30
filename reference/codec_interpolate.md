# Spatially interpolate community-level data

Census block-level weights are used to spatially interpolate CoDEC data
packages from the census tract-level to other Cincy geographies.

## Usage

``` r
codec_interpolate(from, to, weights = c("pop", "homes", "area"))
```

## Arguments

- from:

  a CoDEC data package

- to:

  A simple features object returned by one of the `cincy_*_geo()`
  functions (i.e.,
  [`cincy_census_geo()`](http://geomarker.io/codec/reference/cincy_census_geo.md),
  [`cincy_neighborhood_geo()`](http://geomarker.io/codec/reference/cincy_neighborhood_geo.md),
  or cincy_zcta_geo()\`)

- weights:

  which census block-level weights to use; see details

## Value

a tibble with a new geographic identifier column for the `to` target
geography (`geoid`) in addition to the (interpolated) columns in `from`

## Details

Block-level total population (`pop`), total number of homes (`homes`),
or total land area (`area`) from the 2020 Census can be chosen to use
for the weights. Geospatial intersection happens after transforming
geographies to epsg:5072. See
[`codec_as_sf()`](http://geomarker.io/codec/reference/codec_as_sf.md)
for adding geography to a CoDEC data package. Variables beginning with
"n\_" are interpolated using a weighted sum; all other variables are
interpolated using a weighted mean.

## Examples

``` r
codec_interpolate(codec_read("acs_measures"),
                  cincy_neighborhood_geo())
#> # A tibble: 51 × 24
#>    geoid         year prop_poverty prop_recieved_public…¹ prop_family_househol…²
#>    <chr>        <int>        <dbl>                  <dbl>                  <dbl>
#>  1 Avondale      2023       0.468                  0.440                  0.784 
#>  2 Bond Hill     2023       0.167                  0.179                  0.533 
#>  3 CUF           2023       0.477                  0.0616                 0.470 
#>  4 California    2023       0.0142                 0.031                  0.0668
#>  5 Camp Washin…  2023       0.298                  0.340                  0.540 
#>  6 Carthage      2023       0.0675                 0.200                  0.554 
#>  7 Clifton       2023       0.148                  0.0573                 0.253 
#>  8 College Hill  2023       0.217                  0.210                  0.460 
#>  9 Columbia Tu…  2023       0.0307                 0.0594                 0.204 
#> 10 Corryville    2023       0.536                  0.0278                 0.288 
#> # ℹ 41 more rows
#> # ℹ abbreviated names: ¹​prop_recieved_public_assistance_income,
#> #   ²​prop_family_households_with_single_householder
#> # ℹ 19 more variables: prop_employment_among_civilian_workforce <dbl>,
#> #   prop_housing_units_occupied_by_renters <dbl>,
#> #   prop_median_rent_to_income_ratio_among_renters <dbl>,
#> #   prop_housing_units_vacant <dbl>, …
codec_interpolate(codec_read("property_code_enforcements"),
                  cincy_census_geo("tract", "2019"))
#> # A tibble: 1,998 × 3
#>    geoid        year n_property_code_enforcements
#>    <chr>       <int>                        <dbl>
#>  1 39061000200  2017                            2
#>  2 39061000200  2018                            6
#>  3 39061000200  2019                           10
#>  4 39061000200  2020                            0
#>  5 39061000200  2021                            8
#>  6 39061000200  2022                           19
#>  7 39061000200  2023                           30
#>  8 39061000200  2024                           11
#>  9 39061000200  2025                            0
#> 10 39061000700  2017                           65
#> # ℹ 1,988 more rows
```
