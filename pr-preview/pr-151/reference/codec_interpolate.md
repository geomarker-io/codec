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
  functions (for example
  [`cincy_census_geo()`](http://geomarker.io/codec/reference/cincy_census_geo.md),
  [`cincy_neighborhood_geo()`](http://geomarker.io/codec/reference/cincy_neighborhood_geo.md),
  or
  [`cincy_zcta_geo()`](http://geomarker.io/codec/reference/cincy_zcta_geo.md))

- weights:

  which census block-level weights to use; see details

## Value

a tibble with a new geographic identifier column for the `to` target
geography (`geoid`) in addition to the interpolated columns in `from`

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
#>  1 Avondale      2024       0.511                  0.424                  0.808 
#>  2 Bond Hill     2024       0.181                  0.206                  0.587 
#>  3 CUF           2024       0.477                  0.0819                 0.407 
#>  4 California    2024       0.0228                 0.0306                 0.0665
#>  5 Camp Washin…  2024       0.282                  0.347                  0.691 
#>  6 Carthage      2024       0.0955                 0.298                  0.473 
#>  7 Clifton       2024       0.152                  0.0881                 0.221 
#>  8 College Hill  2024       0.261                  0.234                  0.463 
#>  9 Columbia Tu…  2024       0.06                   0.0531                 0.242 
#> 10 Corryville    2024       0.550                  0.0226                 0.255 
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
