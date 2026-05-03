# CoDEC online data catalog

The CoDEC data catalog is shipped with the package and older versions
can be read from GitHub alongside the source code for this package.

- Use `codec_read()` as a shortcut to read a CoDEC table into R as a
  `codec_tbl` object (see
  [`?as_codec_tbl`](http://geomarker.io/codec/reference/as_codec_tbl.md))

- Use `codec_list()` as a shortcut to list available CoDEC table pins

- `codec_board()` can be used to create a pin board object (see
  [`?pins::pins`](https://pins.rstudio.com/reference/pins-package.html))
  based on a specific version of the codec package

## Usage

``` r
codec_read(
  name,
  board = codec_board(),
  to = NULL,
  weights = c("pop", "homes", "area"),
  include_geography = FALSE
)

codec_list(board = codec_board())

codec_board(
  version = paste0("v", utils::packageVersion("codec")),
  cache = NULL,
  use_cache_on_failure = rlang::is_interactive(),
  headers = NULL
)
```

## Arguments

- name:

  the name of the CoDEC table in the CoDEC data catalog.

- board:

  a pins board object; create with `codec_board()` to read from the
  bundled catalog or earlier versions of the catalog, or to change the
  caching behavior of the pins package

- to:

  optional target geography for interpolation; supply the output of
  [`cincy_census_geo()`](http://geomarker.io/codec/reference/cincy_census_geo.md),
  [`cincy_neighborhood_geo()`](http://geomarker.io/codec/reference/cincy_neighborhood_geo.md),
  or
  [`cincy_zcta_geo()`](http://geomarker.io/codec/reference/cincy_zcta_geo.md)
  to interpolate the table while reading

- weights:

  which census block-level weights to use when `to` is supplied; passed
  to
  [`codec_interpolate()`](http://geomarker.io/codec/reference/codec_interpolate.md)

- include_geography:

  logical; include the `s2_geography` column in the result? Defaults to
  `FALSE`

- version:

  specify a version of the online data catalog using a commit SHA, tag,
  or branch of geomarker-io/codec; uses the bundled board for the
  installed package version by default

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- use_cache_on_failure:

  If the pin fails to download, is it ok to use the last cached version?
  Defaults to `is_interactive()` so you'll be robust to poor internet
  connectivity when exploring interactively, but you'll get clear errors
  when the code is deployed.

- headers:

  Named character vector for additional HTTP headers (such as for
  authentication). See
  [`connect_auth_headers()`](https://pins.rstudio.com/reference/board_connect_url.html)
  for Posit Connect support.

## Value

For `codec_read()`, a `codec_tbl` by default, or an interpolated tibble
/ simple-features tibble when `to` or `include_geography` is used

For `codec_list()`, a character vector CoDEC table names

For `codec_board()`, a pins_board object

## Details

The pin for each CoDEC table has versions (see
[`?pins::pin_versions`](https://pins.rstudio.com/reference/pin_versions.html)),
but `codec_board()` can be used to specify a state of the online data
catalog based on the version of the codec package. (See examples)

## Examples

``` r
# list available CoDEC tables
codec_list()
#> [1] "acs_measures"                "crime"                      
#> [3] "drivetime"                   "environmental_justice_index"
#> [5] "landcover"                   "parcel"                     
#> [7] "property_code_enforcements"  "traffic"                    

# read a CoDEC table and inspect its metadata
d <- codec_read("traffic")
head(d)
#> # A tibble: 6 × 5
#>   census_tract_id_2020 aadtm_trucks_buses aadtm_tractor_trailer aadtm_passenger
#>   <chr>                             <dbl>                 <dbl>           <dbl>
#> 1 39061005200                          0                     0               0 
#> 2 39061026200                   53551460.            207033030.     1108175821.
#> 3 39061023901                   27090895.             39769940.      621615559.
#> 4 39061023701                          0                     0               0 
#> 5 39061010500                          0                     0               0 
#> 6 39061020501                   23210991.             50060424.      556823936.
#> # ℹ 1 more variable: year <int>
attr(d, "title")
#> [1] "Average Annual Vehicle-Meters Driven"
message(attr(d, "description"))
#> # Average Annual Vehicle-Meters Driven
#> 
#> ## About
#> 
#> Traffic is measured in AADTM or Annual Average Daily Traffic Meters, which is the average number of total meters driven by all vehicles per day when grouped into classes (trucks/buses, tractor/trailer, passenger).
#> 
#> For more details about the HPMS, see:
#> 
#> - <https://www.fhwa.dot.gov/policyinformation/hpms.cfm>
#> - <https://data-usdot.opendata.arcgis.com/datasets/usdot::highway-performance-monitoring-system-hpms-2020/about>
#> - <https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/hpms_field_manual_dec2016.pdf>
#> 
#> ## Data
#> 
#> Data is downloaded from the 2020 Highway Performance Monitoring System (HPMS) geodatabase hosted by ESRI using the {[appc](https://github.com/geomarker-io/appc)} package for R.
#> Only roads with `F_SYSTEM` classification of 1 ("interstate") or 2 ("principal arterial - other freeways and expressways") are used.
#> Passenger vehicles (FHWA 1-3) are calculated as the total minus FHWA class 4-7 (single unit) and 8-13 (combo).
#> 
#> For each 2020 census tract geography, sum the class-specific AADTM for all intersecting roads, weighted by their intersecting lengths.

# interpolate while reading
codec_read("acs_measures", to = cincy_neighborhood_geo())
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
codec_read("acs_measures", include_geography = TRUE)
#> Simple feature collection with 226 features and 24 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02153 xmax: -84.25633 ymax: 39.31206
#> Geodetic CRS:  WGS 84
#> # A tibble: 226 × 25
#>    census_tract_id_2020                                s2_geography n_households
#>    <chr>                                         <MULTIPOLYGON [°]>        <int>
#>  1 39061005200          (((-84.44284 39.14481, -84.44274 39.1448, …         1805
#>  2 39061026200          (((-84.82018 39.12086, -84.82015 39.12061,…          734
#>  3 39061023901          (((-84.36623 39.2497, -84.36565 39.24954, …         1912
#>  4 39061023701          (((-84.4088 39.20398, -84.40861 39.20397, …         1339
#>  5 39061010500          (((-84.69515 39.11356, -84.69376 39.11198,…          594
#>  6 39061020501          (((-84.71736 39.24415, -84.7171 39.24294, …         1291
#>  7 39061021900          (((-84.57231 39.20906, -84.57206 39.2088, …          543
#>  8 39061021102          (((-84.69025 39.12426, -84.68968 39.12428,…         2576
#>  9 39061003700          (((-84.49035 39.12595, -84.4881 39.1258, -…         1058
#> 10 39061003800          (((-84.48376 39.14202, -84.48368 39.14177,…          921
#> # ℹ 216 more rows
#> # ℹ 22 more variables: n_households_children <int>, n_housing_units <int>,
#> #   prop_poverty <dbl>, prop_recieved_public_assistance_income <dbl>,
#> #   prop_family_households_with_single_householder <dbl>,
#> #   prop_employment_among_civilian_workforce <dbl>,
#> #   prop_housing_units_occupied_by_renters <dbl>,
#> #   prop_median_rent_to_income_ratio_among_renters <dbl>, …

# inspect the bundled board or read from an older online version
codec_board() |>
  pins::pin_versions("crime")
#> # A tibble: 5 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20250829T181659Z-bf502 2025-08-29 18:16:59 bf502
#> 2 20250831T023010Z-bf502 2025-08-31 02:30:10 bf502
#> 3 20250901T033049Z-bf502 2025-09-01 03:30:49 bf502
#> 4 20250902T005725Z-bf502 2025-09-02 00:57:25 bf502
#> 5 20250902T182616Z-91afb 2025-09-02 18:26:16 91afb
codec_board("v3.0.0-rc1") |>
  pins::pin_versions("crime")
#> # A tibble: 4 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20250829T181659Z-bf502 2025-08-29 18:16:59 bf502
#> 2 20250831T023010Z-bf502 2025-08-31 02:30:10 bf502
#> 3 20250901T033049Z-bf502 2025-09-01 03:30:49 bf502
#> 4 20250902T005725Z-bf502 2025-09-02 00:57:25 bf502
```
