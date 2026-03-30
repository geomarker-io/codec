# CoDEC online data catalog

The CoDEC online data catalog is hosted on GitHub alongside the source
code for this package.

- Use `codec_read()` as a shortcut to read a CoDEC table into R as a
  codec_tbl object (see
  [`?as_codec_tbl`](http://geomarker.io/codec/reference/as_codec_tbl.md))

- Use `codec_list()` as a shortcut to list available CoDEC table pins

- `codec_board()` can be used to create a pin board object (see
  [`?pins::pins`](https://pins.rstudio.com/reference/pins-package.html))
  based on a specific version of the codec package

## Usage

``` r
codec_read(name, board = codec_board())

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

  The name of the CoDEC table in the online CoDEC data catalog.

- board:

  a pins board object; create with `codec_board()` to read from earlier
  versions of the catalog or to change the caching behavior of the pins
  package

- version:

  specify a version of the online data catalog using a commit SHA, tag,
  or branch of geomarker-io/codec; syncs with the version of the
  installed package by default

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

For `codec_read()`, a codec_tbl object (see
[`as_codec_tbl()`](http://geomarker.io/codec/reference/as_codec_tbl.md))

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
#> AADT stands for **A**verage **A**nual **D**aily **T**raffic.
#> Aggregated at the census tract-level, AADT is measured in vehicle-meter counts (`aadtm`) and grouped by class (`passenger`, `trucks_buses`, `tractor_trailer`).
#> 
#> Data is downloaded from the 2020 Highway Performance Monitoring System (HPMS) geodatabase hosted by ESRI using the {[appc](https://github.com/geomarker-io/appc)} package for R.
#> For more details about the HPMS, see:
#> 
#> - <https://www.fhwa.dot.gov/policyinformation/hpms.cfm>
#> - <https://data-usdot.opendata.arcgis.com/datasets/usdot::highway-performance-monitoring-system-hpms-2020/about>
#> - <https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/hpms_field_manual_dec2016.pdf>
#> 
#> ## Data
#> 
#> Only roads with `F_SYSTEM` classification of 1 ("interstate") or 2 ("principal arterial - other freeways and expressways") are used.
#> Passenger vehicles (FHWA 1-3) are calculated as the total minus FHWA class 4-7 (single unit) and 8-13 (combo).
#> 
#> For each 2020 census tract geography, sum the class-specific AADT for all intersecting roads, weighted by their intersecting lengths.

# change the defaults for codec_board() to read from older versions of the board
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
