# Cincy address geographies

CAGIS data (see
[`install_cagis_data()`](http://geomarker.io/codec/reference/install_cagis_data.md))
provides a list of all addresses in Hamilton County. The s2 cell is
derived from LONGITUDE and LATITUDE fields in the CAGIS address
database.

## Usage

``` r
cincy_addr_geo(packaged = TRUE)
```

## Arguments

- packaged:

  logical; use the data included with the package instead of
  (down)loading from the source data?

## Value

a simple features object with columns `cagis_address`,
`cagis_address_place`, `cagis_address_type`, `cagis_s2`,
`cagis_parcel_id`, `cagis_is_condo`, and a geometry column
(`s2_geography`)

## Details

This function previously excluded addresses with type milemarker (`MM`),
park (`PAR`), infrastructure project (`PRJ`), cell tower (`CTW`), vacant
or commercial lot (`LOT`), and other miscellaneous non-residential
addresses (`MIS`, `RR`, `TBA`). Now that they are included, see the
examples for how to filter these out.

## Examples

``` r
cincy_addr_geo() |>
  dplyr::filter(
    !cagis_address_type %in% c(
      "MM", "PAR", "PRJ", "CTW",
      "LOT", "MIS", "RR", "TBA"
    )
  )
#> Simple feature collection with 328745 features and 8 fields (with 1286 geometries empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -84.82008 ymin: 39.02659 xmax: -84.08805 ymax: 39.43353
#> Geodetic CRS:  WGS 84 (CRS84)
#> # A tibble: 328,745 × 9
#>    cagis_address      cagis_address_status cagis_orphan_flag cagis_address_place
#>  * <chr>              <chr>                <chr>             <chr>              
#>  1 9483 ZOLA CT HARR… ASSIGNED             N                 NA                 
#>  2 208 MOHAWK ST CIN… ASSIGNED             N                 NA                 
#>  3 11800 READING RD … USING                N                 U-HAUL - 11800 REA…
#>  4 12050 PRINCETON P… ASSIGNED             N                 STAPLES SRPINGDALE 
#>  5 737 US 50 MILFORD… REGISTERED           NA                NA                 
#>  6 6610 PFEIFFER RD … USING                N                 NA                 
#>  7 6501 PFEIFFER RD … USING                N                 NA                 
#>  8 4244 ROUND BOTTOM… USING                N                 NA                 
#>  9 8210 MARKET PLACE… USING                N                 NA                 
#> 10 8212 MARKET PLACE… USING                N                 NA                 
#> # ℹ 328,735 more rows
#> # ℹ 5 more variables: cagis_address_type <chr>, cagis_s2 <s2_cell>,
#> #   cagis_parcel_id <chr>, cagis_is_condo <lgl>, s2_geography <POINT [°]>
```
