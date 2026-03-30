# Install CAGIS GIS database

This installs the CAGIS Open Data GIS database (`.gdb`) into the data
directory for the codec package. Once downloaded, it will be reused
across R sessions on the same computer. The geodatabase contains many
[layers](https://www.cagis.org/Opendata/Quarterly_GIS_Data/OpenData_Layer_List.txt)
that are updated quarterly. (Historical geodatabases are not available
here.)

## Usage

``` r
install_cagis_data(
  cagis_data_url =
    "https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataQ1_2025.gdb.zip"
)
```

## Arguments

- cagis_data_url:

  the url to the CAGIS Open Data .gdb.zip file; this changes quarterly,
  so [check](https://www.cagis.org/Opendata/Quarterly_GIS_Data) for
  something more recent if the file cannot be found

## See also

This function is called by
[`cincy_neighborhood_geo()`](http://geomarker.io/codec/reference/cincy_neighborhood_geo.md),
[`cincy_city_geo()`](http://geomarker.io/codec/reference/cincy_neighorhood_geo.md)
and others that import individual layers.

## Examples

``` r
if (FALSE) { # \dontrun{
install_cagis_data()
sf::st_layers(install_cagis_data())$name
} # }
```
