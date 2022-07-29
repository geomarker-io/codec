
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CODECtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geomarker-io/CODECtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of CODECtools is to support CODEC data infrastructure through:

-   capturing, creating, and accessing metadata for tabular data in R
-   importing and exporting tabular data resources (CSV file) with
    metadata (YAML file)
-   defining CODEC data format specifications (as vignette in package)
-   providing tools and workflows to validate CODEC data and contribute
    it to CODECdata repository
-   creation of online data catalog

## Installation

You can install the development version of CODECtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("geomarker-io/CODECtools")
```

## Metadata Example

Here is a basic example of adding metadata to data in R, saving it to
disk, and reading it back into R with the metadata:

``` r
library(CODECtools)

my_mtcars <-
  mtcars |>
    add_attrs(name = "Motor Trend Cars", year = "1974") |>
    add_type_attrs() |>
    add_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon")

attributes(my_mtcars)
#> $name
#> [1] "Motor Trend Cars"
#> 
#> $year
#> [1] "1974"
#> 
#> $names
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
#> [11] "carb"
#> 
#> $row.names
#>  [1] "Mazda RX4"           "Mazda RX4 Wag"       "Datsun 710"         
#>  [4] "Hornet 4 Drive"      "Hornet Sportabout"   "Valiant"            
#>  [7] "Duster 360"          "Merc 240D"           "Merc 230"           
#> [10] "Merc 280"            "Merc 280C"           "Merc 450SE"         
#> [13] "Merc 450SL"          "Merc 450SLC"         "Cadillac Fleetwood" 
#> [16] "Lincoln Continental" "Chrysler Imperial"   "Fiat 128"           
#> [19] "Honda Civic"         "Toyota Corolla"      "Toyota Corona"      
#> [22] "Dodge Challenger"    "AMC Javelin"         "Camaro Z28"         
#> [25] "Pontiac Firebird"    "Fiat X1-9"           "Porsche 914-2"      
#> [28] "Lotus Europa"        "Ford Pantera L"      "Ferrari Dino"       
#> [31] "Maserati Bora"       "Volvo 142E"         
#> 
#> $class
#> [1] "data.frame"
sapply(my_mtcars, attributes)
#> $mpg
#> $mpg$name
#> [1] "MPG"
#> 
#> $mpg$description
#> [1] "Miles Per Gallon"
#> 
#> $mpg$type
#> [1] "number"
#> 
#> 
#> $cyl
#> $cyl$type
#> [1] "number"
#> 
#> 
#> $disp
#> $disp$type
#> [1] "number"
#> 
#> 
#> $hp
#> $hp$type
#> [1] "number"
#> 
#> 
#> $drat
#> $drat$type
#> [1] "number"
#> 
#> 
#> $wt
#> $wt$type
#> [1] "number"
#> 
#> 
#> $qsec
#> $qsec$type
#> [1] "number"
#> 
#> 
#> $vs
#> $vs$type
#> [1] "number"
#> 
#> 
#> $am
#> $am$type
#> [1] "number"
#> 
#> 
#> $gear
#> $gear$type
#> [1] "number"
#> 
#> 
#> $carb
#> $carb$type
#> [1] "number"
    
write_metadata(my_mtcars, "my_mtcars_tabular-data-resource.yaml")

yaml::yaml.load_file("my_mtcars_tabular-data-resource.yaml")
#> $profile
#> [1] "tabular-data-resource"
#> 
#> $name
#> [1] "Motor Trend Cars"
#> 
#> $year
#> [1] "1974"
#> 
#> $schema
#> $schema$fields
#> $schema$fields$mpg
#> $schema$fields$mpg$name
#> [1] "MPG"
#> 
#> $schema$fields$mpg$description
#> [1] "Miles Per Gallon"
#> 
#> $schema$fields$mpg$type
#> [1] "number"
#> 
#> 
#> $schema$fields$cyl
#> $schema$fields$cyl$type
#> [1] "number"
#> 
#> 
#> $schema$fields$disp
#> $schema$fields$disp$type
#> [1] "number"
#> 
#> 
#> $schema$fields$hp
#> $schema$fields$hp$type
#> [1] "number"
#> 
#> 
#> $schema$fields$drat
#> $schema$fields$drat$type
#> [1] "number"
#> 
#> 
#> $schema$fields$wt
#> $schema$fields$wt$type
#> [1] "number"
#> 
#> 
#> $schema$fields$qsec
#> $schema$fields$qsec$type
#> [1] "number"
#> 
#> 
#> $schema$fields$vs
#> $schema$fields$vs$type
#> [1] "number"
#> 
#> 
#> $schema$fields$am
#> $schema$fields$am$type
#> [1] "number"
#> 
#> 
#> $schema$fields$gear
#> $schema$fields$gear$type
#> [1] "number"
#> 
#> 
#> $schema$fields$carb
#> $schema$fields$carb$type
#> [1] "number"
```
