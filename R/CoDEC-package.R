#' About the CoDEC package
#'
#' @description
#' The CoDEC tabular-data-resource (TDR) specifications provide a set of patterns designed to make sharing tabular community-level data easier.
#' Examples of this type of data include the pediatric hospitalization rate per month per census tract, the total number of gunshots per season per neighborhood, and the housing code enforcement density per year per ZIP code.
#' 
#' Data is specified as average values or total counts for census tract geographies during a specific year (or year and month).
#' The required **census tract identifer** and **year** (or *year* and *month*) columns in a CoDEC TDR contain the spatiotemporal information that can be used to link other data.
#' 
#' A CoDEC TDR must include a [census tract](https://www2.census.gov/geo/pdfs/education/CensusTracts.pdf) column named `census_tract_id_{year}`, where `{year}` is replaced with the decennial vintage of the census tract geographies used to create the dataset (i.e., `census_tract_id_2000`, `census_tract_id_2010`, or `census_tract_id_2020`).
#' 
#' The census tract identifier column MUST contain 11-digit [GEOID](https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html) identifiers for *all* census tracts in Hamilton County (GEOID: 39061). A list of required census tract identifiers for 2000, 2010, and 2020 are available in the [cincy](https://github.com/geomarker-io/cincy) R package (e.g., `cincy::tract_tigris_2010`).
#' 
#' A CoDEC `tdr` that was *not* created at a census tract level should link to a URL (using the `homepage` [*property*](specs.html#metadata)) that contains code and a descriptive README file about how the data was harmonized (e.g., areal interpolation) with census tract geographies.
#' 
#' Year (and month) temporal variables in a CoDEC TDR must be in a "tidy format" so that each row represents one observation in time. This allows for cumulatively updating data resources without changing field-specific metadata.
#' 
#' A CoDEC TDR must include a column called `year` that contains only integers representing the year during which the data was collected (e.g., 2018, 2023).
#' 
#' It also may contain a `month` column, in which case the unique combination of the `year` and `month` columns represent the calendar month during which the data was collected (e.g., "2023" and "11" together represent November of 2023).
#' @keywords internal
"_PACKAGE"

.onLoad <- function(...) {
  S7::methods_register()
}

#' @import cincy
NULL

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

## usethis namespace: start
## usethis namespace: end
NULL
