#' About the CoDEC package
#'
#' @description
#' The CoDEC data package specifications provide a set of patterns designed to make sharing tabular community-level data easier.
#' Examples of this type of data include the pediatric hospitalization rate per month per census tract, the total number of gunshots per season per neighborhood, and the housing code enforcement density per year per ZIP code.
#' 
#' Data is specified as average values or total counts for census tract geographies during a specific year (or year and month).
#' The required **census tract identifer** and **year** (or *year* and *month*) columns in a CoDEC TDR contain the spatiotemporal information that can be used to link other data.
#' 
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
