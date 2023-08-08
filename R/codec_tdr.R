#' codec_tdr
#'
#' Defines the list of names (and descriptions) of valid CoDEC *property*, *schema*, and *fields* descriptors
#' @return a list of named character vectors, one for each of "property", "schema", and "fields"
#' @examples
#' codec_tdr()$property
#' codec_tdr()$schema
#' codec_tdr()$fields
#' @export
codec_tdr <- function() {
  list(
    property = c(
      "profile" = "profile of this descriptor (always set to `tabular-data-resource` here)",
      "name" = "an identifier string composed of lower case alphanumeric characters, `_`, `-`, and `.`",
      "path" = "location of data associated with resource as a [POSIX path](https://en.wikipedia.org/wiki/Path_%28computing%29#POSIX_pathname_definition) relative to the `tabular-data-resource.yaml` file or a fully qualified URL",
      "version" = "semantic [version](https://specs.frictionlessdata.io/patterns/#data-package-version) of the data resource",
      "title" = "human-friendly title of the resource",
      "homepage" = "homepage on the web related to the data; ideally a code repository used to create the data",
      "description" = "additional notes about the resource",
      "schema" = "a list object containing items in schema"
    ),
    schema = c(
      "fields" = "a list object as long as the number of fields each containing the items in fields",
      "primaryKey" = "a field or set of fields that uniquely identifies each row",
      "foreignKey" = "a field or set of fields that connect to a separate table"
    ),
    fields = c(
      "name" = "machine-friendly name of field/column; must be identical to name of column in data CSV file",
      "title" = "human-friendly name of field/column",
      "description" = "any additional notes about the field/column",
      "type" = "[Frictionless type](https://specs.frictionlessdata.io/table-schema/#types-and-formats) of the field/column (e.g., string, number, boolean)",
      "constraints" = "[Frictionless constraints](https://specs.frictionlessdata.io/table-schema/#constraints), including `enum`, an array of possible values or factor levels"
    )
  )
}

#' read CoDEC tabular data resource
#'
#' This function is shorthand for reading a CoDEC tabular
#' data resource using `read_tdr_csv()` from the installed
#' R package.
#' @param name name of installed codec tabular data resource
#' @param geography a {cincy} geography object; codec data
#' will be returned at this geography using `cincy::interpolate`
#' with block-level population weights
#' @param geometry return the merged `geography` object
#' alongside the output as a simple features object?
#' @return a tibble (codec tabular data resource), or simple features object when `geometry = TRUE`
#' @export
#' @examples
#' codec_data("hamilton_traffic")
#' codec_data("hamilton_traffic", cincy::neigh_cchmc_2020)
#' codec_data("hamilton_landcover", geography = cincy::zcta_tigris_2010, geometry = TRUE)

codec_data <- function(name, geography = cincy::tract_tigris_2010, geometry = FALSE) {

  installed_codec_data <-
    fs::path_package("codec") |>
    fs::path("codec_data") |>
    fs::dir_ls(glob = "*tabular-data-resource.yaml", recurse = TRUE) |>
    purrr::map(read_tdr) |>
    purrr::map_chr(c("tdr", "name"))

  if (!name %in% installed_codec_data) {
    stop(name, " not found in installed codec_data (found: ", paste(installed_codec_data, collapse = ", "), ")", call. = FALSE)
  }

  d <- read_tdr_csv(fs::path(fs::path_package("codec"), "codec_data", name))

  # check to see if we need cincy package without loading it yet
  if (!deparse(substitute(geography)) == "cincy::tract_tigris_2010") {
    if (!requireNamespace("cincy", quietly = TRUE)) {
      stop("geographic interpolation requires the {cincy} package. please install from https://geomarker.io/cincy", call. = FALSE)
    }
  }

  if(identical(geography, cincy::tract_tigris_2010) & geometry) {
    d <- dplyr::left_join(d, cincy::tract_tigris_2010, by = "census_tract_id_2010")
  }

  if (!identical(geography, cincy::tract_tigris_2010)) {
    d_sf <- dplyr::left_join(d, cincy::tract_tigris_2010, by = "census_tract_id_2010")
    d_sf <- sf::st_as_sf(d_sf)
    d_int <- cincy::interpolate(from = d_sf, to = geography, weights = "pop")
    if (!geometry) d_int <- sf::st_drop_geometry(tibble::as_tibble(d_int))
    d <- d_int
  }

  return(d)
}
