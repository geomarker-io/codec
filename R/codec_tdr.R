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
      "profile" = "`tabular-data-resource`",
      "name" = "identifer composed of lower case alphanumeric characters, `_`, `-`, or `.`",
      "path" = "relative file path or URL of data file",
      "version" = "semantic [version](https://specs.frictionlessdata.io/patterns/#data-package-version) of the data resource",
      "title" = "human-friendly title of the resource",
      "homepage" = "homepage on the web related to the data; ideally a code repository used to create the data",
      "description" = "additional notes about the resource",
      "schema" = "a list object containing items in schema"
    ),
    schema = c(
      "fields" = "a list object as long as the number of fields each containing the items in fields",
      "missingValues" = "the string values that should be considered missing observations",
      "primaryKey" = "a field or set of fields that uniquely identifies each row",
      "foreignKeys" = "a field or set of fields that connect to a separate table"
    ),
    fields = c(
      "name" = "machine-friendly name of the field",
      "title" = "human-friendly name of the field",
      "description" = "any additional notes about the field",
      "type" = "Frictionless type of the field",
      "constraints" = "Frictionless constraints, including `enum`, an array of possible values or factor levels"
    )
  )
}

#' read CoDEC tabular data resource
#'
#' This function is shorthand for reading a CoDEC tabular
#' data resource using `read_tdr_csv()` from the installed
#' R package.
#' @param name name of installed codec tabular data resource
#' @param geography a [{cincy} geography object](https://geomarker.io/cincy/#data)
#' to spatially interpolate data to
#' @return a frictionless tabular data resource (`fr_tdr`) object
#' @export
#' @examples
#' codec_data("hamilton_property_code_enforcement")
#' codec_data("hamilton_property_code_enforcement", geography = cincy::neigh_cchmc_2010)
codec_data <- function(name, geography = cincy::tract_tigris_2010) {
  installed_codec_data <-
    fs::path_package("codec") |>
    fs::path("codec_data") |>
    fs::dir_ls(glob = "*tabular-data-resource.yaml", recurse = TRUE) |>
    purrr::map(yaml::read_yaml) |>
    purrr::map_chr("name")

  if (!name %in% installed_codec_data) {
    stop(name, " not found in installed codec_data (found: ", paste(installed_codec_data, collapse = ", "), ")", call. = FALSE)
  }

  d <-
    fs::path(
      fs::path_package("codec"),
      "codec_data",
      name,
      "tabular-data-resource.yaml"
    ) |>
    fr::read_fr_tdr()

  codec_geography <- cincy::tract_tigris_2010

  if (identical(geography, codec_geography)) {
    return(d)
  }

  out <-
    as.data.frame(d) |>
    dplyr::left_join(codec_geography, by = "census_tract_id_2010") |>
    sf::st_as_sf() |>
    cincy::interpolate(to = geography, weights = "pop") |>
    sf::st_drop_geometry() |>
    fr::as_fr_tdr(.template = d)

  return(out)
}
