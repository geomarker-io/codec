#' codec_tdr
#' 
#' Defines the list of names (and descriptions) of valid CODEC *property*, *schema*, and *fields* descriptors
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
      "schema" = "a list object containing items in schema",
      "_s3VersionId" = "the VersionId of the file stored on AWS S3; not user-generated"
    ),
    schema = c(
      "fields" = "a list object as long as the number of fields each containing the items in fields",
      "primaryKey" = "a field or set of fields that uniquely identifies each row",
      "foreignKey" = "a field or set of fields that connect to a separate table",
      "missingValues" = "a list object, one for each column, containing the following"
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

#' Check for Tabular Data Resource Name
#' 
#' Name must be an identifier string composed of lower 
#' case alphanumeric characters, _, -, and .
#'
#' @param name the name field from tabular-data-resource.yaml
#'
#' @return an error message if conditions are not met
#' @export
#'
#' @examples
#' check_tdr_name("name")
check_tdr_name <- function(name) {
  # metadata has a name field
  if(is.null(name)) stop("Metadata must have a field called 'name'.")
  # name is a character string
  if(!is.character(name)) stop("'name' must be character string.")
  # name does not have uppercase letters
  if(stringr::str_detect(name, "[[:upper:]]")) stop("'name' must be all lowercase.")
  # name does not have spaces
  if(stringr::str_detect(name, " ")) stop("'name' must not contain spaces.")
  # nonalphanumeric characters are either -, _, or .
  if(!all(stringr::str_detect(unlist(stringr::str_extract_all(name, "[^[:alnum:]]")), "[_.-]"))) {
    stop("Accepted non-alphanumeric characters for 'name' are '-', '_', and '.'")
  }
}

# TODO make sure name descriptor is identical to CSV file name in path descriptor
# TODO check that field names are unique

