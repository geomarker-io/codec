#' make metadata list from attributes of data.frame
#'
#' This also includes schema based on the attributes of columns.
#' This function is largely for use in `write_metadata()` and
#' should normally not need to be used directly.
#' @param .x a data.frame or tibble
#' @return a list of metadata generated using the attributes of .x
make_metadata_from_attr <- function(.x) {
  metad <- as.list(attributes(.x))
  # TODO only keep frictionless-relevant attributes?
  metad$names <- NULL
  metad$row.names <- NULL
  metad$class <- NULL
  metad$tigris <- NULL
  metad$schema <- list(fields = purrr::map(.x, attributes))
  return(metad)
}

#' extract (table- and column-specific) metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
#' @return .x (invisibly)
#' @examples
#' \dontrun{
#' mtcars |>
#'  add_attrs(name = "Motor Trend Cars", year = "1974") |>
#'  add_type_attrs() |>
#'  add_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon") |>
#'  write_metadata(my_mtcars, "my_mtcars_tabular-data-resource.yaml")
#' }
#' @export
write_metadata <- function(.x, file = "tabular-data-resource.yaml") {
  # TODO automatically make schema metadata from col classes if it doesn't already exist
  .x |>
    add_attrs(profile = "tabular-data-resource") |>
    make_metadata_from_attr() |>
    yaml::as.yaml() |>
    cat(file = file)

  return(invisible(.x))
}

# TODO wrapper save function that extracts metadata from attributes and then saves the data as a CSV file and the metadata as a datapackage.json

# could we make a cool print method for a data.frame with codec attributes?
# or with codec variable names?

# TODO add function to read metadata (and schema) and create a 'pretty' data dictionary / readme / about file

## read_metadata
