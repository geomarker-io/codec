#' make metadata list based on attributes of data.frame (including schema based on attributes of columns)
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

#' save (table- and column-specific) metadata to a file
write_metadata <- function(.x, file = "tabular-data-resource.yaml") {
  # TODO automatically make schema metadata from col classes if it doesn't already exist
  .x |>
    set_attrs(profile = "tabular-data-resource") |>
    make_metadata_from_attr() |>
    yaml::as.yaml() |>
    cat(file = file)

  return(invisible(file))
}

# TODO wrapper save function that extracts metadata from attributes and then saves the data as a CSV file and the metadata as a datapackage.json

# could we make a cool print method for a data.frame with codec attributes?
# or with codec variable names?

# TODO add function to read metadata (and schema) and create a 'pretty' data dictionary / readme / about file
