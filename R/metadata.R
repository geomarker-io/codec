#' make metadata list from attributes of data.frame
#'
#' This also includes schema based on the attributes of columns.
#' This function is largely for use in `write_metadata()` and
#' should normally not need to be used directly.
#' @param .x a data.frame or tibble
#' @return a list of metadata generated using the attributes of .x
make_data_resource_from_attr <- function(.x) {
  metad <- as.list(attributes(.x))
  # TODO only keep frictionless-relevant attributes?
  metad$names <- NULL
  metad$row.names <- NULL
  metad$class <- NULL
  metad$tigris <- NULL
  metad$schema <- list(fields = purrr::map(.x, attributes))
  return(metad)
}

#' extract data resource metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
#' @return .x (invisibly)
#' @examples
#' \dontrun{
#' mtcars |>
#'   add_attrs(name = "Motor Trend Cars", year = "1974") |>
#'   add_type_attrs() |>
#'   add_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon") |>
#'   write_metadata(my_mtcars, "my_mtcars_tabular-data-resource.yaml")
#' }
#' @export
# save_data_resource() ??
write_metadata <- function(.x, file = "tabular-data-resource.yaml") {
  .x |>
    add_attrs(profile = "tabular-data-resource") |>
    make_data_resource_from_attr() |>
    yaml::as.yaml() |>
    cat(file = file)

  return(invisible(.x))
}

