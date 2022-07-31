#' add attributes
#'
#' - Use `add_attr` to add attributes to an R object
#' - Use `add_col_attr` to add attributes to a column inside of a data frame
#' @param .x an object to add attributes to
#' @param var name of variable in .x to add attributes to
#' @param ... additional arguments with name and value corresponding to metadata name and value
#' @return .x with the updated attributes
#' @examples
#' add_attrs(mtcars, name = "Motor Trend Cars", year = "1974")
#' add_col_attrs(mtcars, mpg, name = "MPG", description = "Miles Per Gallon")
#' @export
add_attrs <- function(.x, ...) {
  ## attrs <- rlang::dots_list(...)
  attrs <- rlang::list2(...)
  attributes(.x) <- c(attrs, attributes(.x))
  .x
}

#' @rdname add_attrs
#' @export
add_col_attrs <- function(.x, var, ...) {
  dplyr::mutate(.x, "{{var}}" := add_attrs(dplyr::pull(.x, {{ var }}), ...))
  # dplyr::mutate(.x, {{ var }}, ~ add_attrs(., ...))
}

# TODO link this somehow or explain the mappings somewhere?
class_type_cw <- c(
  "character" = "string",
  "Date" = "date",
  "numeric" = "number",
  "factor" = "string",
  "hms,difftime" = "time",
  "integer" = "integer",
  "logical" = "boolean",
  "POSIXct,POSIXt" = "datetime",
  "difftime" = "number"
)

#' automatically add "type" attributes to columns in a data frame
#'
#' Given a data.frame (or tibble), this function returns data.frame after adding on Frictionless
#' "type" attributes based on the class of each column; levels of factor columns are also captured
#' in the "enum" item of the "constraints" attribute list.
#' @param .x a data.frame or tibble
#' @return an object of the same type as .x, with updated frictionless attributes for factor columns
#' input data frame attributes are preserved
#' @export
add_type_attrs <- function(.x) {
  col_classes <- purrr::map_chr(.x, ~ paste(class(.), collapse = ","))
  col_frictionless_classes <- class_type_cw[col_classes]

  out <- purrr::map2_dfc(.x, col_frictionless_classes, ~ add_attrs(..1, type = ..2))

  out <- out |>
    dplyr::mutate(dplyr::across(
      where(is.factor),
      ~ add_attrs(., constraints = list(enum = attr(., "levels")))
    ))

  attributes(out) <- attributes(.x)
  return(out)
}

#' get descriptors from attributes of data frame
#'
#' `get_descriptors()` looks for attributes with a value of
#' a single character string and returns them in a tibble.
#' Other attributes will not be returned.
#' To get complete data resource metadata in a list,
#' use `make_data_resource_from_attr()`

#' @param .x data frame or tibble
#' @return a tibble with `name` and `value` columns for each attribute
#' @export
get_descriptors <- function(.x) {
  attributes(.x) |>
    purrr::keep(~ length(.) == 1) |>
    tibble::enframe() |>
    dplyr::mutate(value = as.character(.data$value))
}
