#' set attributes for an object
#'
#' attributes are added onto existing attributes
#' @param .x an object to add attributes to
#' @param var name of variable in .x to add attributes to
#' @param ... additional arguments with name and value corresponding to metadata name and value
#' @return .x with the updated attributes
#' @examples
#' set_attrs(mtcars, name = "Motor Trend Cars", year = "1974")
#' set_col_attrs(mtcars, mpg, name = "MPG", description = "Miles Per Gallon")
set_attrs <- function(.x, ...) {
  ## attrs <- rlang::dots_list(...)
  attrs <- rlang::list2(...)
  attributes(.x) <- c(attrs, attributes(.x))
  .x
}

#' set attributes for a column in a data.frame
#'
#' @rdname set_attrs
set_col_attrs <- function(.x, var, ...) {
  dplyr::mutate(.x, "{{var}}" := set_attrs(dplyr::pull(.x, {{ var }}), ...))
  # dplyr::mutate(.x, {{ var }}, ~ set_attrs(., ...))
  # TODO add ability to use tidyselect::across() to set attributes on more than one column at once
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
  # TODO support for spatial columns?
)

#' add type attributes
#' Given a data.frame (or tibble), this function returns data.frame after adding on Frictionless
#' "type" attributes based on the class of each column; levels of factor columns are also captured
#' in the "enum" item of the "constraints" attribute list
#' @param .data a data.frame or tibble
#' @return an object of the same type as .data, with updated frictionless attributes for factor columns
#' input data frame attributes are preserved
add_type_attrs <- function(.data) {

  col_classes <- purrr::map_chr(.data, ~ paste(class(.), collapse = ","))
  col_frictionless_classes <- class_type_cw[col_classes]

  out <- purrr::map2_dfc(.data, col_frictionless_classes, ~ set_attrs(..1, type = ..2))

  out <- out |>
    dplyr::mutate(dplyr::across(
      tidyselect:::where(is.factor),
      ~ set_attrs(., constraints = list(enum = attr(., "levels")))
    ))

  ## TODO what to do with columns that are not supported here? (sfc, others?)

  attributes(out) <- attributes(.data)
  return(out)
}
