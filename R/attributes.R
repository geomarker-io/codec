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
  attributes(.x) <- c(attributes(.x), attrs)
  .x
}

#' @rdname add_attrs
#' @export
add_col_attrs <- function(.x, var, ...) {
  dplyr::mutate(.x, "{{var}}" := add_attrs(dplyr::pull(.x, {{ var }}), ...))
  # dplyr::mutate(.x, {{ var }}, ~ add_attrs(., ...))
}

#' automatically add "name" and "type" attributes to columns in a data frame
#'
#' Given a data.frame (or tibble), this function returns the
#' data.frame after adding on Frictionless "name" and
#' "type" attributes based on the name and class of each column in R:
#'
#' |   **R class**  | **TDR type** |
#' |:--------------:|:------------:|
#' |    character   |    string    |
#' |      Date      |     date     |
#' |     numeric    |    number    |
#' |     factor*     |    string    |
#' |  hms,difftime  |     time     |
#' |     integer    |    integer   |
#' |     logical    |    boolean   |
#' | POSIXct,POSIXt |   datetime   |
#' |    difftime    |    number    |
#'
#' *Levels of factor columns are also captured in the "enum" item of the "constraints" attribute list.
#'
#' @param .x a data.frame or tibble
#' @return an object of the same type as .x, with updated frictionless attributes for factor columns
#' input data frame attributes are preserved
#' @export
add_type_attrs <- function(.x) {
  col_classes <- purrr::map_chr(.x, ~ paste(class(.), collapse = ","))

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

  col_frictionless_classes <- class_type_cw[col_classes]

  # add enum constraints first so it shows up after type
  out <- .x |>
    dplyr::mutate(dplyr::across(
      where(is.factor),
      ~ add_attrs(., constraints = list(enum = attr(., "levels")))
    ))

  out <- purrr::imap_dfc(out, ~ add_attrs(..1, name = ..2))

  out <- purrr::map2_dfc(out, col_frictionless_classes, ~ add_attrs(..1, type = ..2))

  attributes(out) <- attributes(.x)
  return(out)
}

