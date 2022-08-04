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

#' automatically add "type" attributes to columns in a data frame
#'
#' Given a data.frame (or tibble), this function returns the data.frame after adding on Frictionless
#' "type" attributes based on the class of each column in R:
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

  out <- purrr::map2_dfc(out, col_frictionless_classes, ~ add_attrs(..1, type = ..2))

  attributes(out) <- attributes(.x)
  return(out)
}

#' return a list representing valid CODEC-specific metadata names
codec_cdr <- function() {
  list(
    "name",
    "path",
    "title",
    "description",
    "url",
    "license",
    "schema" = list(
      "missingValues",
      "primaryKey",
      "foreignKeys",
      "fields" = list(
        "name", "title", "description",
        "type", "example", "format", "constraints"
      )
    )
  )
}

#' get CODEC descriptors and schema
#'
#' These functions are designed to provide a simple way to extract
#' tibbles of descriptors and schema for data frames and columns with attributes:
#'
#' - `get_descriptors()` gets all descriptors from a data frame
#' - `get_col_descriptors()` gets all field-specific descriptors from a single column inside of a data frame
#' - `get_schema()` gets all field-specific descriptors from all columns inside
#' of a data frame as well as additional schema descriptors (e.g., `missingValues`)
#'
#' To instead get the complete data resource metadata (descriptors & schema)
#' in a list, use `make_tdr_from_attr()`

#' @param .x data frame or tibble
#' @param codec logical; return only CODEC descriptors or schema?
#' @return a tibble (or list of tibbles for `get_schema()`)
#' with `name` and `value` columns for each descriptor
#' @export
get_descriptors <- function(.x, codec = TRUE) {
  out <-
    attributes(.x) |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_cdr())
  return(out)
}

#' @rdname get_descriptors
#' @export
get_col_descriptors <- function(.x, codec = TRUE) {
  out <-
    attributes(.x) |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_cdr()$schema$fields)
  return(out)
}

#' @rdname get_descriptors
#' @export
get_schema <- function(.x, codec = TRUE) {
  out <- purrr::map(.x, get_col_descriptors, codec = codec)
  return(out)
}
