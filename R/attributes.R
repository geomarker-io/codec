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
  attrs <- rlang::dots_list(...)
  attributes(.x) <- c(attributes(.x), attrs)
  .x
}

#' set attributes for a column in a data.frame
#'
#' @rdname set_attrs
set_col_attrs <- function(.x, var, ...) {
  .x[[rlang::enexpr(var)]] <-
    set_attrs(dplyr::pull(.x, {{ var }}), ...)
  .x
}

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

#' automatically add "type" attributes for all columns in a data.frame based on their class
add_type_attrs <- function(.x) {
  col_classes <- purrr::map_chr(.x, ~ paste(class(.), collapse = ","))
  col_frictionless_classes <- class_type_cw[col_classes]

  out <-
    purrr::map2_dfc(
      .x, col_frictionless_classes,
      ~ set_attrs(..1, type = ..2)
    )

  out_levels <- lapply(out, levels)
  # also lives in attr(out, "levels")

  out <-
    purrr::map2_dfc(
      out, out_levels,
      ~ set_attrs(..1, constraints = list(enum = ..2))
    )

  ## TODO what to do with columns that are not supported here? (sfc, others?)

  attributes(out) <- attributes(.x)
  return(out)
}
