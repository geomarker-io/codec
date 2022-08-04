#' get CODEC descriptors and schema
#'
#' These functions are designed to provide a simple way to extract
#' tibbles of descriptors and schema for data frames and columns with attributes:
#'
#' - `get_descriptors()` gets all descriptors from a data frame
#' - `get_col_descriptors()` gets all field-specific descriptors from a single column inside of a data frame
#' - `get_schema()` gets all field-specific descriptors from all columns inside
#' of a data frame
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
    dplyr::mutate(value = purrr::map_chr(.data$value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_cdr())
  return(out)
}

#' @rdname get_descriptors
#' @export
get_col_descriptors <- function(.x, codec = TRUE) {
  out <-
    attributes(.x) |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(.data$value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_cdr()$schema$fields)
  return(out)
}

#' @rdname get_descriptors
#' @export
get_schema <- function(.x, codec = TRUE) {
  out <- purrr::map(.x, get_col_descriptors, codec = codec)
  return(out)
}

#' return a list representing valid CODEC-specific metadata names
#'
#' @export
#' @examples
#' codec_cdr()
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
