#' return a list representing a valid CODEC-specific tabular-data-resource structure
#'
#' @export
#' @examples
#' codec_tdr()
codec_tdr <- function() {
  list(
    "name",
    "path",
    "title",
    "description",
    "url",
    "license",
    "schema" = list(
      "missingValues", # "", "NA" ???
      "primaryKey", # field(s) that uniquely identify each row ??
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
#' - `get_descriptors()` gets all descriptors from a data frame
#' - `get_col_descriptors()` gets all field-specific descriptors from a single column inside of a data frame
#' - `get_schema()` gets all field-specific descriptors from all columns inside
#' of a data frame
#' 
#' To instead get the complete data resource metadata (descriptors & schema)
#' in a list, use `make_tdr_from_attr()`
#' @param .x data frame or tibble
#' @param codec logical; return only CODEC descriptors or schema?
#' @param bind logical; bind schema together into one wide data frame?
#' @return a tibble (or list of tibbles for `get_schema()` if `bind = FALSE`)
#' with `name` and `value` columns for each descriptor
#' @export
get_descriptors <- function(.x, codec = TRUE) {
  .x_attrs <- attributes(.x)
  if (codec) .x_attrs <- .x_attrs[unique(unlist(codec_tdr()))] |> purrr::compact()
  .x_attrs |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(.data$value, ~ paste(., collapse = ", ")))
}

#' @rdname get_descriptors
#' @export
get_col_descriptors <- function(.x, codec = TRUE) {

  .x_attrs <- attributes(.x)

  if (codec) .x_attrs <- .x_attrs[unlist(codec_tdr()$schema$fields)] |> purrr::compact()

  if ("constraints" %in% names(.x_attrs)) {
    .x_attrs <- purrr::modify_at(.x_attrs, "constraints", purrr::flatten_chr)
  }

  tibble::enframe(.x_attrs)
}

#' @rdname get_descriptors
#' @export
get_schema <- function(.x, bind = TRUE, codec = TRUE) {
  out <- purrr::map(.x, get_col_descriptors, codec = codec)
  if (bind) {
    out <- out |>
      purrr::modify(tidyr::pivot_wider) |>
      dplyr::bind_rows(.id = "col")
  }
  return(out)
}
