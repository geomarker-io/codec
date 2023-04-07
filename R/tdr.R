#' make a tabular-data-resource list from the attributes of a data.frame
#'
#' @param .x a data.frame or tibble
#' @param codec logical; use only CoDEC properties?
#' @return a list of tabular-data-resource metadata
make_tdr_from_attr <- function(.x, codec = TRUE) {
  desc <- attributes(.x)
  flds <- purrr::map(.x, attributes)

  if (codec) {
    desc <- purrr::compact(desc[names(codec_tdr()$property)])
    flds <-
      purrr::map(flds, ~ .[names(codec_tdr()$fields)]) |>
      purrr::map(purrr::compact)
    flds[purrr::map_lgl(flds, ~ is.list(.) & length(.) == 0)] <- NULL
    flds <- purrr::compact(flds)
  }

  tdr <- desc
  tdr$schema <- list(fields = flds)

  return(tdr)
}

#' add attributes to a data.frame based on a tabular-data-resource list
#'
#' @param .x a data.frame or tibble
#' @param tdr a tabular-data-resource list (usually created with `yaml::read_yaml` or `make_tdr_from_attr()`)
#' @param codec logical; use only CoDEC properties?
#' @return .x with added tabular-data-resource attributes
#' @export
add_attr_from_tdr <- function(.x, tdr, codec = TRUE) {
  desc <- tdr
  flds <- purrr::pluck(tdr, "schema", "fields")
  purrr::pluck(desc, "schema") <- NULL
  desc <- purrr::compact(desc)

  if (codec) {
    desc <- purrr::compact(desc[names(codec_tdr()$property)])
    flds <- purrr::modify(flds, ~ purrr::compact(.[names(codec_tdr()$fields)]))
  }

  out <- add_attrs(.x, !!!desc)

  for (the_field in names(flds)) {
    out[[the_field]] <-
      add_attrs(out[[the_field]], !!!flds[[the_field]])
  }

  return(out)
}

#' glimpse attributes of a data.frame
#' @param .x data frame or tibble
#' @param codec logical; include only CoDEC properties? (see `?codec_tdr` for details)
#' @return a tibble with `name` and `value` columns of attributes
#' @details values are collapsed using `,`
#' @export
glimpse_attr <- function(.x, codec = TRUE) {
  tdr <- make_tdr_from_attr(.x, codec = codec)
  purrr::pluck(tdr, "schema") <- NULL
  tdr <- purrr::compact(tdr)

  tdr |>
    tibble::enframe() |>
    dplyr::rowwise(name) |>
    dplyr::mutate(value = paste(value, collapse = ",")) |>
    dplyr::ungroup()
}

#' glimpse CoDEC schema
#' @param .x data frame or tibble
#' @return a tibble with attributes for each column in `.x`
#' @details constraints are collapsed using `,`
#' @export
glimpse_schema <- function(.x) {
  tdr <- make_tdr_from_attr(.x, codec = TRUE)
  flds <- purrr::pluck(tdr, "schema", "fields")

  flds <- flds |>
    purrr::modify(tibble::as_tibble) |>
    dplyr::bind_rows()

  if ("constraints" %in% names(flds)) {
    flds <-
      flds |>
      dplyr::rowwise() |>
      dplyr::mutate(constraints = paste(constraints, collapse = ", ")) |>
      dplyr::mutate(constraints = ifelse(constraints == "", NA, constraints)) |>
      dplyr::ungroup()
  }

  return(flds)
}

#' glimpse CoDEC tdr
#' This can be used instead of separately calling `glimpse_attr()` and `glimpse_schema()`
#' @param .x data frame or tibble
#' @param codec logical; include only CoDEC properties? (see `?codec_tdr` for details)
#' @return a named list of tibbles for attributes and schema
#' @export
glimpse_tdr <- function(.x, codec = TRUE) {
  list(glimpse_attr(.x, codec = codec), glimpse_schema(.x)) |>
    rlang::set_names(c("attributes", "schema"))
}
