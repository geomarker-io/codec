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
  out <-
    attributes(.x) |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(.data$value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_tdr())
  return(out)
}

#' @rdname get_descriptors
#' @export
get_col_descriptors <- function(.x, codec = TRUE) {
  out <-
    attributes(.x) |>
    tibble::enframe() |>
    dplyr::mutate(value = purrr::map_chr(.data$value, ~ paste(., collapse = ", ")))

  if (codec) out <- dplyr::filter(out, .data$name %in% codec_tdr()$schema$fields)
  return(out)
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

#' make a tabular-data-resource list from the attributes of a data.frame
#'
#' @param .x a data.frame or tibble
#' @param codec logical; include only CODEC descriptors or schema? (see `?codec_tdr` for details)
#' @return a list of tabular-data-resource metadata
make_tdr_from_attr <- function(.x, codec = TRUE) {

  tdr <-
    get_descriptors(.x, codec = codec) |>
    tibble::deframe() |>
    as.list()

  schm <-
    get_schema(.x, bind = FALSE, codec = codec) |>
    purrr::modify(tibble::deframe) |>
    purrr::modify(as.list)
  tdr$schema <- list(fields = schm)

  return(tdr)
}

#' add CODEC attributes to a data.frame based on a tabular-data-resource list
#'
#' @param .x a data.frame or tibble
#' @param tdr a tabular-data-resource list (usually created with `read_tdr()`)
#' @return .x with added tabular-data-resource attributes
add_attr_from_tdr <- function(.x, tdr) {

  descriptors <-
    tdr |>
    ## TODO better way to filter list based on name??
    tibble::enframe() |>
    dplyr::filter(.data$name %in% codec_tdr()) |>
    tibble::deframe()

  out <- purrr::map2_dfc(.x, tdr$schema$fields, ~ add_attrs(..1, !!!..2))
  out <- add_attrs(out, !!!descriptors)

  return(out)
}

#' extract data resource metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
#' @param codec logical; include only CODEC descriptors or schema? (see `?codec_tdr` for details)
#' @return .x (invisibly)
#' @examples
#' \dontrun{
#' mtcars |>
#'   add_attrs(name = "Motor Trend Cars", year = "1974") |>
#'   add_col_attrs(mpg, title = "MPG", description = "Miles Per Gallon") |>
#'   add_type_attrs() |>
#'   save_tdr(my_mtcars, "my_mtcars_tabular-data-resource.yaml")
#' }
#' @export
save_tdr <- function(.x, file = "tabular-data-resource.yaml", codec = TRUE) {
  .x |>
    add_attrs(profile = "tabular-data-resource") |>
    make_tdr_from_attr(codec = codec) |>
    yaml::as.yaml() |>
    cat(file = file)

  return(invisible(.x))
}

#' read metadata in from a tabular-data-resource.yaml file
#'
#' @param file filename (or connection) of yaml file to read metadata from
#' @return a list of frictionless metadata
#' @export
read_tdr <- function(file = "tabular-data-resource.yaml") {
  # TODO if file is a folder, look for "tabular-data-resource.yaml" there
  metadata <- yaml::yaml.load_file(file)
  return(metadata)
}

#' read a CSV tabular data resource into R
#'
#' The CSV file defined in a tabular-data-resource yaml file
#' will be read into R using `readr::read_csv`. Columns to
#' read and their types are set using metadata. Metadata
#' (descriptors and schema) are stored as attributes
#' of the returned tibble.
#'
#' *Note:*
#' - the `path` descriptor is always relative to the tabular-data-resource file
#' - descriptors will always be restricted to those in `codec_tdr()`
#' @param file path to tabular-data-resource yaml file
#' @return tibble with added tabular-data-resource attributes
#' @export
read_tdr_csv <- function(file = "tabular-data-resource.yaml") {

  metadata <- read_tdr(file)

  descriptors <-
    metadata |>
    tibble::enframe() |>
    dplyr::filter(.data$name %in% codec_tdr()) |>
    tibble::deframe()

  type_class_cw <- c(
    "string" = "c",
    "date" = "D",
    "number" = "n",
    "time" = "t",
    "integer" = "i",
    "boolean" = "l",
    "datetime" = "T"
  )

  col_names <- names(metadata$schema$fields)
  col_types <- purrr::map_chr(metadata$schema$fields, "type")
  col_classes <- purrr::map_chr(col_types, ~ type_class_cw[.])
  lvls <-
    purrr::map(metadata$schema$fields, "constraints", "enum") |>
    purrr::compact()

  col_classes[[names(lvls)]] <- "f"

  data_path <- fs::path(fs::path_dir(file), metadata$path)

  out <-
    readr::read_csv(
      file = data_path,
      col_types = paste(col_classes, collapse = ""),
      col_select = all_of({{ col_names }}),
      locale = readr::locale(
        encoding = "UTF-8",
        decimal_mark = ".",
        grouping_mark = ""
      ),
      ## na = c("", "NA"),
      ## quote = "\"",
      name_repair = "check_unique",
      lazy = FALSE
    )

  for (lvl in names(lvls)) {
    out <- dplyr::mutate(out, {{ lvl }} := forcats::fct_expand(dplyr::pull(out, {{ lvl }}), lvls[[lvl]]))
  }

  out <- add_attr_from_tdr(out, metadata)
  return(out)
}

## write_tdr_csv <- function() {

## }
