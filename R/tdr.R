codec_names <-
  list(
    descriptor = c(
      "name",
      "path",
      "title",
      "description",
      "url",
      "license",
      "schema"
    ),
    schema = c(
      "fields",
      "missingValues",
      "primaryKey"
    ),
    fields = c(
      "name",
      "title",
      "description",
      "type",
      "constraints"
    )
  )

#' make a tabular-data-resource list from the attributes of a data.frame
#'
#' @param .x a data.frame or tibble
#' @param codec logical; use only CODEC descriptors?
#' @return a list of tabular-data-resource metadata
make_tdr_from_attr <- function(.x, codec = TRUE) {
  desc <- attributes(.x)
  flds <- purrr::map(.x, attributes)

  if (codec) {
    desc <- purrr::compact(desc[codec_names$descriptor])
    flds <- purrr::modify(flds, ~ purrr::compact(.[codec_names$fields]))
  }

  tdr <- desc
  tdr$schema <- list(fields = flds)

  return(tdr)
}

#' add attributes to a data.frame based on a tabular-data-resource list
#'
#' @param .x a data.frame or tibble
#' @param tdr a tabular-data-resource list (usually created with `read_tdr()` or `make_tdr_from_attr()`)
#' @param codec logical; use only CODEC descriptors?
#' @return .x with added tabular-data-resource attributes
add_attr_from_tdr <- function(.x, tdr, codec = TRUE) {

  desc <- tdr
  flds <- purrr::pluck(tdr, "schema", "fields")
  purrr::pluck(desc, "schema") <- NULL

  if (codec) {
    desc <- purrr::compact(desc[codec_names$descriptor])
    flds <- purrr::modify(flds, ~ purrr::compact(.[codec_names$fields]))
  }

  out <- add_attrs(.x, !!!desc)

  for (field in names(flds)) {
    out <- add_col_attrs(out, field, !!! tdr$schema$fields[field])
  }

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
write_tdr <- function(.x, file = "tabular-data-resource.yaml", codec = TRUE) {
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

#' write a tabular-data-resource yaml file and data csv file based on a data.frame or tibble
#'
#' The `path` argument specifies where the folder containing
#' the codec-tdr will be created.  Within this path, the folder
#' for the codec-tdr will be named based on the name attribute
#' of the data.frame or tibble. The CSV data file will be named
#' based on the name attribute of the data.frame or tibble
#' and a "tabular-data-resource.yaml" file will also be created.
#' @param .x data.frame or tibble
#' @param path path to directory where tdr will be created; see details
#' @param codec logical; use only CODEC descriptors?
#' @export
write_tdr_csv <- function(.x, dir = getwd(), codec = TRUE) {

  tdr_name <- attr(.x, "name")
  # TODO make paths in yaml file relative to `dir`

  tdr_dir <- fs::path(dir, tdr_name)
  tdr_csv <- fs::path(tdr_dir, paste0(tdr_name, ".csv"))
  tdr_yml <- fs::path(tdr_dir, "tabular-data-resource.yaml")

  fs::dir_create(tdr_dir)
  cli::cli_alert_success("created {tdr_dir}/")

  readr::write_csv(.x, tdr_csv)
  cli::cli_alert_success("wrote data to {tdr_csv}")

  .x |>
    add_attrs(path = tdr_csv) |>
    write_tdr(file = tdr_yml, codec = codec)
  cli::cli_alert_success("wrote metadata to {tdr_yml}")
}
