#' make a tabular-data-resource list from the attributes of a data.frame
#'
#' @param .x a data.frame or tibble
#' @param codec logical; use only CODEC properties?
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
#' @param tdr a tabular-data-resource list (usually created with `read_tdr()` or `make_tdr_from_attr()`)
#' @param codec logical; use only CODEC properties?
#' @return .x with added tabular-data-resource attributes
#' @export
add_attr_from_tdr <- function(.x, tdr, codec = TRUE) {

  desc <- tdr
  flds <- purrr::pluck(tdr, "schema", "fields")
  purrr::pluck(desc, "schema") <- NULL

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

#' extract data resource metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
#' @param codec logical; include only CODEC properties or schema? (see `?codec_tdr` for details)
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
#' are read into R using `readr::read_csv()`. Metadata
#' (properties and schema) are stored as attributes
#' of the returned tibble and are also used to set
#' the column classes of the returned data.frame or tibble.
#'
#' @param dir path to folder that contains a
#' tabular-data-resource.yaml file
#' @param codec logical; use only CODEC properties?
#' @param ... additional options passed onto `readr::read_csv()`
#' @return tibble with added tabular-data-resource attributes
#' @export
read_tdr_csv <- function(dir = getwd(), codec = TRUE, ...) {

  tdr <- read_tdr(fs::path(dir, "tabular-data-resource.yaml"))

  desc <- tdr
  flds <- purrr::pluck(tdr, "schema", "fields")
  purrr::pluck(desc, "schema") <- NULL

  type_class_cw <- c(
    "string" = "c",
    "date" = "D",
    "number" = "n",
    "time" = "t",
    "integer" = "i",
    "boolean" = "l",
    "datetime" = "T"
  )

  col_names <- names(flds)
  col_classes <- type_class_cw[purrr::map_chr(flds, "type")]

  lvls <-
    purrr::map(flds, "constraints", "enum") |>
    purrr::compact()

  if (length(lvls) > 0) col_classes[[names(lvls)]] <- "f"

  data_path <- fs::path(dir, desc$path)

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
      name_repair = "check_unique",
      ...,
    )

  cli::cli_alert_success("read in data from {.path {fs::path(data_path)}}")

  if (length(lvls) > 0) {
    for (lvl in names(lvls)) {
      out <- dplyr::mutate(out,
        {{ lvl }} := forcats::fct_expand(
          dplyr::pull(out, {{ lvl }}),
          as.character(lvls[[lvl]])
        ))
    }
  }

  out <- add_attr_from_tdr(out, tdr, codec = codec)
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
#' @param dir path to directory where tdr will be created; see details
#' @param codec logical; use only CODEC properties?
#' @export
write_tdr_csv <- function(.x, dir = getwd(), codec = TRUE) {

  tdr_name <- attr(.x, "name")

  tdr_dir <- fs::path(dir, tdr_name)
  tdr_csv <- fs::path(tdr_dir, tdr_name, ext = "csv")
  tdr_yml <- fs::path(tdr_dir, "tabular-data-resource.yaml")

  fs::dir_create(tdr_dir)
  cli::cli_alert_success("created {tdr_dir}/")

  readr::write_csv(.x, tdr_csv)
  cli::cli_alert_success("wrote data to {tdr_csv}")

  .x |>
    add_attrs(path = fs::path_rel(tdr_csv, start = tdr_dir)) |>
    write_tdr(file = tdr_yml, codec = codec)
  cli::cli_alert_success("wrote metadata to {tdr_yml}")
}

## make_metadata_md <- function(.x, file_name = "metadata.md") {
##   options(knitr.kable.NA = "")
##   cat("#### Metadata\n\n", file = file_name, append = FALSE)
##   CODECtools::glimpse_attr(d) |>
##     knitr::kable() |>
##     cat(file = file_name, sep = "\n", append = TRUE)
##   cat("\n#### Schema\n\n", file = file_name, append = TRUE)
##   d |>
##     dplyr::select(-ends_with("moe")) |>
##     CODECtools::glimpse_schema() |>
##     knitr::kable() |>
##     cat(file = file_name, sep = "\n", append = TRUE)
## }

#' glimpse attributes of a data.frame
#' @param .x data frame or tibble
#' @param codec logical; include only CODEC properties? (see `?codec_tdr` for details)
#' @return a tibble with `name` and `value` columns of attributes
#' @details values are collapsed using `,`
#' @export
glimpse_attr <- function(.x, codec = TRUE) {
  tdr <- make_tdr_from_attr(.x, codec = codec)
  purrr::pluck(tdr, "schema") <- NULL

  tdr |>
    tibble::enframe() |>
    dplyr::rowwise(name) |>
    dplyr::mutate(value = paste(value, collapse = ",")) |>
    dplyr::ungroup()
    ## dplyr::filter(!name == "schema")
}

#' glimpse CODEC schema
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
