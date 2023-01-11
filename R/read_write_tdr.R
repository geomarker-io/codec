#' read a CSV tabular data resource into R from disk or the web
#'
#' The CSV file defined in a tabular-data-resource yaml file
#' are read into R using `readr::read_csv()`. Metadata
#' (properties and schema) are stored as attributes
#' of the returned tibble and are also used to set
#' the column classes of the returned data.frame or tibble.
#' Files starting with `http://` or `https://` will be automatically
#' downloaded locally first.
#'
#' @param tdr_file path to tabular-data-resource.yaml file;
#' @param codec logical; use only CODEC properties?
#' @param ... additional options passed onto `readr::read_csv()`
#' @return tibble with added tabular-data-resource attributes
#' @export
read_tdr_csv <- function(tdr_file, codec = TRUE, ...) {

  # TODO if not URL and not file, assume it is a folder with tabular-data-resource.yaml file

  # if tdr_file is a URL
  if (grepl("^((http|ftp)s?|sftp)://", tdr_file)) {
    tdr <- yaml::read_yaml(url(tdr_file))
    csv_file <- gsub("tabular-data-resource.yaml",
                     tdr$path,
                     tdr_file,
                     fixed = TRUE)
  } else { # tdr_file is a file
    tdr <- yaml::read_yaml(tdr_file)
    csv_file <- fs::path(fs::path_dir(tdr_file), tdr$path)
  }

  flds <- purrr::pluck(tdr, "schema", "fields")

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

  out <-
    readr::read_csv(
      file = csv_file,
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
#' @return .x, invisibly
#' @export
write_tdr_csv <- function(.x, dir = getwd(), codec = TRUE) {

  tdr_name <- attr(.x, "name")

  tdr_dir <- fs::path(dir, tdr_name)
  fs::dir_create(tdr_dir)

  tdr_csv <- fs::path(tdr_dir, tdr_name, ext = "csv")
  readr::write_csv(.x, tdr_csv)

  tdr_yml <- fs::path(tdr_dir, "tabular-data-resource.yaml")
  .x |>
    add_attrs(path = fs::path_rel(tdr_csv, start = tdr_dir)) |>
    write_tdr(file = tdr_yml, codec = codec)

  return(invisible(.x))
}
