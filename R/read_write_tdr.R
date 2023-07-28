read_tdr_file <- function(tdr_file) {
  tdr_file <- fs::fs_path(tdr_file)
  # if no tabular-data-resource.yaml file, assume it a directory containing one
  if (fs::is_dir(tdr_file)) {
    tdr_file <- fs::path(tdr_file, "tabular-data-resource.yaml")
  }
  tdr <- yaml::read_yaml(tdr_file)
  csv_file <- fs::path(fs::path_dir(tdr_file), tdr$path)
  return(list("tdr" = tdr, "csv_file" = csv_file))
}

is_url <- function(.x) grepl("^((http|ftp)s?|sftp)://", .x)

read_tdr_url <- function(tdr_url) {
  if (!grepl("/tabular-data-resource.yaml", tdr_url)) {
    tdr_url <- paste0(tdr_url, "/tabular-data-resource.yaml")
  }
  tdr <- yaml::read_yaml(url(tdr_url))
  csv_file <-
    gsub("tabular-data-resource.yaml",
      tdr$path,
      tdr_url,
      fixed = TRUE
    )
  return(list("tdr" = tdr, "csv_file" = csv_file))
}

#' Read a tabular data resource into R from disk or the web
#'
#' In addition, the path (or url) of the CSV is returned,
#' relative to how `tdr_location` is specified.
#' @param tdr_location path or url to a `tabular-data-resource.yaml` file
#' (or a directory containing a `tabular-data-resource.yaml` file)
#' @return a list of (1) the "tdr" and (2) the path to its "csv_file"
#' @export
#' @examples
#' read_tdr("https://github.com/geomarker-io/hamilton_landcover/releases/download/v0.1.0") |>
#'   str(4)
read_tdr <- function(tdr_location) {
  ifelse(is_url(tdr_location), read_tdr_url, read_tdr_file)(tdr_location)
}

#' read a CSV tabular data resource into R from disk or the web
#'
#' The CSV file defined in a tabular-data-resource yaml file
#' is read into R using `readr::read_csv()`. Metadata
#' (properties and schema) are stored as attributes
#' of the returned tibble and are also used to set
#' the column classes of the returned data.frame or tibble.
#' Files starting with `http://` or `https://` will be automatically
#' downloaded locally first.
#'
#' @param tdr_file path or url to a `tabular-data-resource.yaml` file
#' (or a directory containing a `tabular-data-resource.yaml` file)
#' @param codec logical; use only CoDEC properties?
#' @param ... additional options passed onto `readr::read_csv()`
#' @return tibble with added tabular-data-resource attributes
#' @export
read_tdr_csv <- function(tdr_file, codec = TRUE, ...) {
  tdr_c <- read_tdr(tdr_file)

  # should check_files() be run if it is a file?
  ## if (!is_url(tdr_c)) check_files(fs::path_dir(tdr_file))

  flds <- purrr::pluck(tdr_c$tdr, "schema", "fields")

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
      file = tdr_c$csv_file,
      col_names = TRUE,
      col_types = paste(col_classes, collapse = ""),
      col_select = all_of({{ col_names }}),
      locale = readr::locale(
        encoding = "UTF-8",
        decimal_mark = ".",
        grouping_mark = ""
      ),
      name_repair = "check_unique",
      na = c("NA", ""),
      ...,
    )

  if (length(lvls) > 0) {
    for (lvl in names(lvls)) {
      out <- dplyr::mutate(
        out,
        {{ lvl }} := forcats::fct_expand(
          dplyr::pull(out, {{ lvl }}),
          as.character(lvls[[lvl]])
        )
      )
    }
  }

  out <- add_attr_from_tdr(out, tdr_c$tdr, codec = codec)
  return(out)
}

#' extract data resource metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
#' @param codec logical; include only CoDEC properties or schema? (see `?codec_tdr` for details)
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
#'
#' @param .x data.frame or tibble with metadata attributes (at least a 'name' attribute)
#' @param dir path to directory where tdr will be created; see details
#' @param codec logical; use only CoDEC properties?
#' @return .x, invisibly
#' @export
write_tdr_csv <- function(.x, dir = getwd(), codec = TRUE) {

  tdr_name <- attr(.x, "name", exact = TRUE)

  if (is.null(tdr_name)) stop("there is no 'name' attribute", call. = FALSE)

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
