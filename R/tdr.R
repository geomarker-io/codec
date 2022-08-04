#' make metadata list from attributes of data.frame
#'
#' @param .x a data.frame or tibble
#' @return frictionless metadata as a list
make_tdr_from_attr <- function(.x) {
  metad <- as.list(attributes(.x))
  metad$schema <- list(fields = purrr::map(.x, attributes))
  return(metad)
}

#' extract data resource metadata from a data frame and save it to a file
#'
#' @param .x a data.frame or tibble
#' @param file name of yaml file to write metadata to
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
save_tdr <- function(.x, file = "tabular-data-resource.yaml") {
  .x |>
    add_attrs(profile = "tabular-data-resource") |>
    make_tdr_from_attr() |>
    yaml::as.yaml() |>
    cat(file = file)

  return(invisible(.x))
}

#' read metadata in from a tabular-data-resource.yaml file
#'
#' @param file name of yaml file to write metadata to
#' @return a list of frictionless metadata
#' @export
read_tdr <- function(file = "tabular-data-resource.yaml") {
  # TODO if file is a folder, look for "tabular-data-resource.yaml" there
  metadata <- yaml::yaml.load_file(file)
  return(metadata)
}

#' add attributes in a metadata list to a data.frame
#'
#' @param .x a data.frame or tibble
#' @param metadata a list containing frictionless metadata (usually created with `read_tdr()`)
#' @return .x with added frictionless metadata attributes
make_attr_from_tdr <- function(.x, metadata) {

  descriptors <-
    metadata |>
    ## TODO better way to filter list based on name??
    tibble::enframe() |>
    dplyr::filter(.data$name %in% codec_cdr()) |>
    tibble::deframe()

  out <- add_attrs(.x, !!!descriptors)

  out <- purrr::map2_dfc(.x, metadata$schema$fields, ~ add_attrs(..1, !!!..2))
  attributes(out) <- attributes(.x)
}

#' read a CSV file and frictionless metadata into R
## read_tdr_csv <- function(file = "tabular-data-resource.yaml") {

##   metadata <- read_tdr(file)

##   descriptors <-
##     metadata |>
##     tibble::enframe() |>
##     dplyr::filter(name %in% cdr) |>
##     tibble::deframe()

##   col_names <- names(metadata$schema$fields)

##   col_types <- purrr::map(metadata$schema$fields, "type")

##   col_classes <- purrr::map(col_types, ~ type_class_cw[.])

##   # TODO change strings that have enum to factors
##   levels <- purrr::map(metadata$schema$fields, "constraints", "enum")

##   data_path <- metadata$path

##   # read the CSV
##   # TODO use dplyr::col_guess for any columns not specified in the metadata?  warning? or error?
##   # or a `strict = TRUE` argument

##   out <- make_attr_from_tdr(out, metadata)
## }

## type_class_cw <- c(
##   "string" = readr::col_character,
##   "date" = readr::col_date,
##   "number" = readr::col_double,
##   ## "string" FACTOR ,
##   "time" = readr::col_time,
##   "integer" = readr::col_integer,
##   "boolean" = readr::col_logical,
##   "datetime" = readr::col_datetime
## )

## write_tdr_csv <- function() {

## }

  
## read_codec_csv <- function(file) {

##   col_logical(),
##   col_integer(),
##   col_double(),
##   col_character(),
##   col_factor(levels),
##   col_date(),
##   col_datetime(),
##   col_number(),
##   col_guess()

##   col_types
##   na

##   readr::read_csv(
##     file,
##     col_names = TRUE,
##     col_types = cols(),
##     col_select = NULL,
##     locale = readr::locale(
##       encoding = "UTF-8",
##       decimal_mark = ".",
##       grouping_mark = ""
##     ),
##     na = na,
##     quoted_na = TRUE,
##     quote = "\"",
##     name_repair = "check_unique",
##     num_threads = readr_threads(),
##     progress = show_progress(),
##     show_col_types = should_show_types(),
##     lazy = FALSE
##   )

  
## }

