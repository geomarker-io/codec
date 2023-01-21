#' Check for census tract id column
#'
#' Errors will be raised if the CODEC tabular data resource does not meet the following requirements:
#' - data MUST contain a census tract identifier column titled
#' `census_tract_id_2000`, `census_tract_id_2010`, or `census_tract_id_2020`
#' - census tract identifier column MUST contain all census tract identifiers
#' in Hamilton County, OH for the appropriate vintage
#'
#' @param .x a codec tabular-data-resource
#' @return .x, invisibly
#' @export
check_census_tract_id <- function(.x) {

  census_tract_id_names <- paste0("census_tract_id_", c("2000", "2010", "2020"))

  # has census_tract_id_{year} column
  if(!any(names(.x) %in% census_tract_id_names)) {
    stop("must contain a census tract id column called census_tract_id_2000, census_tract_id_2010, or census_tract_id_2020")
  }

  census_tract_id_name <- census_tract_id_names[census_tract_id_names %in% names(.x)]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")

  required_census_tract_ids <-
    parse(text = paste0("cincy::tract_tigris_", census_tract_id_year)) |>
    eval() |>
    purrr::pluck("census_tract_id")

  if(!all(required_census_tract_ids %in% .x[[census_tract_id_name]])) {
    stop("the census tract id column, ", census_tract_id_name,
         ", does not contain every census tract", call. = FALSE)
  }
  
    return(invisible(.x))
}

#' Check for files
#'
#' Errors will be raised if the CODEC tabular data resource does not meet the following requirements:
#'
#' - tdr directory must exist and have a matching CSV data file and a tabular-data-resource.yaml file
#' - both files must use UTF-8 character encoding and have newlines encoded as `\n` or `\r\n`
#' - the data file must:
#'   - end in `.csv`
#'   - have a header row containing the unique name of each field
#'   - use `.` as the decimal mark (e.g., `1.34`)
#'   - not use a grouping mark (e.g., `1200` instead of `1,200`)
#'   - be able to be read using the [RFC 4180 standard](https://www.rfc-editor.org/rfc/rfc4180) for CSV files
#' @param .x path to folder containing the tdr
#' @return .x, invisibly
#' @export
check_files <- function(.x) {

  tdr_dir <- fs::path(.x)
  tdr_csv <- fs::path(tdr_dir, fs::path_file(tdr_dir), ext = "csv")
  tdr_yaml <- fs::path(tdr_dir, "tabular-data-resource.yaml")

  if (!fs::dir_exists(tdr_dir)) {
    stop("cannot find ", tdr_dir, call. = FALSE)
  }

  if (!fs::file_exists(tdr_csv)) {
    stop("cannot find matching CSV data file, ", tdr_csv)
  }
  
  if (!fs::file_exists(tdr_yaml)) {
    stop("cannot find metadata file, ", tdr_yaml)
  }

  # test encoding
  if (!stringi::stri_enc_isutf8(tdr_csv)) {
    stop(tdr_csv, " is not encoded in UTF-8")
  }
  if (!stringi::stri_enc_isutf8(tdr_yaml)) {
    stop(tdr_yaml, " is not encoded in UTF-8")
  }

  # try to read (first 100 lines of) CSV file
  test_read_csv_file <-
    purrr::safely(readr::read_csv)(
      file = tdr_csv,
      n_max = 100,
      col_names = TRUE,
      show_col_types = FALSE,
      locale = readr::locale(
        encoding = "UTF-8",
        decimal_mark = ".",
        grouping_mark = ""
      ),
      name_repair = "check_unique",
      )

  if (!is.null(test_read_csv_file$error)) {
    stop(tdr_csv, " could not be read without error:\n\n", test_read_csv_file$error)
  }

  return(invisible(.x))
}

#' Check for missing values
#'
#' Errors will be raised if the CODEC tabular data resource does not meet the following requirements:
#'
#' @param .x a codec tabular-data-resource
#' @return .x, invisibly
#' @export
check_missing_values <- function(.x) {
  return(invisible(.x))
}
