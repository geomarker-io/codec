#' Check a tabular-data-resource against CODEC specifications
#'
#' This function will check that the file structure is correct,
#' that the metadata can be read using CODEC specifications,
#' that the CSV data can be read in with accompanying metadata,
#' and that the data contains a census tract identifier column.
#' @param .x path to folder containing the tdr
#' @return tibble with added tabular-data-resource attributes
#' (equivalent to read_tdr_csv with `codec = TRUE`)
#' @export
check_codec_tdr_csv <- function(.x) {
  check_files(.x)
  tdr <- read_tdr(.x)$tdr
  check_codec_tdr(tdr)
  tdr_d <- read_tdr_csv(.x)
  check_census_tract_id(tdr_d)
  return(invisible(tdr_d))
}

#' check CODEC tdr
#' @param tdr a codec tabular-data-resource list object
#' @return `tdr`, invisibly
#' @export
check_codec_tdr <- function(tdr) {

  # must have "name" and "path" descriptors
  if (!purrr::pluck_exists(tdr, "name")) stop("`name` property descriptor is required", call. = FALSE)
  if (!purrr::pluck_exists(tdr, "path")) stop("`path` property descriptor is required", call. = FALSE)

  # name must be valid
  check_tdr_name(tdr$name)

  # "name" must be identical to filename of "path" (without ".csv")
  if (!tdr$name == fs::path_ext_remove(fs::path_file(tdr$path))) {
    stop("name: ", tdr$name, " does not match ",
      "file in path: ", tdr$path,
      call. = FALSE
    )
  }

  # all properties must be in codec specs
  codec_property_names <- names(codec_tdr()$property)
  codec_schema_names <- names(codec_tdr()$schema)
  codec_fields_names <- names(codec_tdr()$fields)

  tdr_property_names <- names(tdr)
  tdr_schema_names <- names(tdr$schema)
  tdr_fields_names <-
    tdr$schema$fields |>
    purrr::map(names) |>
    unlist() |>
    unique()

  if (!all(tdr_property_names %in% codec_property_names)) {
    stop("property descriptors not in the codec specification are not allowed: ",
      paste(tdr_property_names[!tdr_property_names %in% codec_property_names], collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(tdr_schema_names %in% codec_schema_names)) {
    stop("schema descriptors not in the codec specification are not allowed: ",
      paste(tdr_schema_names[!tdr_schema_names %in% codec_schema_names], collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(tdr_fields_names %in% codec_fields_names)) {
    stop("field descriptors not in the codec specification are not allowed: ",
      paste(tdr_fields_names[!tdr_fields_names %in% codec_fields_names], collapse = ", "),
      call. = FALSE
    )
  }

  # TODO all descriptors should be neither empty or missing

  return(invisible(tdr))
}


#' check CODEC tdr name
#'
#' Name must be an identifier string composed of lower
#' case alphanumeric characters, _, -, and .
#' @param name the name field from tabular-data-resource.yaml
#' @return NULL, invisibly
#' @keywords internal
check_tdr_name <- function(name) {
  # name is a character string
  if (!is.character(name)) stop("'name' must be character string.", call. = FALSE)
  # name does not have uppercase letters
  if (stringr::str_detect(name, "[[:upper:]]")) stop("'name' must be all lowercase.", call. = FALSE)
  # name does not have spaces
  if (stringr::str_detect(name, " ")) stop("'name' must not contain spaces.", call. = FALSE)
  # nonalphanumeric characters are either -, _, or .
  if (!all(stringr::str_detect(unlist(stringr::str_extract_all(name, "[^[:alnum:]]")), "[_.-]"))) {
    stop("Accepted non-alphanumeric characters for 'name' are '-', '_', and '.'", call. = FALSE)
  }
  return(invisible(NULL))
}

#' check CODEC tdr path
#' @keywords internal
check_tdr_path <- function(path) {
  # must be posix-style URL or relative file path
  # must end in .csv
}

#' Check for census tract id column
#'
#' Errors will be raised if the CODEC tabular data resource does not meet the following requirements:
#' - data MUST contain a single census tract identifier column titled
#' `census_tract_id_2000`, `census_tract_id_2010`, `census_tract_id_2020`,
#' or `census_tract_id`
#' - (if `census_tract_id`, then an accompanying `census_tract_vintage` column
#' containing one distinct year MUST be present)
#' - census tract identifier column MUST contain all census tract identifiers
#' in Hamilton County, OH for the appropriate vintage
#'
#' @param .x a codec tabular-data-resource
#' @return .x, invisibly
#' @keywords internal
check_census_tract_id <- function(.x) {
  census_tract_id_names <- paste0("census_tract_id", c("", "_2000", "_2010", "_2020"))

  # has census_tract_id_{year} or census_tract_id column
  if (!any(names(.x) %in% census_tract_id_names)) {
    stop("must contain a census tract id column called census_tract_id, census_tract_id_2000, census_tract_id_2010, or census_tract_id_2020", call. = FALSE)
  }

  # make sure only one tract column
  if (sum(names(.x) %in% census_tract_id_names) > 1) {
    stop("must contain only one census tract id column", call. = FALSE)
    }

  census_tract_id_name <- census_tract_id_names[census_tract_id_names %in% names(.x)]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")

  # if census_tract_id
  if (census_tract_id_name == "census_tract_id") {
    # check for census_tract_vintage column
    if (!"census_tract_vintage" %in% names(.x)) {
      stop("if census_tract_id is used as the tract identifer column, a census_tract_vintage column must exist")
    }
    census_tract_id_year <- unique(.x$census_tract_vintage)
    # check that vintage column only has one year value in it
    if (length(census_tract_id_year) > 1) {
      stop("the census_tract_vintage column must have only one unique value")
    }
    # check that vintage is either 2000, 2010, or 2020
    if (!as.character(census_tract_id_year) %in% c("2000", "2010", "2020")) {
      stop("census_tract_vintage must be 2000, 2010, or 2020", call. = FALSE)
    }
  }

  required_census_tract_ids <-
    parse(text = paste0("cincy::tract_tigris_", census_tract_id_year)) |>
    eval() |>
    purrr::pluck(glue::glue("census_tract_id_{census_tract_id_year}"))

  if (!all(required_census_tract_ids %in% .x[[census_tract_id_name]])) {
    stop("the census tract id column, ",
      census_tract_id_name,
      ", does not contain every census tract in ",
      paste0("`cincy::tract_tigris_", census_tract_id_year, "`"),
      call. = FALSE
    )
  }

  return(invisible(.x))
}

#' Check files
#'
#' Errors will be raised if the CODEC tabular data resource does not meet the following requirements:
#' - tdr directory must contain a `.csv` with the same name and a `tabular-data-resource.yaml` file
#' - both files must use UTF-8 character encoding and have newlines encoded as `\n` or `\r\n`
#' - the data file must:
#'   - follow the [RFC 4180 standard](https://www.rfc-editor.org/rfc/rfc4180) for CSV files *and*
#'   - have a header row containing a *unique* name for each field
#'   - use `.` as the decimal mark (e.g., `1.34`)
#'   - not use a grouping mark (e.g., `1200` instead of `1,200`)
#' @param .x path to folder containing the tdr
#' @return .x, invisibly
#' @export
check_files <- function(.x) {
  tdr_dir <- fs::path(.x)
  tdr_csv <- fs::path(tdr_dir, fs::path_file(tdr_dir), ext = "csv")
  tdr_yaml <- fs::path(tdr_dir, "tabular-data-resource.yaml")

  # check for files and folder
  if (!fs::dir_exists(tdr_dir)) {
    stop("cannot find ", tdr_dir, call. = FALSE)
  }

  if (!fs::file_exists(tdr_csv)) {
    stop("cannot find matching CSV data file, ", tdr_csv)
  }

  if (!fs::file_exists(tdr_yaml)) {
    stop("cannot find metadata file, ", tdr_yaml, call. = FALSE)
  }

  # test encoding
  if (!stringi::stri_enc_isutf8(tdr_csv)) {
    stop(tdr_csv, " does not seem to be encoded using UTF-8", call. = FALSE)
  }
  if (!stringi::stri_enc_isutf8(tdr_yaml)) {
    stop(tdr_yaml, " does not seem to be encoded using UTF-8", call. = FALSE)
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
        grouping_mark = "",
      ),
      name_repair = "check_unique",
    )

  if (!is.null(test_read_csv_file$error)) {
    stop(tdr_csv, " could not be read without error:\n\n", test_read_csv_file$error, call. = FALSE)
  }

  return(invisible(.x))
}
