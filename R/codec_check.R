#' Check a tabular-data-resource against CoDEC specifications
#'
#' `check_codec_tdr_csv()` will use each of these functions to
#' check that
#' - the file structure is correct
#' - the metadata can be read using CoDEC specifications
#' - the CSV data can be read in with accompanying metadata
#' - the data contains a census tract identifier column
#' - the data contains a year (or year and month) column(s)
#' - all fields in the CSV data are described in the metadata and vice-versa
#' See `vignette("codec-specs")` for the CoDEC specifications.
#' @param tdr a codec tabular-data-resource
#' @param tdr_md a codec tabular-data-resource metadata list object
#' @param path path to tdr folder
#' @param name the name field from tabular-data-resource.yaml
#' @return for `check_codec_tdr_csv`, a tibble with added
#' tabular-data-resource attributes (equivalent to read_tdr_csv with `codec = TRUE`)
#' @export
check_codec_tdr_csv <- function(path) {
  check_files(path)
  tdr <- read_tdr(path)$tdr
  check_codec_tdr(tdr)

  md_fields <- names(tdr$schema$fields)
  d_fields <- names(readr::read_csv(read_tdr(path)$csv_file, n_max = 0, show_col_types = FALSE))
  if(! all(d_fields %in% md_fields)) {
    stop("the metadata does not describe all fields in the data", call. = FALSE)
  }
  if(! all(md_fields %in% d_fields)) {
    stop("the metadata describes fields that are not in the data", call. = FALSE)
  }

  tdr_d <- read_tdr_csv(path)
  check_data(tdr_d)
  return(invisible(tdr_d))
}

#' Check data
#' @rdname check_codec_tdr_csv
check_data <- function(tdr) {
  check_census_tract_id(tdr)
  check_date(tdr)
}

#' Check census tract id column
#' @rdname check_codec_tdr_csv
check_census_tract_id <- function(tdr) {
  census_tract_id_names <- paste0("census_tract_id", c("_2000", "_2010", "_2020"))

  # has census_tract_id_{year} or census_tract_id column
  if (!any(names(tdr) %in% census_tract_id_names)) {
    stop("must contain a census tract id column called census_tract_id_2000, census_tract_id_2010, or census_tract_id_2020", call. = FALSE)
  }

  # make sure only one tract column
  if (sum(names(tdr) %in% census_tract_id_names) > 1) {
    stop("must contain only one census tract id column", call. = FALSE)
    }

  census_tract_id_name <- census_tract_id_names[census_tract_id_names %in% names(tdr)]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")

  required_census_tract_ids <-
    parse(text = paste0("cincy::tract_tigris_", census_tract_id_year)) |>
    eval() |>
    purrr::pluck(paste0("census_tract_id_", census_tract_id_year))

  if (!all(required_census_tract_ids %in% tdr[[census_tract_id_name]])) {
    stop("the census tract id column, ",
      census_tract_id_name,
      ", does not contain every census tract in ",
      paste0("`cincy::tract_tigris_", census_tract_id_year, "`"),
      call. = FALSE
    )
  }

  return(invisible(tdr))
}

#' Check date
#' @rdname check_codec_tdr_csv
check_date <- function(tdr) {

  if (! "year" %in% names(tdr)) {
    stop("must contain a 'year' column", call. = FALSE)
  }

  years <- unique(tdr$year)
  if (! identical(years, as.integer(years))) {
    stop("the 'year' field must only contain integer years", call. = FALSE)
  }

  if ("month" %in% names(tdr)) {
    if (! all(tdr$month %in% 1:12)) {
      stop("the 'month' field  must only contain integer values 1-12", call. = FALSE)
    }
  }
  return(invisible(tdr))
}

#' Check files
#' @rdname check_codec_tdr_csv
#' @export
check_files <- function(path) {
  tdr_dir <- fs::path(path)
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

  return(invisible(NULL))
}

#' check CoDEC tdr
#' @rdname check_codec_tdr_csv
#' @export
check_codec_tdr <- function(tdr_md) {

  # must have "name" and "path" descriptors
  if (!purrr::pluck_exists(tdr_md, "name")) stop("`name` property descriptor is required", call. = FALSE)
  if (!purrr::pluck_exists(tdr_md, "path")) stop("`path` property descriptor is required", call. = FALSE)

  # name must be valid
  check_tdr_name(tdr_md$name)

  # path must be valid
  check_tdr_path(tdr_md$path)

  # "name" must be identical to filename of "path" (without ".csv")
  if (!tdr_md$name == fs::path_ext_remove(fs::path_file(tdr_md$path))) {
    stop("name: ", tdr_md$name, " does not match ",
      "file in path: ", tdr_md$path,
      call. = FALSE
    )
  }

  # all properties must be in codec specs
  codec_property_names <- names(codec_tdr()$property)
  codec_schema_names <- names(codec_tdr()$schema)
  codec_fields_names <- names(codec_tdr()$fields)

  tdr_property_names <- names(tdr_md)
  tdr_schema_names <- names(tdr_md$schema)
  tdr_fields_names <-
    tdr_md$schema$fields |>
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

  return(invisible(tdr_md))
}


#' check CoDEC tdr name
#' @rdname check_codec_tdr_csv
#' @export
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

#' check CoDEC tdr path
#' @rdname check_codec_tdr_csv
#' @export
check_tdr_path <- function(path) {
  # path is a character string
  if (!is.character(path)) stop("'path' must be character string", call. = FALSE)
  # path ends with .csv
  if (! fs::path_ext(path) == "csv") stop("'path' must end with '.csv'", call. = FALSE)
  # path can be a URL
  if (is_url(path)) return(invisible(NULL))
  # if not URL, check for absolute path
  if (fs::is_absolute_path(path)) stop("'path' must be a relative file path")
  # posix style path can't be enforced?
  return(invisible(NULL))
}

