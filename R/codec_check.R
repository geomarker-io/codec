check_census_tract_id <- function(x) {
  census_tract_id_names <- paste0("census_tract_id", c("_2000", "_2010", "_2020"))
  # has census_tract_id_{year} or census_tract_id column
  if (sum(names(x) %in% census_tract_id_names) != 1) {
    return("must contain one census tract id column called census_tract_id_2000, census_tract_id_2010, or census_tract_id_2020")
  }
  census_tract_id_name <- census_tract_id_names[census_tract_id_names %in% names(x)]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")
  required_census_tract_ids <-
    parse(text = paste0("cincy::tract_tigris_", census_tract_id_year)) |>
    eval() |>
    purrr::pluck(paste0("census_tract_id_", census_tract_id_year))

  if (!all(required_census_tract_ids %in% x[[census_tract_id_name]])) {
    return(glue::glue("the census tract id column, {census_tract_id_name}, does not contain every census tract in `cincy::tract_tigris_{census_tract_id_year}`"))
  }
  return(invisible(NULL))
}

check_date <- function(x) {
  if (! "year" %in% names(x)) {
    return("must contain a 'year' column")
  }
  years <- unique(x$year)
  if (! all(years %in% 1970:2099)) {
    return("the 'year' field must only contain integer years between 1970 and 2099")
  }
  if ("month" %in% names(x)) {
    if (! all(x$month %in% 1:12)) {
      return("the 'month' field  must only contain integer values 1-12")
    }
  }
  return(invisible(NULL))
}

check_name <- function(name) {
  # name is a character string
  if (!is.character(name)) return("`name` must be a character string")
  # name does not have uppercase letters
  if (stringr::str_detect(name, "[[:upper:]]")) return("`name` must be all lowercase")
  # name does not have spaces
  if (stringr::str_detect(name, " ")) return("`name` must not contain spaces")
  # nonalphanumeric characters are either -, _, or .
  if (!all(stringr::str_detect(unlist(stringr::str_extract_all(name, "[^[:alnum:]]")), "[_.-]"))) {
    return("`name` must only contain a-z, 0-9, -, _, .")
  }
  return(invisible(NULL))
}
