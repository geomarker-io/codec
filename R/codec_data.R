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
    stop("must contain a census tract id column called census_tract_id_2000, census_tract_id_2010, or census_trat_id_2020")
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

check_files <- function(.x) {

}

check_missing_values <- function() {

}
