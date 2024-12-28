#' Read a dpkg from CoDEC into R
#'
#' Public data packages are downloaded from `gh://geomarker-io/codec/` using
#' `dpkg::stow()` to cache a local copy in the user's data directory.
#' @param codec_dpkg name of CoDEC dpkg
#' @param overwrite logical; re-download the remote file even though
#' a local file with the same name exists?
#' @returns a CoDEC data package (see `dpkg::as_dpkg()`)
#' @export
#' @examples
#' get_codec_dpkg("drivetime-v0.2.2")
get_codec_dpkg <- function(codec_dpkg, overwrite = FALSE) {
  out <-
    paste0("gh://geomarker-io/codec/", codec_dpkg) |>
    dpkg::stow(overwrite = overwrite) |>
    dpkg::read_dpkg()
  return(out)
}

#' Convert a tibble to a CoDEC data package
#' 
#' **CoDEC Specifications:**
#' 1. The data must include a [census tract](https://www2.census.gov/geo/pdfs/education/CensusTracts.pdf)
#' identifier column (i.e., `census_tract_id_2010`, or `census_tract_id_2020`).
#' The column must contain 11-digit
#' [GEOID](https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html)
#' identifiers for every census tract in Hamilton County, OH.
#' 2. The data includes a year column (`year`), an integer year representing the
#' vintage of the data (e.g. `2021`).
#' The data can optionally include a month column (`month`), an integer month of the year.
#' 3. Data must be structured in a tidy format such that each row is an observation
#' for a specific census tract at a specific year (and month).
#'
#' @param x data.frame or tibble meeting CoDEC data specifications above
#' @param name see `dpkg::as_dpkg()`
#' @param version see `dpkg::as_dpkg()`
#' @param title see `dpkg::as_dpkg()`
#' @param description see `dpkg::as_dpkg()`
#' @param homepage see `dpkg::as_dpkg()`
#' @returns for as_codec_dpkg, a dpkg object
#' @export
as_codec_dpkg <- function(x, name, version, title = character(), description = character(), homepage = character()) {
  chk1 <- check_census_tract_id(x)
  if (!is.null(chk1)) rlang::abort(chk1)
  chk2 <- check_date(x)
  if (!is.null(chk2)) rlang::abort(chk2)
  out <- dpkg::as_dpkg(x, name = name, version = version, title = title, description = description, homepage = homepage)
  return(out)
}

#' is_codec_dpkg
#' @rdname as_codec_dpkg
#' @returns for is_codec_dpkg, a logical
#' @export
#' @examples
#' is_codec_dpkg(mtcars)
is_codec_dpkg <- function(x) {
  list_dpkg <- dpkg::dpkg_meta(x)
  list_dpkg$x <- tibble::as_tibble(x)
  out <-
    tryCatch(
      {
        do.call(as_codec_dpkg, list_dpkg)
        TRUE
      },
      error = function(x) FALSE
    )
  return(out)
}

check_census_tract_id <- function(x) {
  census_tract_id_names <- paste0("census_tract_id", c("_2010", "_2020"))
  # has census_tract_id_{year} or census_tract_id column
  if (sum(names(x) %in% census_tract_id_names) != 1) {
    return("must contain one census tract id column called census_tract_id_2010 or census_tract_id_2020")
  }
  census_tract_id_name <- census_tract_id_names[census_tract_id_names %in% names(x)]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")
  if (census_tract_id_year == "2010") {
    required_census_tract_ids <- cincy_census_geo("tract", "2019")$geoid
  } else if (census_tract_id_year == "2020") {
    required_census_tract_ids <- cincy_census_geo("tract", "2020")$geoid
  }

  if (!all(required_census_tract_ids %in% x[[census_tract_id_name]])) {
    return(glue::glue("the census tract id column, {census_tract_id_name},",
      "does not contain every census tract for that vintage;",
      "Check for missing census tract observations and",
      "check that you are using the correct vintage.",
      .sep = " "
    ))
  }
  return(invisible(NULL))
}

check_date <- function(x) {
  if (!"year" %in% names(x)) {
    return("must contain a 'year' column")
  }
  years <- unique(x$year)
  if (!all(years %in% 1970:2099)) {
    return("the 'year' field must only contain integer years between 1970 and 2099")
  }
  if ("month" %in% names(x)) {
    if (!all(x$month %in% 1:12)) {
      return("the 'month' field  must only contain integer values 1-12")
    }
  }
  return(invisible(NULL))
}
