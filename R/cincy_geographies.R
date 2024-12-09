#' cincy_census_geo("tract", "2024")
#' cincy_census_geo("tract", "2020")
#' cincy_census_geo("tract", "2019")
#' cincy_census_geo("bg", "2020")
#' cincy_census_geo("bg", "2019")
cincy_census_geo <- function(geography = c("tract", "bg"), vintage = as.character(2024:2013)) {
  geography <- rlang::arg_match(geography)
  vintage <- rlang::arg_match(vintage)
  tiger_url <- glue::glue(
    "https://www2.census.gov/geo/tiger/TIGER{vintage}",
    "/{toupper(geography)}/tl_{vintage}_39_{geography}.zip"
  )
  tiger_local <- dpkg::stow_url(tiger_url)
  out <-
    sf::read_sf(glue::glue("/vsizip/", tiger_local),
      query = glue::glue("SELECT GEOID FROM tl_{vintage}_39_{geography} WHERE COUNTYFP = '061'")
    )
  names(out) <- tolower(names(out))
  out$s2_geography <- sf::st_as_s2(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

#' cincy_county_geo("2024")
cincy_county_geo <- function(vintage = as.character(2024:2013)) {
  vintage <- rlang::arg_match(vintage)
  tiger_url <- glue::glue("https://www2.census.gov/geo/tiger/TIGER{vintage}/COUNTY/tl_{vintage}_us_county.zip")
  tiger_local <- dpkg::stow_url(tiger_url)
  out <-
    sf::read_sf(glue::glue("/vsizip/", tiger_local),
      query = glue::glue("SELECT GEOID FROM tl_{vintage}_us_county WHERE GEOID = '39061'")
    )
  return(sf::st_as_s2(out$geometry))
}

cincy_neighborhood_geo <- function(vintage, type = c("tract", "community_council", "sna")) {
}

## get_cincy_city

#' cincy_zcta_geo()
#' cincy_zcta_geo("2018")
cincy_zcta_geo <- function(vintage = as.character(2024:2013)) {
  vintage <- rlang::arg_match(vintage)
  is_vintage_old <- vintage %in% as.character(2013:2019)
  tiger_url <- glue::glue(
    "https://www2.census.gov/geo/tiger/TIGER{vintage}/",
    ifelse(is_vintage_old, "ZCTA5", "ZCTA520"),
    "/tl_{vintage}_us_zcta",
    ifelse(is_vintage_old, "510", "520"),
    ".zip"
  )
  tiger_local <- dpkg::stow_url(tiger_url)
  out <-
    sf::read_sf(glue::glue("/vsizip/", tiger_local),
      query = glue::glue(
        "SELECT ",
        ifelse(is_vintage_old, "GEOID10", "GEOID20"),
        " FROM tl_{vintage}_us_zcta",
        ifelse(is_vintage_old, "510", "520"),
        " WHERE ",
        ifelse(is_vintage_old, "GEOID10", "GEOID20"),
        " IN ({paste(paste0(\"'\", cincy_zip_codes, \"'\"), collapse = \", \")})"
      )
    )
  names(out) <- gsub("[0-9]", "", tolower(names(out)))
  out$s2_geography <- sf::st_as_s2(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

# from cincy::zcta_tiger_2020 (version 1.1.0) on 2024-11-08
cincy_zip_codes <-
  c(
    "45214", "45208", "45236", "45247", "45225", "45205", "45220",
    "45206", "45223", "45232", "45174", "45207", "45209", "45212",
    "45213", "45217", "45218", "45229", "45238", "45242", "45051",
    "45002", "45227", "45211", "45215", "45216", "45219", "45224",
    "45033", "45237", "45239", "45248", "45041", "45267", "45030",
    "45252", "45244", "45202", "45249", "45255", "45226", "45203",
    "45246", "45111", "45147", "45052", "45240", "45241", "45243",
    "45251", "45001", "45204", "45231", "45230", "45233"
  )
