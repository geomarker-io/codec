#' cincy_census_geo()
#' cincy_census_geo("bg", "2019", s2_geography = FALSE)
cincy_census_geo <- function(geography = c("tract", "bg"), vintage = as.character(2024:2013), s2_geography = TRUE) {
  geography <- rlang::arg_match(geography)
  vintage <- rlang::arg_match(vintage)
  tiger_url <- glue::glue("https://www2.census.gov/geo/tiger/TIGER{vintage}",
                          "/{toupper(geography)}/tl_{vintage}_39_{geography}.zip")
  tiger_local <- dpkg::stow_url(tiger_url)
  out <-
    sf::read_sf(glue::glue("/vsizip/", tiger_local),
      query = glue::glue("SELECT GEOID FROM tl_{vintage}_39_{geography} WHERE COUNTYFP = '061'")
    )
  names(out) <- tolower(names(out))
  if (s2_geography) {
    out$s2_geography <- sf::st_as_s2(out$geometry)
    out <- sf::st_drop_geometry(out)
  }
  return(out)
}


get_cincy_neighborhoods <- function(vintage) {
}

## get_cincy_city

## get_cincy_county

get_cincy_zctas <- function(vintage) {
  tiger_zcta_url <- glue::glue("https://www2.census.gov/geo/tiger/TIGER{vintage}/ZCTA520/tl_{vintage}_us_zcta520.zip")
}
