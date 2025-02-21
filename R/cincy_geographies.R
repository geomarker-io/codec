#' Cincy census tracts and block groups
#'
#' Read tract and block group ("bg") geographies from the online Census
#' [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' files into R
#' @param geography which type of cincy census geography to return
#' @param vintage a character vector of a year corresponding to the vintage of TIGER/Line data
#' @details
#' Compressed shapefiles are downloaded from TIGER into an R user data directory and will be cached
#' for use across other R sessions (see `?dpkg::stow` for more details).
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`)
#' @export
#' @examples
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
  out <- sf::st_as_sf(out)
  return(out)
}

#' Cincy county
#' @rdname cincy_census_geo
#' @export
#' @examples
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

#' Install CAGIS GIS database
#' 
#' This installs the CAGIS Open Data GIS database (`.gdb`) into the data
#' directory for the codec package. Once downloaded, it will be reused
#' across R sessions on the same computer.
#' The geodatabase contains many
#' [layers](https://www.cagis.org/Opendata/Quarterly_GIS_Data/OpenData_Layer_List.txt) that are
#' updated quarterly. (Historical geodatabases are not available here.)
#' @seealso This function is called by `cincy_neighborhood_geo()`, `cincy_city_geo()`
#' and others that import individual layers.
#' @param cagis_data_url the url to the CAGIS Open Data .gdb.zip file; this changes quarterly, so
#' [check](https://www.cagis.org/Opendata/Quarterly_GIS_Data) for something more recent if the file cannot be found
#' @export
#' @examples
#' options(timeout = max(2500, getOption("timeout")), download.file.method = "libcurl")
#' install_cagis_data()
#' sf::st_layers(install_cagis_data())$name
install_cagis_data <- function(cagis_data_url = "https://www.cagis.org/Opendata/Quarterly_GIS_Data/CAGISOpenDataQ1_2025.gdb.zip") {
  withr::local_options(timeout = 600)
  cagis_gdb_name <- tools::file_path_sans_ext(basename(cagis_data_url))
  dest <- file.path(tools::R_user_dir(package = "codec", "data"), cagis_gdb_name)
  if (file.exists(dest)) {
    return(dest)
  }
  tmp <- tempfile(fileext = ".zip")
  utils::download.file(cagis_data_url, destfile = tmp, mode = "wb")
  utils::unzip(tmp, exdir = dirname(dest))
  return(dest)
}

#' Cincy address geographies
#'
#' CAGIS data (see `install_cagis_data()`) provides a list of all addresses in Hamilton County. 
#' Addresses are filtered for the following criteria: 
#' - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
#' - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#'   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
#' - s2 cell is derived from LONGITUDE and LATITUDE fields in CAGIS address database
#' @returns a simple features object with columns `cagis_address`, `cagis_address_place`, `cagis_address_type`,
#' `cagis_s2`, `cagis_parcel_id`, `cagis_is_condo`, and a geometry column (`s2_geography`)
#' @export
#' @examples
#' cincy_addr_geo()
cincy_addr_geo <- function() {
  install_cagis_data() |>
  sf::st_read(layer = "Addresses") |>
  tibble::as_tibble() |>
  dplyr::filter(STATUS %in% c("ASSIGNED", "USING")) |>
  dplyr::filter(ORPHANFLG == "N") |>
  dplyr::filter(!ADDRTYPE %in% c("MM", "PAR", "PRJ", "CTW", "LOT", "MIS", "RR", "TBA")) |>
  dplyr::transmute(
    cagis_address = FULLMAILADR,
    cagis_address_place = BLDGPLACE,
    cagis_address_type = ADDRTYPE,
    cagis_s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE)),
    cagis_parcel_id = PARCELID,
    cagis_is_condo = CONDOFLG %in% c("Y")
  ) |>
  dplyr::mutate(s2_geography = s2::s2_cell_to_lnglat(cagis_s2)) |>
  sf::st_as_sf()
}

#' Cincy neighborhood geographies
#'
#' CAGIS data (see `install_cagis_data()`) provides community council boundaries, but these boundaries can
#' overlap and do not align with census geographies or ZIP codes.
#' By default, the statistical neighborhood approximations are instead returned,
#' which are calculated by aggregating census tracts into 50 matching neighborhoods.
#' @param geography which type of cincy neighborhood geography to return
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`)
#' @export
#' @examples
#' cincy_neighborhood_geo("statistical_neighborhood_approximations")
#' cincy_neighborhood_geo("community_council")
cincy_neighborhood_geo <- function(geography = c("statistical_neighborhood_approximations", "community_council")) {
  geography <- rlang::arg_match(geography)
  if (geography == "statistical_neighborhood_approximations") {
    noi <- c("Cincinnati_Statistical_Neighborhood_Approximations" = "SNA_NAME")
  }
  if (geography == "community_council") {
    noi <- c("Cincinnati_Community_Council_Neighborhoods" = "NEIGH")
  }
  d <- sf::st_read(install_cagis_data(), names(noi), quiet = TRUE)
  out <- tibble::tibble(
    geoid = sf::st_drop_geometry(d)[, noi],
    s2_geography = sf::st_as_s2(sf::st_cast(sf::st_zm(d$SHAPE), "MULTIPOLYGON"))
  ) |>
    sf::st_as_sf()
  return(out)
}

#' cincy_city_geo()
#' @export
#' @rdname cincy_neighorhood_geo
#' @examples
#' cincy_city_geo()
cincy_city_geo <- function() {
  cagis_db <- install_cagis_data()
  out <- sf::st_read(cagis_db, layer = "Cincinnati_City_Boundary", quiet = TRUE)
  return(sf::st_as_s2(out$SHAPE))
}

#' Cincy ZIP Code Tabulation Areas
#'
#' Read [ZIP Code Tabulation Areas
#' (ZCTAs)](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html)
#' geographies from the online Census
#' [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' files into R
#' @param vintage a character vector of a year corresponding to the vintage of TIGER/Line data
#' @export
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`)
#' @examples
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
  out <- sf::st_as_sf(out)
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
