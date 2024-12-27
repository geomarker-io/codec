#' Coerce CoDEC data package into a simple features object
#'
#' This functions uses the name of the census tract column in
#' the CoDEC data package to add the appropriate `cincy_census_geo()`
#' and convert it into a sf object.
#' @param x a CoDEC data package
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`) in addition to the existing columns in x
#' @details Tract identifers do not change across decennial censuses, but the digital representation of their boundaries
#' may be improved over time.  Here, data packages using 2010 tract identifers use the TIGER/Line 2019 tract shapefiles
#' and data packages using 2020 tract identifiers use the TIGER/Line 2020 tract shapefiles
#' @export
#' @examples
#' codec_as_sf(get_codec_dpkg("property_code_enforcements-v0.2.0"))
codec_as_sf <- function(x) {
  if (!is_codec_dpkg(x)) rlang::abort("x must be a CoDEC data package")
  codec_tract_id_name <-
    ifelse(
      any(grepl("census_tract_id_2010", names(x), fixed = TRUE)),
      "census_tract_id_2010", "census_tract_id_2020"
    )
  tiger_vintage <- ifelse(codec_tract_id_name == "census_tract_id_2020", "2020", "2019")
  gd <-
    cincy_census_geo("tract", tiger_vintage) |>
    dplyr::left_join(x, by = c("geoid" = codec_tract_id_name))
  return(gd)
}

#' Spatially interpolate community-level data
#'
#' Census block-level weights are used to spatially interpolate different geographies.
#' @param from a CoDEC data package
#' @param to name of target geography
#' @param weights which census block-level weights to use; see details
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`) in addition to the (interpolated) columns in `from`
#' @details
#' Block-level total population (`pop`), total number of homes (`homes`), or total land area (`area`)
#' from the 2020 Census can be chosen to use for the weights.
#' Geospatial intersection happens after transforming geographies to epsg:5072.
#' See `codec_as_sf()` for adding geography to a CoDEC data package.
#' Variables beginning with "n_" are interpolated using a weighted sum;
#' all other variables are interpolated using a weighted mean.
#' @export
#' @examples
#' codec_interpolate(from = get_codec_dpkg("acs_measures-v0.1.0"))
#' # TODO codec_interpolate(from = get_codec_dpkg("property_code_enforcements-v0.2.0"))
codec_interpolate <- function(from, to = "zcta", weights = c("pop", "homes", "area")) {
  weights <- rlang::arg_match(weights)
  from_sf <-
    from |>
    codec_as_sf() |>
    dplyr::slice_sample(n = 1, by = "geoid") |>
    dplyr::select(geoid) |>
    sf::st_transform(5072)

  if (to == "zcta") {
    to_sf <-
      cincy_zcta_geo("2020") |>
      sf::st_transform(5072)
  }

  bw <-
    cincy_block_weights() |>
    sf::st_transform(5072) |>
    dplyr::select(the_weight = pop, s2_geography)
  # TODO add back in choice for weights

  interpolation_weights <-
    sf::st_intersection(dplyr::select(to_sf, geoid), dplyr::select(from_sf, geoid)) |>
    dplyr::filter(sf::st_is(s2_geography, c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) |>
    sf::st_join(bw) |>
    sf::st_drop_geometry() |>
    dplyr::arrange(geoid) |>
    stats::na.omit() |>
    dplyr::filter(the_weight > 0) |>
    dplyr::mutate(weight_coef = the_weight / sum(the_weight), .by = c("geoid")) |>
    dplyr::summarize(weight = sum(weight_coef), .by = c("geoid", "geoid.1")) |>
    suppressWarnings()

  out <-
    from |>
    dplyr::left_join(interpolation_weights, by = c("census_tract_id_2020" = "geoid.1")) |>
    dplyr::group_by(geoid, year) |>
    ## TODO
    ## dplyr::left_join(interpolation_weights, by = c("census_tract_id_2010" = "geoid.1")) |>
    ## dplyr::group_by(geoid, year, month) |>
    dplyr::summarize(
      dplyr::across(
        c(-tidyselect::starts_with("n_"), -tidyselect::starts_with("census_tract_id_"), -weight),
        \(x) stats::weighted.mean(x, weight, na.rm = TRUE)
      ),
      dplyr::across(tidyselect::starts_with("n_"), \(x) sum(x * weight, na.rm = TRUE))
    ) |>
    dplyr::ungroup()

  return(out)
}

cincy_block_weights <- function() {
  tiger_url <- "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_39_tabblock20.zip"
  tiger_local <- dpkg::stow_url(tiger_url)
  rd <-
    sf::read_sf(glue::glue("/vsizip/", tiger_local),
      query = glue::glue("SELECT GEOID20,ALAND20,HOUSING20,POP20 FROM tl_2020_39_tabblock20 WHERE COUNTYFP20 = '061'")
    )
  out <-
    rd |>
    sf::st_transform(5072) |>
    sf::st_point_on_surface() |>
    suppressWarnings() |>
    dplyr::select(pop = POP20, homes = HOUSING20, area = ALAND20)
  out$s2_geography <- sf::st_as_s2(out$geometry)
  out <- sf::st_drop_geometry(out)
  out <- sf::st_as_sf(out)
  return(out)
}

utils::globalVariables(c(
  "POP20", "HOUSING20", "ALAND20",
  "geoid", "pop", "s2_geography",
  "the_weight", "weight_coef",
  "year", "month", "weight"
))
