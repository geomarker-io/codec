#' Spatially interpolate community-level data
#'
#' Weights at the census block-level are used to spatially interpolate different geographies.
#' Block-level total population, total number of homes, or total land area from the 2020 Census
#' can be chosen to use for the weights.
#' @param from a CoDEC data package
#' @param to ; if NULL, tract geographies are returned without data interpolation
#' @details Tract identifers do not change across decennial censuses, but the digital representation of their boundaries
#' may be improved over time.  Here, data packages using 2010 tract identifers use the TIGER/Line 2019 tract shapefiles
#' and data packages using 2020 tract identifiers use the TIGER/Line 2020 tract shapefiles
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`) in addition to the (interpolated) columns in `from`
#' @examples
#' codec_interpolate(from = get_codec_dpkg("property_code_enforcements-v0.2.0"))
#' codec_interpolate(get_codec_dpkg("property_code_enforcements-v0.2.0"), to = "zcta")
codec_interpolate <- function(from, to = NULL, weights = c("pop", "homes", "area")) {
  weights <- rlang::arg_match(weights)
  if (!is_codec_dpkg(from)) rlang::abort("from must be a CoDEC data package")
  md <- dpkg::dpkg_meta(from)
  gd_vintage <- ifelse(any(grepl("census_tract_id_2010", names(from), fixed = TRUE)), "2019", "2020")
  gd <-
    cincy_census_geo("tract", gd_vintage) |>
    dplyr::left_join(from, by = c("geoid" = paste0("census_tract_id_", ifelse(gd_vintage == "2019", "2010", "2020"))))
  if (is.null(to)) {
    return(gd)
  }
  return("interpolation is under construction.....")
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

from <-
  cincy_census_geo("tract", "2020") |>
  sf::st_transform(5072) |>
  dplyr::mutate(n = 0.1)
to <- sf::st_transform(cincy_zcta_geo("2020"), 5072)

bw <-
  cincy_block_weights() |>
  sf::st_transform(5072) |>
  dplyr::select(weight = pop, s2_geography)
  
# from 'total weights'
fromm <-
  sf::st_join(from, bw, left = FALSE) |>
  sf::st_drop_geometry() |>
  dplyr::summarize(total_weight = sum(weight, na.rm = TRUE), .by = "geoid") |>
  dplyr::right_join(from, by = "geoid") |>
  sf::st_as_sf()

# calculate intersections and intersection proportions
intersections <-
  fromm |>
  sf::st_intersection(to) |>
  dplyr::filter(sf::st_is(s2_geography, c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) |>
  dplyr::mutate(intersection_id = dplyr::row_number())

weights <-
  sf::st_join(bw, intersections) |>
  sf::st_drop_geometry() |>
  dplyr::summarize(
    weight = sum(weight, na.rm = TRUE),
    weight_coef = weight / total_weight,
    .by = "intersection_id"
  ) |>
  dplyr::distinct(weight_coef, .keep_all = TRUE) |>
  dplyr::right_join(intersections, by = "intersection_id") |>
  dplyr::select(geoid, geoid.1, weight, weight_coef)

# non-extensive (same total)
fromm |>
  sf::st_drop_geometry() |>
  dplyr::left_join(weights, by = "geoid") |>
  dplyr::summarize(n_interpolated = sum(n * weight_coef, na.rm = TRUE), .by = "geoid.1")

# extensive (same average)
fromm |>
  sf::st_drop_geometry() |>
  dplyr::left_join(weights, by = "geoid") |>
  dplyr::summarize(n_interpolated = weighted.mean(n, weight, na.rm = TRUE), .by = "geoid.1")

