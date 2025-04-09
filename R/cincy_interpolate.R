#' Coerce CoDEC data package into a simple features object
#'
#' The name of the census tract column in the CoDEC data package is used to add
#' the appropriate cincy geography.
#' @param x a CoDEC data package
#' @details Tract identifers do not change across decennial censuses, but the digital representation of their boundaries
#' may be improved over time.  Here, data packages using 2010 tract identifers use the TIGER/Line 2019 tract shapefiles
#' and data packages using 2020 tract identifiers use the TIGER/Line 2020 tract shapefiles
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`) in addition to the columns in `x`
#' @export
#' @examples
#' codec_as_sf(get_codec_dpkg("property_code_enforcements-v0.2.0"))
codec_as_sf <- function(x) {
  if (!is_codec_dpkg(x)) rlang::abort("x must be a CoDEC data package")
  codec_tract_id_name <- get_codec_tract_id_name(x)
  tiger_vintage <- ifelse(codec_tract_id_name == "census_tract_id_2020", "2020", "2019")
  gd <-
    cincy_census_geo("tract", tiger_vintage) |>
    dplyr::left_join(x, by = c("geoid" = codec_tract_id_name))
  return(gd)
}

get_codec_tract_id_name <- function(x) {
  if (!is_codec_dpkg(x)) rlang::abort("x must be a CoDEC data package")
  ifelse(any(grepl("census_tract_id_2010", names(x), fixed = TRUE)),
    "census_tract_id_2010", "census_tract_id_2020"
  )
}

#' Spatially interpolate community-level data
#'
#' Census block-level weights are used to spatially interpolate CoDEC data packages from the census tract-level
#' to other Cincy geographies.
#' @param from a CoDEC data package
#' @param to A simple features object returned by one of the `cincy_*_geo()` functions
#' (i.e., `cincy_census_geo()`, `cincy_neighborhood_geo()`, or cincy_zcta_geo()`)
#' @param weights which census block-level weights to use; see details
#' @returns a tibble with a new geographic identifier column for the `to` target geography (`geoid`)
#' in addition to the (interpolated) columns in `from`
#' @details
#' Block-level total population (`pop`), total number of homes (`homes`), or total land area (`area`)
#' from the 2020 Census can be chosen to use for the weights.
#' Geospatial intersection happens after transforming geographies to epsg:5072.
#' See `codec_as_sf()` for adding geography to a CoDEC data package.
#' Variables beginning with "n_" are interpolated using a weighted sum;
#' all other variables are interpolated using a weighted mean.
#' @export
#' @examples
#' codec_interpolate(get_codec_dpkg("acs_measures-v0.1.0"),
#'                   cincy_neighborhood_geo())
#' codec_interpolate(get_codec_dpkg("property_code_enforcements-v0.2.0"),
#'                   cincy_census_geo("tract", "2020"))
codec_interpolate <- function(from, to, weights = c("pop", "homes", "area")) {
  if (!is_codec_dpkg(from)) rlang::abort("x must be a CoDEC data package")
  codec_tract_id_name <- get_codec_tract_id_name(from)
  weights <- rlang::arg_match(weights)
  from_sf <-
    from |>
    codec_as_sf() |>
    dplyr::slice_sample(n = 1, by = "geoid") |>
    dplyr::select(geoid) |>
    sf::st_transform(5072)
  if (!grepl("^cincy_(census|neighborhood|zcta)_geo\\(", deparse(substitute(to)))) {
    rlang::abort("`to` must be supplied via `cincy_census_geo()`, `cincy_neighborhood_geo()`, or `cincy_zcta_geo()`")
  }
  to_sf <- sf::st_transform(to, 5072)

  bw <-
    cincy_block_weights() |>
    sf::st_transform(5072) |>
    dplyr::select(the_weight = {{ weights }}, s2_geography)

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
    dplyr::rename("geoid.1" := {{ codec_tract_id_name }}) |>
    dplyr::left_join(interpolation_weights, by = "geoid.1") |>
    dplyr::group_by(geoid, year) |>
    ## TODO
    ## dplyr::group_by(geoid, year, month) |>
    dplyr::summarize(
      dplyr::across(
        c(-tidyselect::starts_with("n_"), -tidyselect::starts_with("geoid.1"), -weight),
        \(x) stats::weighted.mean(x, weight, na.rm = TRUE)
      ),
      dplyr::across(tidyselect::starts_with("n_"), \(x) sum(x * weight, na.rm = TRUE))
    ) |>
    dplyr::ungroup()

  return(out)
}

cincy_block_weights <- function() {
  tiger_local <- tiger_download("ftp://ftp2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_39_tabblock20.zip")
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
  "year", "month", "weight",
  ":="
))
