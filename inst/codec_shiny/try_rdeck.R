devtools::load_all()

## pak::pak("qfes/rdeck@v0.5.2")
library(rdeck)
library(sf)

tracts_sf <- cincy::tract_tigris_2010

dpkgs <-
  list(
    environmental_justice_index =
      get_codec_dpkg("environmental_justice_index-v0.1.0"),
    hh_acs_measures =
      get_codec_dpkg("hh_acs_measures-v1.1.1") |>
        dplyr::filter(year == 2019),
    drivetime =
      get_codec_dpkg("drivetime-v0.2.2"),
    landcover =
      get_codec_dpkg("landcover-v0.1.0"),
    traffic =
      get_codec_dpkg("traffic-v0.1.2") |>
        dplyr::left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
        st_as_sf() |>
        interpolate(cincy::tract_tigris_2010) |>
        st_drop_geometry() |>
        tibble::as_tibble()
  )

d <-
  purrr::reduce(dpkgs, dplyr::left_join, by = "census_tract_id_2010", .init = tracts_sf) |>
  tibble::as_tibble() |>
  st_as_sf() |>
  st_transform(st_crs(4326))

add_codec_map_layer <- function(rdeck, col) {
  rdeck |>
    add_polygon_layer(
      id = deparse(substitute(col)),
      name = deparse(substitute(col)),
      data = d,
      get_polygon = geometry,
      get_fill_color = scale_color_linear({{ col }}),
      ## get_fill_color = scale_color_quantile({{ col }}, probs = seq(0, 1, 0.125)),
      opacity = 0.3,
      get_line_color = "#505050",
      get_line_width = 10,
      visible = FALSE,
      ## get_elevation = n_children_lt18
      pickable = TRUE,
      auto_highlight = TRUE,
      tooltip = c(census_tract_id_2010, {{ col }})
    )
}

rdeck(map_style = mapbox_light(), initial_bounds = st_bbox(d)) |>
  add_polygon_layer(
    id = "census_tract", name = "census_tract", data = d,
    get_polygon = geometry, get_fill_color = "#ffffff00",
    get_line_color = "#505050", get_line_width = 10,
    visible = TRUE, pickable = FALSE
  ) |>
  add_codec_map_layer(median_rent_to_income_percentage) |>
  add_codec_map_layer(prcnt_area_within_1mi_high_volume_road)
