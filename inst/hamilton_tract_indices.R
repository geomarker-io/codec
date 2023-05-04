library(codec)
name <- "tract_indices"
version <- "v0.3.0"

d <-
  read_tdr_csv(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  )) |>
  dplyr::filter(substr(census_tract_id, 1, 5) == "39061") |>
  dplyr::select(-low_food_access_pop) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = as.integer(2019)) |>
  add_col_attrs(year, name = "year", title = "Year", description = "data year") |>
  add_type_attrs() |>
  add_attrs(description = "A collection of census-derived indices for census tracts in Hamilton County")

write_tdr_csv(d, "codec_data")
check_codec_tdr_csv(fs::path("codec_data", name))
