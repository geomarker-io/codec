library(codec)
name <- "hh_acs_measures"
version <- "v1.0.0"

d <-
  read_tdr_csv(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}"
  )) |>
  dplyr::filter(substr(census_tract_id, 1, 5) == "39061") |>
  dplyr::select(-census_tract_vintage) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  add_col_attrs(census_tract_id_2010,
    name = "census_tract_id_2010",
    title = "Census Tract Identifier"
  ) |>
  add_attrs(description = "2010 - 2020 measures derived from ACS variables for census tracts in Hamilton County")

write_tdr_csv(d, "codec_data")
check_codec_tdr_csv(fs::path("codec_data", name))
