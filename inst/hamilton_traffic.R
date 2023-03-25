library(CODECtools)
name <- "hamilton_traffic"
version <- "v0.1.0"

d <-
  read_tdr_csv(glue::glue(
  "https://github.com/geomarker-io/",
  "{name}/releases/download/{version}/"
)) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = as.integer(2017)) |>
  add_col_attrs(year, name = "year", title = "Year", description = "data year") |>
  add_type_attrs()

write_tdr_csv(d, "codec_data")
check_codec_tdr_csv(fs::path("codec_data", name))
