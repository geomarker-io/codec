library(CoDEC)
name <- "hamilton_property_code_enforcement"
version <- "0.1.2"

d <-
  read_tdr_csv(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  )) |>
  dplyr::mutate(year = as.integer(2022)) |>
  add_col_attrs(year, name = "year", title = "Year", description = "data year") |>
  add_type_attrs()

write_tdr_csv(d, "codec_data")
check_codec_tdr_csv(fs::path("codec_data", name))
