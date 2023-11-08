devtools::load_all()
name <- "hamilton_property_code_enforcement"
version <- "0.1.3"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr::fr_mutate(year = 2022) |>
  fr::update_field("year", title = "Year", description = "data year")
d_tdr@version <- version

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
