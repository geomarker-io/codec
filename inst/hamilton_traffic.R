devtools::load_all()
name <- "hamilton_traffic"
version <- "v0.1.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr::fr_rename(census_tract_id_2010 = census_tract_id) |>
  fr::fr_mutate(year = as.integer(2017))
d_tdr@version <- version

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
