devtools::load_all()
name <- "hh_acs_measures"
version <- "v1.1.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr::fr_filter(substr(census_tract_id, 1, 5) == "39061") |>
  fr::fr_select(-census_tract_vintage) |>
  fr::fr_rename(census_tract_id_2010 = census_tract_id)

d_tdr@version <- version

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
