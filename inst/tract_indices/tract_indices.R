devtools::load_all()
name <- "tract_indices"
version <- "v0.3.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr::fr_filter(substr(census_tract_id, 1, 5) == "39061") |>
  fr::fr_select(-low_food_access_pop) |>
  fr::fr_rename(census_tract_id_2010 = census_tract_id) |>
  fr::fr_mutate(year = 2019)

d_tdr@version <- version

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
