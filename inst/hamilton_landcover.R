devtools::load_all()
name <- "hamilton_landcover"
version <- "v0.1.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr::fr_rename(census_tract_id_2010 = census_tract_id) |>
  fr::fr_mutate(year = as.integer(2019)) |>
  fr::update_field("year",
    title = "Year",
    description = "The actual year is unique to each data product and denoted in the field names"
  )
d_tdr@version <- version

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
