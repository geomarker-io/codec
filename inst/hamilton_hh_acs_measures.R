devtools::load_all()
name <- "hh_acs_measures"
version <- "v1.1.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

md <- as.list(rd)

d <-
  rd |>
  tibble::as_tibble() |>
  dplyr::filter(substr(census_tract_id, 1, 5) == "39061") |>
  dplyr::select(-census_tract_vintage) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id)

# create tdr by adding back in metadata for any
# columns present in the original metadata
d_tdr <-
  purrr::reduce2(
    names(d),
    md$schema$fields[names(d)],
    \(accum, xx, yy) fr::update_field(x = accum, field = xx, !!!yy),
    .init = fr::as_fr_tdr(
      d,
      name = name,
      version = version,
      title = "Harmonized Historical American Community Survey Measures",
      description = "2010 - 2021 measures derived from ACS variables for census tracts in the contiguous US",
      homepage = "https://geomarker.io/hh_acs_measures"
    )
  )

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
