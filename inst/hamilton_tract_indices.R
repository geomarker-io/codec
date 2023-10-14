devtools::load_all()
name <- "tract_indices"
version <- "v0.3.0"

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
  dplyr::select(-low_food_access_pop) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = 2019)

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
      title = "Census Tract-Level Neighborhood Indices",
      description = "A collection of census-derived indices for census tracts in Hamilton County",
      homepage = "https://geomarker.io/tract_indices"
    )
  )

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
