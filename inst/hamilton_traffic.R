devtools::load_all()
name <- "hamilton_traffic"
version <- "v0.1.0"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

md <- as.list(rd)

d <-
  tibble::as_tibble(rd) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = as.integer(2017))

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
      title = "Hamilton County Parcel and Household Traffic",
      description = "Number and fraction of parcels and households near traffic by census tract",
      homepage = "https://geomarker.io/hamilton_traffic"
    )
  )

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
