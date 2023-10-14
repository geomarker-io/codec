devtools::load_all()
name <- "hamilton_crime_risk"
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
  dplyr::mutate(year = as.integer(2020))

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
    title = "Hamilton County Crime Risk",
    description = "Average [crime risk](https://github.com/geomarker-io/hamilton_crime_risk/blob/master/AGS-CrimeRisk-Methodology-2022A.pdf) (2014-2020) at 2020 census blocks interpolated (population weighted) to 2010 census tracts.",
      homepage = "https://geomarker.io/hamilton_crime_risk"
    )
  ) |>
  fr::update_field("year", description = "Represents 2014 - 2020 average")

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
