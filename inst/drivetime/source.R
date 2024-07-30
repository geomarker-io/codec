isochrones <-
  s3::s3_get("s3://geomarker/drivetime/isochrones/cchmc_isochrones.rds") |>
  readRDS()

d <-
  sf::st_intersection(cincy::tract_tigris_2010, isochrones) |>
  dplyr::mutate(
    area = round(as.numeric(sf::st_area(geometry))),
    drive_time = as.numeric(as.character(drive_time))
  ) |>
  dplyr::group_by(census_tract_id_2010) |>
  dplyr::mutate(wt_drive_time = drive_time * area / sum(area)) |>
  dplyr::summarize(drive_time_avg = round(sum(wt_drive_time), 1)) |>
  sf::st_drop_geometry()

as_codec_dpkg(d, name = "drivetime", version = "0.2.0") |>
  dpkg_write(
    name = "drivetime",
    version = "0.2.0",
    dir = tempdir(),
    readme_file = fs::path("inst", "drivetime", "README", ext = "md")
  ) |>
  dpkg_s3_put()
