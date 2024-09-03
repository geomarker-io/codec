if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

isochrones <-
  s3::s3_get("s3://geomarker/drivetime/isochrones/cchmc_isochrones.rds") |>
  readRDS()

out <-
  sf::st_intersection(cincy::tract_tigris_2010, isochrones) |>
  dplyr::mutate(
    area = round(as.numeric(sf::st_area(geometry))),
    drive_time = as.numeric(as.character(drive_time))
  ) |>
  dplyr::group_by(census_tract_id_2010) |>
  dplyr::mutate(wt_drive_time = drive_time * area / sum(area)) |>
  dplyr::summarize(drive_time_avg = round(sum(wt_drive_time), 1)) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(year = 2024)

out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "drivetime",
    version = "0.2.2",
    title = "Average Drive Time to Cincinnati Children's",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "drivetime", "README.md")), collapse = "\n")
  )

dpkg::use_dpkg_badge(out_dpkg)
dpkg::dpkg_gh_release(out_dpkg)
