devtools::load_all()
codec_name <- "drivetime"

library(dplyr, warn.conflicts = FALSE)
library(sf)

isochrones <-
  readRDS(url(
    "https://github.com/degauss-org/drivetime/releases/download/1.3.0/cchmc_isochrones.rds"
  ))

out <-
  sf::st_intersection(
    sf::st_transform(cincy_census_geo("tract", "2020"), sf::st_crs(isochrones)),
    isochrones
  ) |>
  dplyr::mutate(
    area = round(as.numeric(sf::st_area(s2_geography))),
    drive_time = as.numeric(as.character(drive_time))
  ) |>
  dplyr::group_by(geoid) |>
  dplyr::mutate(wt_drive_time = drive_time * area / sum(area)) |>
  dplyr::summarize(drive_time_avg = round(sum(wt_drive_time), 1)) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(year = 2024L)

out |>
  rename(census_tract_id_2020 = geoid) |>
  as_codec_tbl(
    name = codec_name,
    description = paste(
      readLines(fs::path_package(
        "codec",
        "data-raw",
        "codec_tbl",
        codec_name,
        "README.md"
      )),
      collapse = "\n"
    )
  ) |>
  write_codec_pin()
