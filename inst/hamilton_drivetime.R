devtools::load_all()
library(dplyr)
library(sf)

name <- "hamilton_drivetime"
version <- "0.2.0"
description <- "A census tract-level measure of drive time to Cincinnati Children's Hospital Medical Center is derived using 6-minute interval drive time isochrones obtained from [openroute service](https://classic-maps.openrouteservice.org/reach?n1=38.393339&n2=-95.339355&n3=5&b=0&i=0&j1=30&j2=15&k1=en-US&k2=km). Each tract-level drive time is an area-weighted average of drive times."

isochrones <-
  s3::s3_get("s3://geomarker/drivetime/isochrones/cchmc_isochrones.rds") |>
  readRDS()

d <-
  st_intersection(cincy::tract_tigris_2010, isochrones) |>
  mutate(
    area = round(as.numeric(st_area(geometry))),
    drive_time = as.numeric(as.character(drive_time))) |>
  group_by(census_tract_id_2010) |>
  mutate(wt_drive_time = drive_time * area/sum(area)) |>
  summarize(drive_time_avg = round(sum(wt_drive_time),1)) |>
  st_drop_geometry()

d$year <- as.integer(2022)

d <- fr::as_fr_tdr(d, name = name, version = version, description = description)

d@schema@fields$census_tract_id_2010@title <- "Census tract identifer"
d@schema@fields$drive_time_avg@title <- "Average drivetime to CCHMC"
d@schema@fields$year@description <- "Isochrones were obtained in 2022, but these rarely change over time (absent changes to major roadways)"

fr::write_fr_tdr(d, fs::path_package("codec", "codec_data"))

check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
