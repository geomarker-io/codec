if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
library(dplyr)
library(sf)
library(dpkg)
library(geoarrow)
options(arrow.unsafe_metadata = TRUE)

crime_incidents <-
  dpkg::stow("https://github.com/geomarker-io/xx_address/releases/download/crime_incidents-v0.1.0/crime_incidents-v0.1.0.parquet") |>
  arrow::read_parquet() |>
  mutate(geometry = sf::st_as_sfc(geometry)) |>
  st_as_sf(crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010) |>
  st_drop_geometry() |>
  select(-tlid) |>
  group_by(census_tract_id_2010) |>
  summarize(across(c(property, violent, other), \(x) sum(x, na.rm = TRUE)))

shotspotter <-
  dpkg::stow("https://github.com/geomarker-io/xx_address/releases/download/shotspotter-v0.1.0/shotspotter-v0.1.0.parquet") |>
  arrow::read_parquet() |>
  mutate(geometry = sf::st_as_sfc(geometry)) |>
  st_as_sf(crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010) |>
  st_drop_geometry() |>
  select(-tlid) |>
  group_by(census_tract_id_2010) |>
  summarize(gunshots = sum(gunshots, na.rm = TRUE))

d_out <-
  cincy::tract_tigris_2010 |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(crime_incidents, by = "census_tract_id_2010") |>
  left_join(shotspotter, by = "census_tract_id_2010") |>
  mutate(across(c(property, violent, other, gunshots), \(x) ifelse(is.na(x), 0, x))) |>
  filter(!is.na(census_tract_id_2010)) |>
  mutate(year = "2024")

mapview::mapview(left_join(cincy::tract_tigris_2010, d_out), zcol = "property")

out_dpkg <-
  d_out |>
  as_codec_dpkg(
    name = "xx_address",
    version = "0.1.0",
    title = "Crime",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "xx_address", "README.md")), collapse = "\n")
  )

dpkg_gh_release(out_dpkg, draft = FALSE)
