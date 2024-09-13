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
  dpkg::stow("gh://geomarker-io/xx_address/crime_incidents-v0.1.1") |>
  arrow::read_parquet() |>
  mutate(geometry = sf::st_as_sfc(geometry)) |>
  st_as_sf(crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010, largest = TRUE) |>
  st_drop_geometry() |>
  mutate(
    year = lubridate::year(date_time), 
    month = lubridate::month(date_time)
      ) |>
  select(-tlid, -address_x, -date_time) |>
  group_by(census_tract_id_2010, year, month, category) |>
  tally()|>
  tidyr::pivot_wider(
    names_from = category, 
    values_from = n
  )

shotspotter <-
  dpkg::stow("gh://geomarker-io/xx_address/shotspotter-v0.1.1") |>
  arrow::read_parquet() |>
  mutate(geometry = sf::st_as_sfc(geometry)) |>
  st_as_sf(crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010, largest = TRUE) |>
  st_drop_geometry() |>
  mutate(
    year = lubridate::year(date_time), 
    month = lubridate::month(date_time)
    ) |>
  select(-tlid, -address_x, -date_time) |>
  group_by(census_tract_id_2010, year, month) |>
  tally() |>
  rename(gunshots = n)

all_tracts <- 
  cincy::tract_tigris_2010 |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(date = list(seq.Date(
    from = as.Date("2011-01-01"), 
    to = as.Date("2024-06-01"), 
    by = "month"
  ))) |>
  tidyr::unnest(cols = c(date)) |>
  mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date)
  ) |>
  select(-date)

d_out <-
  left_join(all_tracts, crime_incidents, by = c("census_tract_id_2010", "year", "month")) |>
  left_join(shotspotter, by = c("census_tract_id_2010", "year", "month")) |>
  mutate(
    across(c(property, violent, other, gunshots), 
    \(x) ifelse(is.na(x), 0, x))) |>
  filter(!is.na(census_tract_id_2010))

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
