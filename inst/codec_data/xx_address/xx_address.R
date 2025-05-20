if (
  tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)
) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
library(dplyr)
library(sf)
library(dpkg)
library(geoarrow)
library(addr)
options(arrow.unsafe_metadata = TRUE)

city_tracts <-
  codec::cincy_census_geo("tract", "2013") |>
  st_join(codec::cincy_city_geo() |> st_as_sf(), left = FALSE) |>
  st_drop_geometry() |>
  select(census_tract_id_2010 = geoid)

make_tract_date <- function(d, tract_set) {
  min_year <- min(d$year, na.rm = TRUE)
  min_year_month <- min(d[d$year == min_year, "month"], na.rm = TRUE)
  max_year <- max(d$year, na.rm = TRUE)
  max_year_month <- max(d[d$year == max_year, "month"], na.rm = TRUE)

  tract_set |>
    mutate(
      date = list(seq.Date(
        from = as.Date(glue::glue("{min_year}-{min_year_month}-01")),
        to = as.Date(glue::glue("{max_year}-{max_year_month}-01")),
        by = "month"
      ))
    ) |>
    tidyr::unnest(cols = c(date)) |>
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    select(-date)
}

crime_incidents <-
  dpkg::stow("gh://geomarker-io/xx_address/crime_incidents-v0.1.2") |>
  arrow::read_parquet() |>
  select(date_time, lon_jittered, lat_jittered, category) |>
  filter(!is.na(lon_jittered), !is.na(lat_jittered)) |>
  st_as_sf(coords = c("lon_jittered", "lat_jittered"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010, largest = TRUE) |>
  st_drop_geometry() |>
  mutate(
    year = lubridate::year(date_time),
    month = lubridate::month(date_time)
  ) |>
  select(-date_time) |>
  group_by(census_tract_id_2010, year, month, category) |>
  tally() |>
  tidyr::pivot_wider(
    names_from = category,
    values_from = n
  )

crime_incidents <-
  left_join(
    make_tract_date(crime_incidents, city_tracts),
    crime_incidents,
    by = c("census_tract_id_2010", "year", "month")
  ) |>
  mutate(across(c(property, violent, other), \(x) ifelse(is.na(x), 0, x)))

shotspotter <-
  dpkg::stow("gh://geomarker-io/xx_address/shotspotter-v0.1.2") |>
  arrow::read_parquet() |>
  select(date_time, lon_jittered, lat_jittered) |>
  filter(!is.na(lon_jittered), !is.na(lat_jittered)) |>
  st_as_sf(coords = c("lon_jittered", "lat_jittered"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010, largest = TRUE) |>
  st_drop_geometry() |>
  mutate(
    year = lubridate::year(date_time),
    month = lubridate::month(date_time)
  ) |>
  select(-date_time) |>
  group_by(census_tract_id_2010, year, month) |>
  tally() |>
  rename(gunshots = n)

shotspotter_tracts <-
  shotspotter |>
  group_by(census_tract_id_2010) |>
  tally() |>
  arrange(n) |>
  filter(n > 1) |>
  st_drop_geometry() |>
  select(-n)

shotspotter <-
  left_join(
    make_tract_date(shotspotter, shotspotter_tracts),
    shotspotter,
    by = c("census_tract_id_2010", "year", "month")
  ) |>
  mutate(gunshots = ifelse(is.na(gunshots), 0, gunshots))

reported_shootings <-
  dpkg::stow("gh://geomarker-io/xx_address/reported_shootings-v0.1.0") |>
  arrow::read_parquet() |>
  select(date, lon_jittered, lat_jittered) |>
  filter(!is.na(lon_jittered), !is.na(lat_jittered)) |>
  st_as_sf(coords = c("lon_jittered", "lat_jittered"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010, largest = TRUE) |>
  st_drop_geometry() |>
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) |>
  select(-date) |>
  group_by(census_tract_id_2010, year, month) |>
  tally() |>
  rename(reported_shootings = n)

reported_shootings <-
  left_join(
    make_tract_date(reported_shootings, city_tracts),
    reported_shootings,
    by = c("census_tract_id_2010", "year", "month")
  ) |>
  mutate(
    reported_shootings = ifelse(
      is.na(reported_shootings),
      0,
      reported_shootings
    )
  )

all_tracts <-
  codec::cincy_census_geo("tract", "2013") |>
  st_drop_geometry() |>
  rename(census_tract_id_2010 = geoid)

all_tracts <-
  make_tract_date(
    d = bind_rows(crime_incidents, shotspotter, reported_shootings),
    all_tracts
  )

d_out <-
  purrr::reduce(
    .x = list(all_tracts, crime_incidents, shotspotter, reported_shootings),
    .f = \(x, y)
      left_join(x, y, by = c("census_tract_id_2010", "year", "month"))
  ) |>
  filter(!is.na(census_tract_id_2010))

out_dpkg <-
  d_out |>
  as_codec_dpkg(
    name = "xx_address",
    version = "0.2.1",
    title = "Crime",
    homepage = "https://geomarker.io/codec",
    description = paste(
      readLines(fs::path_package(
        "codec",
        "codec_data",
        "xx_address",
        "README.md"
      )),
      collapse = "\n"
    )
  )

dpkg_gh_release(out_dpkg, draft = FALSE)
