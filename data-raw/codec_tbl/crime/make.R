devtools::load_all()
codec_name <- "crime"

library(addr)
library(dplyr, warn.conflicts = FALSE)

pin_read_dpkg <- function(dpkg_parquet_url) {
  stopifnot(
    length(dpkg_parquet_url) == 1,
    inherits(dpkg_parquet_url, "character")
  )
  dpkg_local <-
    pins::board_url(c(x = dpkg_parquet_url)) |>
    pins::pin_download("x")
  arrow::read_parquet(dpkg_local)
}

d_shotspotter <-
  pin_read_dpkg(
    "https://github.com/geomarker-io/xx_address/releases/download/shotspotter-v0.1.2/shotspotter-v0.1.2.parquet"
  ) |>
  mutate(
    s2_cell = s2::as_s2_cell(s2::s2_lnglat(lon_jittered, lat_jittered)),
    year = as.integer(format(date_time, "%Y")),
    month = as.integer(format(date_time, "%m")),
    census_tract_id_2020 = substr(
      addr::s2_join_tiger_bg(s2_cell, "2020"),
      1,
      11
    ),
    .keep = "none"
  ) |>
  filter(year > 2016) |>
  summarize(n_shots_fired = n(), .by = c(year, month, census_tract_id_2020))

d_reported_shootings <-
  pin_read_dpkg(
    "https://github.com/geomarker-io/xx_address/releases/download/reported_shootings-v0.1.0/reported_shootings-v0.1.0.parquet"
  ) |>
  mutate(
    s2_cell = s2::as_s2_cell(s2::s2_lnglat(lon_jittered, lat_jittered)),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    census_tract_id_2020 = substr(
      addr::s2_join_tiger_bg(s2_cell, "2020"),
      1,
      11
    ),
    .keep = "none"
  ) |>
  filter(year > 2016) |>
  summarize(
    n_reported_shootings = n(),
    .by = c(year, month, census_tract_id_2020)
  )

d_crime_incidents <-
  pin_read_dpkg(
    "https://github.com/geomarker-io/xx_address/releases/download/crime_incidents-v0.1.2/crime_incidents-v0.1.2.parquet"
  ) |>
  mutate(
    s2_cell = s2::as_s2_cell(s2::s2_lnglat(lon_jittered, lat_jittered)),
    year = as.integer(format(date_time, "%Y")),
    month = as.integer(format(date_time, "%m")),
    category = category,
    census_tract_id_2020 = substr(
      addr::s2_join_tiger_bg(s2_cell, "2020"),
      1,
      11
    ),
    .keep = "none"
  ) |>
  filter(year > 2016) |>
  summarize(
    n_crime_incidents = n(),
    .by = c(year, month, census_tract_id_2020, category)
  ) |>
  tidyr::pivot_wider(
    names_from = category,
    names_prefix = "n_crime_incidents_",
    values_from = n_crime_incidents
  )

out_st <- expand.grid(
  census_tract_id_2020 = cincy_census_geo("tract", "2020")$geoid,
  year = 2017:2024,
  month = 1:12
)

out <-
  list(out_st, d_shotspotter, d_reported_shootings, d_crime_incidents) |>
  purrr::reduce(left_join, by = c("census_tract_id_2020", "year", "month"))

out |>
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
