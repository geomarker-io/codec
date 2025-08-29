devtools::load_all()
codec_name <- "property_code_enforcements"

library(dplyr, warn.conflicts = FALSE)
library(s2)
library(addr)

d_pce <-
  pins::board_url(c(
    x = "https://github.com/geomarker-io/parcel/releases/download/property_code_enforcements-v1.1.1/property_code_enforcements-v1.1.1.parquet"
  )) |>
  pins::pin_download("x") |>
  arrow::read_parquet()

pce_tract_year <-
  d_pce |>
  mutate(
    s2_cell = as_s2_cell(s2_lnglat(lon_jittered, lat_jittered)),
    date = as.Date(date),
    census_bg_id_2020 = addr::s2_join_tiger_bg(s2_cell, "2020"),
    .keep = "none"
  ) |>
  mutate(
    year = as.integer(format(date, "%Y")),
    census_tract_id_2020 = substr(census_bg_id_2020, 1, 11)
  ) |>
  filter(year > 2016) |>
  summarize(
    n_property_code_enforcements = n(),
    .by = c(year, census_tract_id_2020)
  )

out <- expand.grid(
  census_tract_id_2020 = cincy_census_geo("tract", "2020")$geoid,
  year = 2017:2025
) |>
  left_join(pce_tract_year, by = c("census_tract_id_2020", "year"))


out[
  is.na(out$n_property_code_enforcements),
  "n_property_code_enforcements"
] <- 0

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
