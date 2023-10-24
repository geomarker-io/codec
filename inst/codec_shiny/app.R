library(codec)
library(dplyr)
library(fr)

codec_data_installed <-
  fs::path_package("codec") |>
  fs::path("codec_data") |>
  fs::dir_ls() |>
  fs::path_file()

codec_data_geography <- cincy::neigh_cchmc_2010
codec_data_geography_name <- names(codec_data_geography)[[1]]

d <-
  purrr::map(
    codec_data_installed,
    \(x) codec::codec_data(name = x, geography = codec_data_geography),
    .progress = "interpolating codec data"
  ) |>
  purrr::set_names(codec_data_installed)

md <-
  purrr::map(d, as.list) |>
  purrr::map(\(x) purrr::pluck(x, "schema", "fields")) |>
  purrr::flatten()

d <-
  d |>
  purrr::map(tibble::as_tibble) |>
  dplyr::bind_rows()

x_var <- "pct_green_2019"
y_var <- "dep_index"

d_selected <-
  d |>
  dplyr::select(dplyr::all_of(c(codec_data_geography_name, x_var, y_var))) |>
  dplyr::left_join(codec_data_geography, by = codec_data_geography_name) |>
  sf::st_as_sf()

md_selected <- md[c(x_var, y_var)]


