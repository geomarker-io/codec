devtools::load_all()
codec_name <- "landcover"

rd <- readr::read_csv(
  "https://github.com/geomarker-io/hamilton_landcover/releases/download/v0.1.0/hamilton_landcover.csv"
)

out <-
  rd |>
  tibble::as_tibble() |>
  dplyr::rename(
    enhanced_vegatation_index_2018 = evi_2018,
    census_tract_id_2010 = census_tract_id
  ) |>
  dplyr::mutate(year = 2019L)

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
