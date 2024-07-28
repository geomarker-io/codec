dest_path <- tempfile(fileext = ".gdb.zip")

"https://www.arcgis.com/sharing/rest/content/items/c199f2799b724ffbacf4cafe3ee03e55/data" |>
  utils::download.file(dest_path, mode = "wb", method = "wget")

#' sf::st_layers(dsn = dest_path)$name |>
#'   strsplit("_", fixed = TRUE) |>
#'   purrr::map_chr(3)

rd <-
  sf::st_read(
    dsn = dest_path,
    query = paste(
      "SELECT F_SYSTEM, AADT, AADT_SINGLE_UNIT, AADT_COMBINATION",
      "FROM HPMS_FULL_OH_2020",
      "WHERE F_SYSTEM IN ('1', '2', '3')"
    ),
    quiet = FALSE
  )

aadt <-
  rd |>
  sf::st_transform(sf::st_crs(cincy::tract_tigris_2020)) |>
  sf::st_zm()

out <-
  sf::st_join(cincy::tract_tigris_2020, aadt) |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::group_by(census_tract_id_2020) |>
  dplyr::summarize(
    aadt = sum(AADT, na.rm = TRUE),
    aadt_truck = sum(AADT_SINGLE_UNIT, AADT_COMBINATION, na.rm = TRUE)
  )

library(dotenv)
dpkg_write(
  out,
  name = "aadt",
  version = "0.1.0",
  dir = tempdir(),
  readme_file = fs::path("inst", "traffic", "README", ext = "md"),
  source_file = fs::path("inst", "traffic", "source", ext = "R")
) |>
  dpkg_s3_put(dpkg_path)
