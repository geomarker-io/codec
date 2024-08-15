if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

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

out$year <- 2020

out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "traffic",
    version = "0.1.2",
    title = "Average Annual Daily Truck and Total Traffic Counts",
    description = paste(readLines(fs::path_package("codec", "codec_data", "traffic", "README.md")), collapse = "\n"),
    homepage = "https://geomarker.io/codec"
  )

codec_dpkg_s3_put(out_dpkg)
