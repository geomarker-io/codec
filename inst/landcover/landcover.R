if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

rd <- fr::read_fr_tdr("https://github.com/geomarker-io/hamilton_landcover/releases/download/v0.1.0/")

out <-
  rd |>
  tibble::as_tibble() |>
  dplyr::rename(enhanced_vegatation_index_2018 = evi_2018,
                census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = 2019)

out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "landcover",
    version = "0.1.0",
    title = "Landcover, Built Environment, and Greenness",
    homepage = "https://github.com/geomarker-io/hamilton_landcover",
    description = paste(readLines(fs::path_package("codec", "landcover", "README.md")), collapse = "\n")
  )

codec_dpkg_s3_put(out_dpkg)
