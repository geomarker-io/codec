if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

rd <- fr::read_fr_tdr("https://github.com/geomarker-io/hh_acs_measures/releases/download/v1.1.0/")

out <-
  rd |>
  fr::fr_filter(substr(census_tract_id, 1, 5) == "39061") |>
  fr::fr_select(-census_tract_vintage) |>
  fr::fr_rename(census_tract_id_2010 = census_tract_id)


out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "hh_acs_measures",
    version = "1.1.1",
    title = "Harmonized Historical American Community Survey Measures",
    homepage = "https://github.com/geomarker-io/hh_acs_measures",
    description = paste(readLines(fs::path_package("codec", "hh_acs_measures", "README.md")), collapse = "\n")
  )

codec_dpkg_s3_put(out_dpkg)
