if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

tf <- tempfile(fileext = ".zip")
download.file("https://eji.cdc.gov/Documents/Data/2022/DBS/Ohio.zip", tf)
dpkg_path <- unzip(tf, exdir = tempdir())

rd <-
  sf::st_read(fs::path_temp("Ohio.gdb")) |>
  tibble::as_tibble()

out <-
  rd |>
  dplyr::filter(substr(GEOID, 1, 5) == "39061") |>
  dplyr::select(
    census_tract_id_2010 = GEOID,
    prcnt_area_within_1mi_epa_npl_site = E_NPL,
    prcnt_area_within_1mi_epa_tri_site = E_TRI,
    prcnt_area_within_1m_epa_tsd_site = E_TSD,
    prcnt_area_within_1mi_epa_rmp_site = E_RMP,
    prcnt_area_within_1mi_coal_mine = E_COAL,
    prcnt_area_within_1mi_lead_mine = E_LEAD,
    prcnt_area_within_1_mi_greenspace = E_PARK,
    prcnt_homes_built_before_1980 = E_HOUAGE,
    walkability_index_epa = E_WLKIND,
    prcnt_area_within_1mi_railroad = E_RAIL,
    prcnt_area_within_1mi_high_volume_road = E_ROAD,
    prcnt_area_within_1mi_airport = E_AIRPRT,
    prcnt_area_huc12_watershed = E_IMPWTR
  )

out$year <- 2022

out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "environmental_justice_index",
    version = "0.1.0",
    title = "Environmental Justice Index",
    homepage = "https://www.atsdr.cdc.gov/placeandhealth/eji/index.html",
    description = paste(readLines(fs::path_package("codec", "codec_data", "environmental_justice_index", "README.md")), collapse = "\n")
  )

dpkg::use_dpkg_badge(out_dpkg)
dpkg::dpkg_gh_release(out_dpkg)
