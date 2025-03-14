if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
source("inst/codec_data/green/dataverse.R")

nlcd_legend <-
  tibble::tribble(
    ~value, ~landcover_class, ~landcover, ~green,
    11, "water", "water", FALSE,
    12, "water", "ice/snow", FALSE,
    21, "developed", "developed open", TRUE,
    22, "developed", "developed low intensity", TRUE,
    23, "developed", "developed medium intensity", FALSE,
    24, "developed", "developed high intensity", FALSE,
    31, "barren", "rock/sand/clay", FALSE,
    41, "forest", "deciduous forest", TRUE,
    42, "forest", "evergreen forest", TRUE,
    43, "forest", "mixed forest", TRUE,
    51, "shrubland", "dwarf scrub", TRUE,
    52, "shrubland", "shrub/scrub", TRUE,
    71, "herbaceous", "grassland", TRUE,
    72, "herbaceous", "sedge", TRUE,
    73, "herbaceous", "lichens", TRUE,
    74, "herbaceous", "moss", TRUE,
    81, "cultivated", "pasture/hay", TRUE,
    82, "cultivated", "cultivated crops", TRUE,
    90, "wetlands", "woody wetlands", TRUE,
    95, "wetlands", "emergent herbaceous wetlands", TRUE
  )

green_rast <-
  dpkg::stow("https://s3-us-west-2.amazonaws.com/mrlc/Annual_NLCD_LndCov_2023_CU_C1V0.tif") |>
  terra::rast()

hc <- 
  codec::cincy_county_geo() |> 
  sf::st_as_sfc() |> 
  terra::vect() |> 
  terra::project(green_rast)

tract <- 
  codec::cincy_census_geo("tract", "2020") |> 
  terra::vect() |> 
  terra::project(green_rast)

out <- tibble::tibble(census_tract_id_2020 = tract$geoid)

# land cover - greenspace
out$greenspace <-
  green_rast |>
  terra::crop(hc) |>
  terra::extract(tract) |>
  dplyr::rename(value = 2) |>
  dplyr::left_join(nlcd_legend, by = "value") |>
  dplyr::group_by(ID) |>
  dplyr::summarize(greenspace = round(sum(green) / dplyr::n() * 100)) |>
  dplyr::pull(greenspace)

# tree canopy
out$treecanopy <- 
  dpkg::stow("https://s3-us-west-2.amazonaws.com/mrlc/nlcd_tcc_CONUS_2021_v2021-4.zip") |>
  unzip(files = "nlcd_tcc_conus_2021_v2021-4.tif", exdir = "/Users/RASV5G/Library/Application Support/org.R-project.R/R/stow/") |>
  terra::rast() |>
  terra::crop(hc) |>
  terra::extract(tract, fun = median, ID = FALSE) |>
  dplyr::pull(1)

# impervious
out$impervious <- 
  get_dv_url(
      persistent_id = "doi:10.7910/DVN/KXETFC",
      filename = glue::glue("Annual_NLCD_FctImp_2023_CU_C1V0_COG.tif"),
      version = "latest"
    ) |>
  terra::rast(vsi = TRUE) |>
  terra::extract(tract, fun = median, ID = FALSE) |>
  dplyr::pull(1)

# EVI 
out$evi <- 
  terra::sds("data-raw/MOD13Q1.A2024161.h11v05.061.2024181211403.hdf")[2] |> 
  terra::project(terra::crs(hc)) |>
  terra::crop(hc) |>
  terra::extract(tract, fun = median) |>
  dplyr::mutate(evi = round(`250m 16 days EVI`*0.0001, 3)) |>
  dplyr::pull(evi)

# parks
sf::st_layers(dsn = "data-raw/TPL.gdb/")
tract_sf <- codec::cincy_census_geo("tract", "2020") |> sf::st_transform(5072)

parks_sa <- 
  sf::st_read("data-raw/TPL.gdb/", layer = "ParkServiceAreas", quiet = TRUE) |>
  sf::st_transform(sf::st_crs(tract_sf)) 

parks_green <- 
    sf::st_read("data-raw/TPL.gdb/", layer = "Greenspace", quiet = TRUE) |>
    sf::st_transform(sf::st_crs(tract_sf)) 

get_area_pct <- function(tract, parks) {
  area_denom <- sf::st_area(tract) |> as.numeric()

  intersection <- sf::st_intersection(tract, parks)

  if(nrow(intersection) < 1) {
    return(0)
  } else {
    area_numer <- 
      intersection|>
      sf::st_union() |>
      sf::st_area() |>
      as.numeric()
  
    return(round(area_numer / area_denom * 100))
  }
}

out$park_service_area <- 
  purrr::map_dbl(
    1:nrow(tract_sf),
    \(x) get_area_pct(tract_sf[x,], parks_sa)
  )

out$park_greenspace <- 
    purrr::map_dbl(
      1:nrow(tract_sf),
      \(x) get_area_pct(tract_sf[x,], parks_green)
    )

# dpkg
out_dpkg <-
  out |>
  dplyr::mutate(year = 2023) |>
  as_codec_dpkg(
    name = "green",
    version = "0.1.0",
    title = "Greenspace and Built Environment",
    homepage = "https://github.com/geomarker-io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "green", "README.md")), collapse = "\n")
  )

dpkg::dpkg_gh_release(out_dpkg, draft = FALSE)
