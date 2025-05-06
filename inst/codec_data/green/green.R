if (
  tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)
) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
source("inst/codec_data/green/dataverse.R")

# fmt: skip
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
  dpkg::stow(
    "https://s3-us-west-2.amazonaws.com/mrlc/Annual_NLCD_LndCov_2023_CU_C1V0.tif"
  ) |>
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
out$greenspace_2023 <-
  green_rast |>
  terra::crop(hc) |>
  terra::extract(tract) |>
  dplyr::rename(value = 2) |>
  dplyr::left_join(nlcd_legend, by = "value") |>
  dplyr::group_by(ID) |>
  dplyr::summarize(greenspace = round(sum(green) / dplyr::n() * 100)) |>
  dplyr::pull(greenspace)

# tree canopy
out$treecanopy_2021 <-
  dpkg::stow(
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_tcc_CONUS_2021_v2021-4.zip"
  ) |>
  unzip(
    files = "nlcd_tcc_conus_2021_v2021-4.tif",
    exdir = "/Users/RASV5G/Library/Application Support/org.R-project.R/R/stow/"
  ) |>
  terra::rast() |>
  terra::crop(hc) |>
  terra::extract(tract, fun = mean, ID = FALSE) |>
  dplyr::pull(1)

# impervious
out$impervious_2023 <-
  get_dv_url(
    persistent_id = "doi:10.7910/DVN/KXETFC",
    filename = glue::glue("Annual_NLCD_FctImp_2023_CU_C1V0_COG.tif"),
    version = "latest"
  ) |>
  terra::rast(vsi = TRUE) |>
  terra::extract(tract, fun = mean, ID = FALSE) |>
  dplyr::pull(1)

# EVI
out$evi_2024 <-
  terra::sds("data-raw/MOD13Q1.A2024161.h11v05.061.2024181211403.hdf")[2] |>
  terra::project(terra::crs(hc)) |>
  terra::crop(hc) |>
  terra::extract(tract, fun = mean) |>
  dplyr::mutate(evi = round(`250m 16 days EVI` * 0.0001, 3)) |>
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

  if (nrow(intersection) < 1) {
    return(0)
  } else {
    area_numer <-
      intersection |>
      sf::st_union() |>
      sf::st_area() |>
      as.numeric()

    return(round(area_numer / area_denom * 100))
  }
}

out$park_service_area <-
  purrr::map_dbl(
    1:nrow(tract_sf),
    \(x) get_area_pct(tract_sf[x, ], parks_sa)
  )

out$park_greenspace <-
  purrr::map_dbl(
    1:nrow(tract_sf),
    \(x) get_area_pct(tract_sf[x, ], parks_green)
  )

# city tree canopy
city_canopy <-
  sf::st_read(
    "data-raw//Cincinnati Canopy Inforamtion 2/CincinnatiCanopyInformation.gdb",
    layer = "Block_Group_Canopy_Change",
    quiet = TRUE
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    block_group_id_2010 = ID,
    TreeCanopy_2020_Area
  ) |>
  dplyr::mutate(
    census_tract_id_2010 = stringr::str_sub(block_group_id_2010, 1, 11)
  ) |>
  dplyr::group_by(census_tract_id_2010) |> #2010 tract/bgs
  dplyr::summarize(TreeCanopy_2020_Area = sum(TreeCanopy_2020_Area)) |>
  dplyr::left_join(
    codec::cincy_census_geo("tract", "2013"),
    by = c("census_tract_id_2010" = "geoid")
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(3735) |>
  dplyr::mutate(
    tract_area = sf::st_area(s2_geography),
    city_treecanopy_2020 = as.numeric(round(
      TreeCanopy_2020_Area / tract_area * 100
    ))
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(census_tract_id_2010, city_treecanopy_2020)

out$city_treecanopy_2020 <-
  dplyr::left_join(
    codec::cincy_census_geo("tract", "2013"),
    city_canopy,
    by = c("geoid" = "census_tract_id_2010")
  ) |>
  dplyr::rename(
    census_tract_id_2010 = geoid,
    geometry = s2_geography
  ) |>
  sf::st_transform(5072) |>
  cincy::interpolate(to = cincy::tract_tigris_2020, weights = "area") |>
  sf::st_drop_geometry() |>
  dplyr::pull(city_treecanopy_2020)

# dpkg
out_dpkg <-
  out |>
  dplyr::mutate(year = 2023) |>
  as_codec_dpkg(
    name = "green",
    version = "0.1.0",
    title = "Greenspace and Built Environment",
    homepage = "https://github.com/geomarker-io/codec",
    description = paste(
      readLines(fs::path_package("codec", "codec_data", "green", "README.md")),
      collapse = "\n"
    )
  )

dpkg::write_dpkg(out_dpkg, dir = "inst/codec_data/green/")

dpkg::dpkg_gh_release(out_dpkg, draft = FALSE)
