devtools::load_all()
name <- "hamilton_landcover"
version <- "v0.1.0" # should be "0.1.0" in repo; fix below if updated!!!

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d <-
  tibble::as_tibble(rd) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = as.integer(2019))

d_tdr <-
  fr::as_fr_tdr(d,
    name = name,
    title = "Hamilton County Landcover and Built Environment Characteristics",
    ## version = version,
    version = "0.1.0",
    description = "Greenspace, imperviousness, treecanopy, and greenness (EVI) for all tracts in Hamilton County",
    homepage = "https://geomarker.io/hamilton_landcover"
  ) |>
  fr::update_field("census_tract_id_2010",
    title = "Census Tract Identifier"
  ) |>
  fr::update_field("pct_green_2019",
    title = "Percent Greenspace 2019",
    description = "percent of pixels in each tract classified as green"
  ) |>
  fr::update_field("pct_impervious_2019",
    title = "Percent Impervious 2019",
    description = "average percent imperviousness for pixels in each tract"
  ) |>
  fr::update_field("pct_treecanopy_2016",
    title = "Percent Treecanopy 2016",
    description = "average percent tree canopy for pixels in each tract"
  ) |>
  fr::update_field("evi_2018",
    title = "Enhanced Vegetation Index 2018",
    description = "average enhanced vegetation index for pixels in each tract"
  ) |>
  fr::update_field("year",
    title = "Year",
    description = "The actual year is unique to each data product and denoted in the field names"
  )

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
