devtools::load_all()

# calling these will still utilize the downloaded files, if available
cincy_tract_geo_2020 <- cincy_census_geo("tract", "2020", packaged = FALSE)
cincy_bg_geo_2020 <- cincy_census_geo("bg", "2020", packaged = FALSE)
cincy_addr_geo_2025 <- cincy_addr_geo(packaged = FALSE)
cincy_neighborhood_geo_sna <- cincy_neighborhood_geo(
  "statistical_neighborhood_approximations",
  packaged = FALSE
)
cincy_zcta_geo_2020 <- cincy_zcta_geo("2020", packaged = FALSE)
cincy_block_weights_local <- cincy_block_weights(packaged = FALSE)

usethis::use_data(
  cincy_tract_geo_2020,
  cincy_bg_geo_2020,
  cincy_addr_geo_2025,
  cincy_neighborhood_geo_sna,
  cincy_zcta_geo_2020,
  cincy_block_weights_local,
  internal = TRUE,
  overwrite = TRUE
)
