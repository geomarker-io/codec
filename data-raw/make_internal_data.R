devtools::load_all()

cincy_tract_geo_2020 <- cincy_census_geo("tract", "2020", packaged = FALSE)
cincy_bg_geo_2020 <- cincy_census_geo("bg", "2020", packaged = FALSE)
cincy_county_geo_2020 <- cincy_county_geo("2020", packaged = FALSE)
cincy_addr_geo_2025 <- cincy_addr_geo(packaged = FALSE)
cincy_neighborhood_geo_sna <- cincy_neighborhood_geo(
  "statistical_neighborhood_approximations",
  packaged = FALSE
)

usethis::use_data(
  cincy_tract_geo_2020,
  cincy_bg_geo_2020,
  cincy_county_geo_2020,
  cincy_addr_geo_2025,
  cincy_neighborhood_geo_sna,
  internal = TRUE,
  overwrite = TRUE
)
