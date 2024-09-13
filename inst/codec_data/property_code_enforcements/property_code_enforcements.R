if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
library(dplyr)
library(dpkg)
library(addr)
options(arrow.unsafe_metadata = TRUE)

cagis_addr_data <- 
  addr::cagis_addr |>
  mutate(cagis_s2 = purrr::map(cagis_addr_data, \(d) pull(d, cagis_s2))) |>
  select(-cagis_addr_data) |>
  tidyr::unnest(cols = c(cagis_s2)) |>
  filter(!is.na(cagis_s2)) |>
  distinct(cagis_addr, .keep_all = TRUE) |>
  mutate(census_tract_id_2010 = tract::get_census_tract_id(cagis_s2, year = "2010")) |>
  distinct(cagis_addr, .keep_all = TRUE) 

addr_per_tract <-
  cagis_addr_data |>
  group_by(census_tract_id_2010) |>
  summarize(n_addr = n())

# read in parcel data resource
property_code_enforcements <-
  dpkg::stow("gh://geomarker-io/parcel/property_code_enforcements-v1.0.1") |>
  dpkg::read_dpkg() |>
  mutate(
      year = lubridate::year(date), 
      month = lubridate::month(date)
  ) |>
  left_join(cagis_addr_data, by = "cagis_addr") |>
  group_by(census_tract_id_2010, year, month) |>
  summarize(n_violations = n()) |>
  filter(!is.na(census_tract_id_2010)) |>
  left_join(addr_per_tract, by = "census_tract_id_2010") |>
  mutate(violations_per_addr = n_violations/n_addr)

all_tracts <- 
  cincy::tract_tigris_2010 |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  mutate(date = list(seq.Date(
    from = as.Date("2001-04-01"), 
    to = as.Date("2024-07-01"), 
    by = "month"
  ))) |>
  tidyr::unnest(cols = c(date)) |>
  mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date)
  ) |>
  select(-date)

out_dpkg <-
  left_join(
    all_tracts, 
    property_code_enforcements, 
    by = c("census_tract_id_2010", "year", "month")) |>
  as_codec_dpkg(
    name = "property_code_enforcements",
    version = "0.1.0",
    title = "Property Code Enforcements",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "property_code_enforcements", "README.md")), collapse = "\n")
  )

dpkg_gh_release(out_dpkg, draft = FALSE)