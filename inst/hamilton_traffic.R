devtools::load_all()
name <- "hamilton_traffic"
version <- "v0.1.0" # should be "0.1.0" in repo; fix below if updated!!!

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d <-
  tibble::as_tibble(rd) |>
  dplyr::rename(census_tract_id_2010 = census_tract_id) |>
  dplyr::mutate(year = as.integer(2017))

d_tdr <-
  fr::as_fr_tdr(
    d,
    name = "hamilton_traffic",
    title = "Hamilton County Parcel and Household Traffic",
    version = "0.1.0",
    description = "Number and fraction of parcels and households near traffic by census tract",
    homepage = "https://geomarker.io/hamilton_traffic"
  ) |>
  fr::update_field("census_tract_id_2010", title = "Census Tract Identifier") |>
  fr::update_field("n_parcels", title = "Number of Parcels", description = "number of parcels in tract") |>
  fr::update_field("n_parcels_near_traffic", title = "Number of Parcels Near Traffic", description = "number of parcels within 400 m of roads with non-zero average annual daily traffic") |>
  fr::update_field("n_parcels_near_truck_traffic", title = "Number of Parcels Near Truck Traffic", description = "number of parcels within 400 m of roads with non-zero average annual daily truck traffic") |>
  fr::update_field("n_households", title = "Number of Households", description = "number of households in tract") |>
  fr::update_field("n_households_near_traffic", title = "Number of Households Near Traffic", description = "number of households within 400 m of roads with non-zero average annual daily traffic") |>
  fr::update_field("n_households_near_truck_traffic", title = "Number of Households Near Truck Traffic", description = "number of households within 400 m of roads with non-zero average annual daily truck traffic") |>
  fr::update_field("median_parcel_traffic", title = "Median Parcel Traffic", description = "median average annual daily traffic within 400 m of each parcel (vehicle-meters)") |>
  fr::update_field("median_household_traffic", title = "Median Household Traffic", description = "median average annual daily traffic within 400 m of each household (truck-meters)") |>
  fr::update_field("median_parcel_truck_traffic", title = "Median Parcel Truck Traffic", description = "median average annual daily truck traffic within 400 m of each parcel (vehicle-meters)") |>
  fr::update_field("median_household_truck_traffic", title = "Median Household Truck Traffic", description = "median average annual daily truck traffic within 400 m of each household (truck-meters)") |>
  fr::update_field("frac_households_near_traffic", title = "Fraction of Households Near Traffic", description = "fraction of households within 400 m of roads with non-zero average annual daily traffic") |>
  fr::update_field("frac_parcels_near_traffic", title = "Fraction of Parcels Near Traffic", description = "fraction of parcels within 400 m of roads with non-zero average annual daily traffic") |>
  fr::update_field("frac_households_near_truck_traffic", title = "Fraction of Households Near Truck Traffic", description = "fraction of households within 400 m of roads with non-zero average annual daily truck traffic") |>
  fr::update_field("frac_parcels_near_truck_traffic", title = "Fraction of Parcels Near Truck Traffic", description = "fraction of parcels within 400 m of roads with non-zero average annual daily truck traffic") |>
  fr::update_field("year",
    title = "Year",
    description = "The actual year is unique to each data product and denoted in the field names"
  )

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
