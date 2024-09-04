if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
library(dplyr)
library(sf)
library(dpkg)

cagis_parcels <- dpkg::stow("https://github.com/geomarker-io/parcel/releases/download/cagis_parcels-v1.1.1/cagis_parcels-v1.1.1.parquet") |>
  dpkg::read_dpkg() |>
  select(parcel_id, centroid_lat, centroid_lon, land_use, condo_id, market_total_value, acreage, homestead, rental_registration)

online_parcels <- dpkg::stow("https://github.com/geomarker-io/parcel/releases/download/auditor_online_parcels-v0.2.1/auditor_online_parcels-v0.2.1.parquet") |>
  dpkg::read_dpkg()

property_code_enforcements <- dpkg::stow("https://github.com/geomarker-io/parcel/releases/download/property_code_enforcements-v1.0.1/property_code_enforcements-v1.0.1.parquet") |>
  dpkg::read_dpkg() |>
  mutate(
    parcel_id = stringr::str_sub(cagis_parcel_id, 2),
    parcel_id = stringr::str_pad(parcel_id, width = 13, side = "right", pad = "0"),
    .keep = "unused"
  ) |>
  filter(date >= "2014-01-01") |> # filter to last 10 years
  group_by(parcel_id, .drop = FALSE) |>
  summarize(violations = n())

d <-
  left_join(cagis_parcels, online_parcels, by = "parcel_id") |>
  left_join(property_code_enforcements, by = "parcel_id") |>
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010) |>
  st_drop_geometry()

d_land_use <-
  d |>
  # collapse land_use categories
  mutate(
    land_use_collapsed =
      case_when(
        land_use %in% c("apartment, 40+ units", "apartment, 4-19 units", "office / apartment over", "apartment, 20-39 units") ~ "apartment",
        land_use %in% c("single family dwelling") ~ "single_family_dwelling",
        land_use %in% c("two family dwelling", "three family dwelling") ~ "two_to_three_family_dwelling",
        land_use %in% c("condominium unit", "condo or pud garage") ~ "condominium",
        land_use %in% c("metropolitan housing authority", "lihtc res") ~ "assisted_housing",
        TRUE ~ "other"
      )
  ) |>
  group_by(census_tract_id_2010, land_use_collapsed) |>
  tally() |>
  mutate(fraction = round(n / sum(n, na.rm = TRUE), 2)) |>
  select(-n) |>
  tidyr::pivot_wider(
    names_from = land_use_collapsed,
    values_from = fraction,
    names_prefix = "fraction_"
  )

d_homestead <-
  d |>
  group_by(census_tract_id_2010, homestead) |>
  tally() |>
  mutate(fraction_homestead = round(n / sum(n, na.rm = TRUE), 2)) |>
  filter(homestead == TRUE) |>
  select(census_tract_id_2010, fraction_homestead)

d_violations <-
  d |>
  group_by(census_tract_id_2010) |>
  summarize(
    n_parcels = n(),
    n_violations = sum(violations, na.rm = TRUE)
  ) |>
  mutate(violations_per_parcel = n_violations / n_parcels)

d_medians <-
  d |>
  group_by(census_tract_id_2010) |>
  select(-violations) |>
  summarize(
    across(where(is.numeric), \(x) median(x, na.rm = TRUE))
  ) # per household?

d_out <-
  full_join(d_land_use, d_violations, by = "census_tract_id_2010") |>
  full_join(d_homestead, by = "census_tract_id_2010") |>
  full_join(d_medians, by = "census_tract_id_2010") |>
  filter(!is.na(census_tract_id_2010)) |>
  mutate(year = "2024")

out_dpkg <-
  d_out |>
  as_codec_dpkg(
    name = "parcel",
    version = "0.1.0",
    title = "Parcel Characteristics",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "parcel", "README.md")), collapse = "\n")
  )

codec_dpkg_s3_put(out_dpkg)
