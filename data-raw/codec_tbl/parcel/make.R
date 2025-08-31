devtools::load_all()
codec_name <- "parcel"

library(dplyr, warn.conflicts = FALSE)
library(sf)
library(dpkg)
options(arrow.unsafe_metadata = TRUE)

cagis_parcels <-
  dpkg::stow("gh://geomarker-io/parcel/cagis_parcels-v1.1.1") |>
  dpkg::read_dpkg() |>
  select(
    parcel_id,
    centroid_lat,
    centroid_lon,
    land_use,
    condo_id,
    market_total_value,
    acreage,
    homestead,
    rental_registration
  )

online_parcels <-
  dpkg::stow("gh://geomarker-io/parcel/auditor_online_parcels-v0.2.1") |>
  dpkg::read_dpkg()

d <-
  left_join(cagis_parcels, online_parcels, by = "parcel_id") |>
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010) |>
  st_drop_geometry()

d_land_use <-
  d |>
  # collapse land_use categories
  mutate(
    land_use_collapsed = case_when(
      land_use %in%
        c(
          "apartment, 40+ units",
          "apartment, 4-19 units",
          "office / apartment over",
          "apartment, 20-39 units"
        ) ~
        "apartment",
      land_use %in% c("single family dwelling") ~ "single_family_dwelling",
      land_use %in% c("two family dwelling", "three family dwelling") ~
        "two_to_three_family_dwelling",
      land_use %in% c("condominium unit", "condo or pud garage") ~
        "condominium",
      land_use %in% c("metropolitan housing authority", "lihtc res") ~
        "assisted_housing",
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

d_medians <-
  d |>
  group_by(census_tract_id_2010) |>
  summarize(
    across(where(is.numeric), \(x) median(x, na.rm = TRUE))
  )

d_out <-
  full_join(d_land_use, d_homestead, by = "census_tract_id_2010") |>
  full_join(d_medians, by = "census_tract_id_2010") |>
  filter(!is.na(census_tract_id_2010)) |>
  mutate(year = 2024L)

d_out |>
  as_codec_tbl(
    name = codec_name,
    description = paste(
      readLines(fs::path_package(
        "codec",
        "data-raw",
        "codec_tbl",
        codec_name,
        "README.md"
      )),
      collapse = "\n"
    )
  ) |>
  write_codec_pin()
