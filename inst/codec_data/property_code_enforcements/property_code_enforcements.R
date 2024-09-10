if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))
library(dplyr)
library(sf)
library(dpkg)
library(addr)
options(arrow.unsafe_metadata = TRUE)

code_enforcement_url <- "https://data.cincinnati-oh.gov/api/views/cncm-znd6/rows.csv?accessType=DOWNLOAD"

raw_data <-
  readr::read_csv(
    code_enforcement_url,
    col_types = readr::cols_only(
      SUB_TYPE_DESC = "character",
      NUMBER_KEY = "character",
      ENTERED_DATE = readr::col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      FULL_ADDRESS = "character",
      LATITUDE = "numeric",
      LONGITUDE = "numeric",
      DATA_STATUS_DISPLAY = "character"
    )
  ) |>
  filter(
    !DATA_STATUS_DISPLAY %in% c(
      "Closed - No Violation",
      "Closed - No Violations Found",
      "Duplicate Case",
      "Closed - Duplicate Complaint"
    )
  ) |>
  mutate(SUB_TYPE_DESC = stringr::str_to_lower(SUB_TYPE_DESC),
         address = stringr::str_to_lower(FULL_ADDRESS), 
        address = clean_address_text(address)) |> 
  filter(address != "")

d <- 
  raw_data |>
  select(
    date = ENTERED_DATE, 
    lat = LATITUDE, 
    lon = LONGITUDE,
  ) |>
  mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date)
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(cincy::tract_tigris_2010)) |>
  st_join(cincy::tract_tigris_2010) |>
  st_drop_geometry() |>
  group_by(census_tract_id_2010, year, month) |>
  tally() |>
  filter(!is.na(census_tract_id_2010))

d_parcels_per_tract <- 
  dpkg::stow("https://github.com/geomarker-io/codec/releases/download/parcel-v0.1.0/parcel-v0.1.0.parquet") |>
  dpkg::read_dpkg() |>
  select(census_tract_id_2010, n_parcels)

d <- 
  left_join(d, d_parcels_per_tract, by = "census_tract_id_2010") |>
  mutate(violations_per_parcel = n / n_parcels) |>
  select(census_tract_id_2010, year, month, violations_per_parcel)

out_dpkg <-
  d |>
  as_codec_dpkg(
    name = "property_code_enforcements",
    version = "0.1.0",
    title = "Property Code Enforcements",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "property_code_enforcements", "README.md")), collapse = "\n")
  )

dpkg_gh_release(out_dpkg, draft = FALSE)