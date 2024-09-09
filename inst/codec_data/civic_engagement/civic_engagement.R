if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

library(dplyr)
library(readr)

rd <- read_csv(
  "https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-20240905-no.csv",
  col_types =
    cols_only(
      AddressPreDirectional = col_character(),
      AddressNumber = col_double(),
      AddressStreet = col_character(),
      AddressSuffix = col_character(),
      CityName = col_character(),
      AddressZip = col_character(),
      `2024 General Election` = col_factor(),
      `2024 Primary Election` = col_factor(),
      `2023 General Election` = col_factor(),
      `2023 August Election` = col_factor(),
      PRIMARY_MAY_2023 = col_factor(),
      GENERAL_NOV_2022 = col_factor(),
      `AUG PRIMARY ELECTION 2022` = col_factor(),
      PRIMARY_MAY_2022 = col_factor(),
      GENERAL_NOV_2021 = col_factor(),
      PRIMARY_MAY_2021 = col_factor(),
      GENERAL_NOV_2020 = col_factor(),
      SPECIAL_AUG_2020 = col_factor()
    )
)

d <-
  rd |>
  dplyr::mutate(
    voter_address = paste(
      AddressPreDirectional, AddressNumber, AddressStreet,
      AddressSuffix, CityName, "OH", AddressZip
    ),
    .keep = "unused"
  )

# remove missing address components left in the paste
d$address <- gsub("NA ", "", d$voter_address, fixed = TRUE)

library(dht)

d_geocode <- dht::degauss_run(d, "geocoder", "3.3.0-v8")

