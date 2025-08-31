devtools::load_all()
codec_name <- "voter_participation"

library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(s2)

rd <- read_csv(
  glue::glue(
    "https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-",
    format(Sys.Date(), "%Y%m%d"),
    "-no.csv"
  ),
  col_types = cols_only(
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
    GENERAL_NOV_2020 = col_factor()
  )
)

d <-
  rd |>
  mutate(foofy_state = "OH") |>
  tidyr::unite(
    "voter_address",
    c(
      AddressPreDirectional,
      AddressNumber,
      AddressStreet,
      AddressSuffix,
      CityName,
      foofy_state,
      AddressZip
    ),
    sep = " ",
    remove = TRUE,
    na.rm = TRUE
  )

source(
  fs::path_package(
    "codec",
    "data-raw",
    "codec_tbl",
    codec_name,
    "geocode_degauss.R"
  )
)

d_gcd <- geocode_degauss(d$voter_address)


d_gcd <-
  d_gcd |>
  mutate(
    s2_cell = as_s2_cell(s2_lnglat(lon, lat)),
    census_tract_id_2020 = substr(s2_join_tiger_bg(s2_cell, "2020"), 1, 11),
  )

d_rates <-
  d |>
  mutate(census_tract_id_2020 = d_gcd$census_tract_id_2020) |>
  summarize(
    across(
      c(
        `2024 General Election`,
        `2024 Primary Election`,
        `2023 General Election`,
        `2023 August Election`,
        PRIMARY_MAY_2023,
        GENERAL_NOV_2022,
        `AUG PRIMARY ELECTION 2022`,
        PRIMARY_MAY_2022,
        GENERAL_NOV_2021,
        PRIMARY_MAY_2021
      ),
      \(.) sum(!is.na(.)) / length(.)
    ),
    .by = census_tract_id_2020
  )

out <-
  cincy_census_geo("tract", "2020") |>
  select(census_tract_id_2020 = geoid) |>
  sf::st_drop_geometry() |>
  left_join(d_rates, by = "census_tract_id_2020") |>
  mutate(year = 2025L)

out |>
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
