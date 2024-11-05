if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}

library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(addr)
library(dpkg)

sessionInfo()

rd <- read_csv(
  glue::glue(
    "https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-",
    format(Sys.Date(), "%Y%m%d"),
    "-no.csv"
  ),
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
  mutate(foofy_state = "OH") |>
  tidyr::unite("voter_address",
    c(
      AddressPreDirectional, AddressNumber, AddressStreet,
      AddressSuffix, CityName, foofy_state, AddressZip
    ),
    sep = " ",
    remove = TRUE,
    na.rm = TRUE
  )

cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())

d_geocode <- addr::addr_match_geocode(
  x = d$voter_address,
  ref_addr = cagis_addr()$cagis_addr,
  ref_s2 = cagis_s2,
  county = "39061",
  year = "2022"
)

d_geocode <- d_geocode |>
  mutate(
    bg = s2_join_tiger_bg(s2, "2023"),
    tract = substr(bg, 1, 11)
  )

d_rates <-
  bind_cols(d, d_geocode) |>
  group_by(tract) |>
  summarize(across(
    c(
      `2024 General Election`, `2024 Primary Election`,
      `2023 General Election`, `2023 August Election`,
      PRIMARY_MAY_2023, GENERAL_NOV_2022,
      `AUG PRIMARY ELECTION 2022`, PRIMARY_MAY_2022,
      GENERAL_NOV_2021, PRIMARY_MAY_2021,
      GENERAL_NOV_2020, SPECIAL_AUG_2020
    ),
    \(.) sum(!is.na(.)) / length(.)
  ))

out <-
  cincy::tract_tigris_2020 |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  left_join(d_rates, by = c("census_tract_id_2020" = "tract"))

out_dpkg <-
  out |>
  mutate(year = "2024") |>
  as_codec_dpkg(
    name = "voter_participation",
    version = glue::glue("0.1-", format(Sys.Date(), "%Y%m%d")),
    title = "Voter Participation Rates",
    homepage = "https://geomarker.io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "voter_participation", "README.md")), collapse = "\n")
  )

dpkg::dpkg_gh_release(out_dpkg, draft = FALSE)
