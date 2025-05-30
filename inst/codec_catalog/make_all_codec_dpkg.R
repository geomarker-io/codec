## library(codec)
library(dplyr, warn.conflicts = FALSE)

dpkgs <-
  list(
    get_codec_dpkg("acs_measures-v0.1.0") |>
      filter(year == "2022"),
    get_codec_dpkg("voter_participation-v0.2.0") |>
      mutate(census_tract_id_2020,
        year,
        voter_participation_rate = `2023 General Election`,
        .keep = "none"
      ),
    get_codec_dpkg("traffic-v0.1.2"),
    get_codec_dpkg("drivetime-v0.2.2"),
    get_codec_dpkg("environmental_justice_index-v0.1.0"),
    get_codec_dpkg("green-v0.1.0"),
    get_codec_dpkg("parcel-v0.1.1"),
    get_codec_dpkg("property_code_enforcements-v0.2.0"),
    get_codec_dpkg("xx_address-v0.2.0")
  )

saveRDS(dpkgs, "inst/codec_catalog/all_codec_dpkg.rds")

## dpkgs_fields <-
##   dpkgs |>
##   purrr::map(names) |>
##   purrr::map(\(.) setNames(., vapply(., snake_title, character(1))))
