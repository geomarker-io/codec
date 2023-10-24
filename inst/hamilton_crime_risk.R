devtools::load_all()
library(fr)
name <- "hamilton_crime_risk"
version <- "v0.1.0"

rd <-
  read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

d_tdr <-
  rd |>
  fr_rename(census_tract_id_2010 = census_tract_id,
            crime_total = Total,
            crime_personal = Personal,
            crime_murder = Murder,
            crime_rape = Rape,
            crime_robbery = Robbery,
            crime_assualt = Assault,
            crime_property = Property,
            crime_burglary = Burglary,
            crime_theft = Theft,
            crime_motveh = MotVeh) |>
  fr_mutate(year = as.integer(2020)) |>
  fr::update_field("year", description = "Represents 2014 - 2020 average")

d_tdr@version <- version
d_tdr@homepage <- "https://geomarker.io/hamilton_crime_risk"

d_tdr@description <- "Average [crime risk](https://github.com/geomarker-io/hamilton_crime_risk/blob/master/AGS-CrimeRisk-Methodology-2022A.pdf) (2014-2020) at 2020 census blocks interpolated (population weighted) to 2010 census tracts."

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
