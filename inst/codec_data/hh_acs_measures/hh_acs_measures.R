if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

all_acs5_variables <-
  dpkg::stow_url("https://api.census.gov/data/2022/acs/acs5/variables.json") |>
  jsonlite::read_json()

get_acs_5yr_data <- function(acs_variables, state = "39", county = "061", year = "2022") {
  if (Sys.getenv("CENSUS_API_KEY") == "") stop("set CENSUS_API_KEY enviroment variable")
  cli::cli_alert_info(glue::glue("getting {paste(acs_variables, collapse = ', ')}; defined as:"))
  all_acs5_variables$variables[acs_variables] |>
    vapply(\(.) paste(.$concept, .$label, collapse = "!!"), character(1)) |>
    stats::setNames(rep_len("*", length(acs_variables))) |>
    cli::cli_bullets()
  the_resp <-
    httr2::request("https://api.census.gov/data") |>
    httr2::req_url_path_append(year) |>
    httr2::req_url_path_append("acs") |>
    httr2::req_url_path_append("acs5") |>
    httr2::req_url_query(get = acs_variables, .multi = "comma") |>
    ## httr2::req_url_query(`for` = "block group:*") |>
    ## httr2::req_url_query(`in` = glue::glue("state:{state} county:{county} tract:*")) |>
    httr2::req_url_query(`for` = "tract:*") |>
    httr2::req_url_query(`in` = glue::glue("state:{state} county:{county}")) |>
    httr2::req_url_query(`key` = Sys.getenv("CENSUS_API_KEY")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  out <-
    the_resp |>
    purrr::discard_at(1) |>
    purrr::list_transpose() |>
    stats::setNames(the_resp[[1]]) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(acs_variables), as.numeric),
      census_tract_id_2020 = paste0(state, county, tract),
      ## census_blockgroup_id_2020 = paste0(state, county, tract, .data$`block group`),
      .keep = "none"
    )
  return(out)
}


list(
  prcnt_poverty =
    get_acs_5yr_data(c("B17001_001E", "B17001_002E")) |>
    dplyr::mutate(prcnt_poverty = B17001_002E / B17001_001E, .keep = "unused"),
  prcnt_public_assistance =
    get_acs_5yr_data(c("B19058_001E", "B19058_002E")) |>
    dplyr::mutate(prcnt_snap = B19058_001E / B19058_002E, .keep = "unused")
)




out_dpkg <-
  out |>
  as_codec_dpkg(
    name = "hh_acs_measures",
    version = "1.1.1",
    title = "Harmonized Historical American Community Survey Measures",
    homepage = "https://github.com/geomarker-io/hh_acs_measures",
    description = paste(readLines(fs::path_package("codec", "codec_data", "hh_acs_measures", "README.md")), collapse = "\n")
  )

codec_dpkg_s3_put(out_dpkg)
