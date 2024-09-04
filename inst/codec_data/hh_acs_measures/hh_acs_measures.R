if (tryCatch(read.dcf("DESCRIPTION")[1, "Package"] == "codec", finally = FALSE)) {
  devtools::load_all()
} else {
  library(codec)
}
message("Using CoDEC, version ", packageVersion("codec"))

all_acs5_variables <-
  dpkg::stow("https://api.census.gov/data/2022/acs/acs5/variables.json") |>
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
    httr2::req_retry() |>
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
  out <-
    out |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(acs_variables), \(.) dplyr::na_if(., -666666666)))
  return(out)
}

## some that are not simple percentages:
# n_children
# insurance
# rent at least 30% of income
# substandard housing conditions
# year built categories
# primary langague at home is not english
# adults with at least high school education

#' make ACS 5 year n data
#' @param X formula: my_acs_var ~ B000000_000
#' @examples
#' make_acs_5y_n_data(n_households ~ B11005_001E)
make_acs_5y_n_data <- function(x) {
  var_name <- as.list(as.formula(x))[[2]]
  var_census_name <- as.formula(x)[[3]]
  out <-
    get_acs_5yr_data(as.character(var_census_name)) |>
    dplyr::rename({{ var_name }} := {{ var_census_name }}) |>
    dplyr::mutate({{ var_name }} := round({{ var_name }}, 0))
  return(out)
}

#' make ACS 5 year percentage data
#' @param x formula: my_acs_var ~ B00000_000 / B00000_000
#' @param .keep passed to dplyr::mutate when creating new percentage column
#' @examples
#' make_acs_5y_prcnt_data(prcnt_poverty ~ B17001_001E / B17001_002E)
make_acs_5y_prcnt_data <- function(x, .keep = "unused") {
  var_name <- as.list(as.formula(x))[[2]]
  var_numerator <- as.formula(x)[[3]][[2]]
  var_denominator <- as.formula(x)[[3]][[3]]
  out <-
    dplyr::left_join(
      get_acs_5yr_data(as.character(var_numerator)),
      get_acs_5yr_data(as.character(var_denominator)),
      by = "census_tract_id_2020"
    ) |>
    dplyr::mutate(
      {{ var_name }} := round({{ var_numerator }} / {{ var_denominator }} * 100, 2),
      .keep = .keep
    )
  return(out)
}

#' make ACS 5 year n and percentage data
#'
#' ACS derived variables are expressed as formulas that use
#' [ACS census variables] (https://api.census.gov/data/2022/acs/acs5/variables.json)
#' Currently, two types of formulas are supported:
#'
#' - n, count, or median data are often available as an existing variable and can be expressed as: `my_acs_measure ~ B00000_000E`
#' - percentage data are often computed using a numerator and denominator and can be expressed as: `my_acs_measure ~ B00000_000E / B00000_000E`
#' @examples
#' make_acs_5y_data(n_households ~ B11005_001E)
#' make_acs_5y_data(prcnt_poverty ~ B17001_001E / B17001_002E)
make_acs_5y_data <- function(x, .keep = "unused") {
  if (inherits(as.formula(x)[[3]], "name")) {
    return(make_acs_5y_n_data(x))
  }
  return(make_acs_5y_prcnt_data(x, .keep = .keep))
}

out <-
  list(
    n_households ~ B11005_001E,
    n_households_children ~ B11005_002E,
    n_housing_units ~ B25001_001E,
    median_home_value ~ B25077_001E,
    prcnt_poverty ~ B17001_002E / B17001_001E,
    prcnt_recieved_public_assistance_income ~ B19058_002E / B19058_001E,
    prcnt_family_households_with_single_householder ~ B11001_004E / B11001_002E,
    prcnt_employment_among_civilian_workforce ~ B23025_004E / B23025_003E,
    prcnt_housing_units_occupied_by_renters ~ B25003_003E / B25003_001E,
    prcnt_median_rent_to_income_ratio_among_renters ~ B25071_001E,
    prcnt_housing_units_vacant ~ B25002_003E / B25002_001E,
    prcnt_white_and_not_hispanic_or_latino ~ B03002_003E / B03002_001E,
    prcnt_black_and_not_hispanic_or_latino ~ B03002_004E / B03002_001E,
    prcnt_white_and_hispanic_or_latino ~ B03002_013E / B03002_001E,
    prcnt_black_and_hispanic_or_latino ~ B03002_014E / B03002_001E
  ) |>
  purrr::map(make_acs_5y_data, .progress = "making acs data") |>
  purrr::reduce(dplyr::left_join, by = "census_tract_id_2020")

out_dpkg <-
  out |>
  dplyr::mutate(year = 2022) |>
  as_codec_dpkg(
    name = "hh_acs_measures",
    version = "0.0.1",
    title = "Harmonized Historical American Community Survey Measures",
    homepage = "https://github.com/geomarker-io/codec",
    description = paste(readLines(fs::path_package("codec", "codec_data", "hh_acs_measures", "README.md")), collapse = "\n")
  )

dpkg::dpkg_gh_release(out_dpkg, draft = FALSE)
