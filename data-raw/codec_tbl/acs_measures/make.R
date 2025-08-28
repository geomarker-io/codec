devtools::load_all()
codec_name <- "acs_measures"

library(dplyr, warn.conflicts = FALSE)

all_acs5_variables <-
  dpkg::stow("https://api.census.gov/data/2022/acs/acs5/variables.json") |>
  jsonlite::read_json()

get_acs_5yr_data <- function(
  acs_variables,
  state = "39",
  county = "061",
  year = "2022"
) {
  if (Sys.getenv("CENSUS_API_KEY") == "")
    stop("set CENSUS_API_KEY enviroment variable")
  cli::cli_alert_info(glue::glue(
    "getting {paste(acs_variables, collapse = ', ')}; defined as:"
  ))
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
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(acs_variables), as.numeric),
      census_tract_id_2020 = paste0(state, county, tract),
      ## census_blockgroup_id_2020 = paste0(state, county, tract, .data$`block group`),
      .keep = "none"
    )
  out <-
    out |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(acs_variables),
      \(.) dplyr::na_if(., -666666666)
    ))
  return(out)
}

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
    dplyr::mutate({{ var_name }} := {{ var_name }})
  return(out)
}

#' make ACS 5 year percentage data
#' @param x formula: my_acs_var ~ B00000_000 / B00000_000
#' @examples
#' make_acs_5y_prop_data(prcnt_poverty ~ B17001_002E / B17001_001E)
make_acs_5y_prop_data <- function(x) {
  vars <- attr(terms(as.formula(x)), "variables")
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
      {{ var_name }} := {{ var_numerator }} / {{ var_denominator }},
      .keep = "unused"
    )
  return(out)
}

make_acs_5y_data <- function(x) {
  if (inherits(as.formula(x)[[3]], "name")) {
    return(make_acs_5y_n_data(x))
  }
  return(make_acs_5y_prop_data(x))
}

out <-
  list(
    n_households ~ B11005_001E,
    n_households_children ~ B11005_002E,
    n_housing_units ~ B25001_001E,
    median_home_value ~ B25077_001E,
    prop_poverty ~ B17001_002E / B17001_001E,
    prop_recieved_public_assistance_income ~ B19058_002E / B19058_001E,
    prop_family_households_with_single_householder ~ B11001_004E / B11001_002E,
    prop_employment_among_civilian_workforce ~ B23025_004E / B23025_003E,
    prop_housing_units_occupied_by_renters ~ B25003_003E / B25003_001E,
    prop_median_rent_to_income_ratio_among_renters ~ B25071_001E,
    prop_housing_units_vacant ~ B25002_003E / B25002_001E,
    prop_white_and_not_hispanic_or_latino ~ B03002_003E / B03002_001E,
    prop_black_and_not_hispanic_or_latino ~ B03002_004E / B03002_001E,
    prop_white_and_hispanic_or_latino ~ B03002_013E / B03002_001E,
    prop_black_and_hispanic_or_latino ~ B03002_014E / B03002_001E
  ) |>
  purrr::map(make_acs_5y_data, .progress = "making acs data")

out$n_persons_under_18 <-
  get_acs_5yr_data(c(
    paste0("B01001_00", 3:6, "E"),
    paste0("B01001_0", 27:30, "E")
  )) |>
  rowwise() |>
  mutate(
    n_persons_under_18 = sum(c_across(-census_tract_id_2020)),
    .keep = "unused"
  ) |>
  ungroup()

out$prop_health_insurance <-
  get_acs_5yr_data(c(
    "B27001_001E",
    paste0("B27001_00", c(4, 7), "E"),
    paste0("B27001_0", seq(10, 28, by = 3), "E"),
    paste0("B27001_0", seq(32, 56, by = 3), "E")
  )) |>
  rowwise() |>
  mutate(
    prop_health_insurance = sum(c_across(
      -c(B27001_001E, census_tract_id_2020)
    )) /
      B27001_001E,
    .keep = "unused"
  ) |>
  ungroup()

# rent at least 30% of income
out$rent_burdened <-
  get_acs_5yr_data(
    c("B25070_001E", paste0("B25070_", c("007", "008", "009", "010"), "E"))
  ) |>
  rowwise() |>
  mutate(
    prop_rent_burdened = sum(c_across(-c(B25070_001E, census_tract_id_2020))) /
      B25070_001E,
    .keep = "unused"
  ) |>
  ungroup()

out$housing_conditions <-
  get_acs_5yr_data(
    c(
      "B25123_001E",
      paste0("B25123_00", 3:6, "E"),
      "B25123_009E",
      paste0("B25123_0", 10:12, "E")
    )
  ) |>
  rowwise() |>
  mutate(
    prop_housing_conditions = sum(c_across(
      -c(B25123_001E, census_tract_id_2020)
    )) /
      B25123_001E,
    .keep = "unused"
  ) |>
  ungroup()

out$housing_age <-
  get_acs_5yr_data(
    c(
      "B25034_001E",
      paste0("B25034_00", 7:9, "E"),
      "B25034_010E"
    )
  ) |>
  rowwise() |>
  mutate(
    prop_built_prior_1980 = sum(c_across(
      -c(B25034_001E, census_tract_id_2020)
    )) /
      B25034_001E,
    .keep = "unused"
  ) |>
  ungroup()


out$language <-
  get_acs_5yr_data(
    c(
      "C16002_001E",
      paste0("C16002_00", c(4, 7), "E"),
      paste0("C16002_01", c(0, 3), "E")
    )
  ) |>
  rowwise() |>
  mutate(
    prop_limited_english_speaking = sum(c_across(
      -c(C16002_001E, census_tract_id_2020)
    )) /
      C16002_001E,
    .keep = "unused"
  ) |>
  ungroup()

# adults with at least high school education
out$edu <-
  get_acs_5yr_data(
    c("B15003_001E", paste0("B15003_0", 17:25, "E"))
  ) |>
  rowwise() |>
  mutate(
    prop_adults_hs_edu = sum(c_across(-c(B15003_001E, census_tract_id_2020))) /
      B15003_001E,
    .keep = "unused"
  ) |>
  ungroup()

out_tbl <- purrr::reduce(out, dplyr::left_join, by = "census_tract_id_2020")

out_dpkg <-
  out_tbl |>
  dplyr::mutate(year = 2022) |>
  as_codec_dpkg(
    name = "acs_measures",
    version = "0.1.0",
    title = "American Community Survey Measures",
    homepage = "https://github.com/geomarker-io/codec",
    description = paste(
      readLines(fs::path_package(
        "codec",
        "codec_data",
        "acs_measures",
        "README.md"
      )),
      collapse = "\n"
    )
  )


out_dpkg |>
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
