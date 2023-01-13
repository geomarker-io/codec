test_that("codec_tdr", {
  expect_snapshot(codec_tdr())
})

test_that("check metadata name", {
  tdr <- list()
  tdr$name <- "census_data"
  expect_no_condition(check_tdr_name(tdr$name))
  expect_error(check_tdr_name(tdr$wrong))
  expect_error(check_tdr_name(1))
  expect_error(check_tdr_name("CensusData"))
  expect_error(check_tdr_name("census data"))
  expect_error(check_tdr_name("census+data"))
})

test_that("check for census tract identifier", {

  d <-
    read_tdr_csv("https://github.com/geomarker-io/hamilton_drivetime/releases/download/v0.1.0") |>
    dplyr::rename(census_tract_id_2010 = census_tract_id) |>
    CODECtools::add_type_attrs()

  expect_equal(check_census_tract_id(d), d)

  expect_error({
    .x |>
      dplyr::select(-census_tract_id_2010) |>
      check_census_tract_id()
  },
  regexp = "must contain a census tract id column called")

  expect_error({
    .x |>
      dplyr::slice(-1) |>
      check_census_tract_id()
  },
  regexp = "does not contain every census tract")
                   
})
