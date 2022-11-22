test_that("check metadata name", {
  tdr <- read_tdr("https://codec-data.s3.amazonaws.com/census_mega_data/tabular-data-resource.yaml")
  expect_no_condition(check_tdr_name(tdr$name))
  expect_error(check_tdr_name(tdr$wrong))
  expect_error(check_tdr_name(1))
  expect_error(check_tdr_name("CensusData"))
  expect_error(check_tdr_name("census data"))
  expect_error(check_tdr_name("census+data"))
})
