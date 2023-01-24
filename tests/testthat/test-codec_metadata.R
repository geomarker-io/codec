test_that("codec_tdr", {
  expect_snapshot(codec_tdr())
})

test_that("check codec_tdr", {
  tdr <- read_tdr(test_path("tract_indices"))$tdr

  expect_no_condition(check_codec_tdr(tdr))

  tdr |>
    purrr::assign_in("name", NULL) |>
    check_codec_tdr() |>
    expect_error(regexp = "`name` property descriptor is required")

  tdr |>
    purrr::assign_in("path", NULL) |>
    check_codec_tdr() |>
    expect_error(regexp = "`path` property descriptor is required")

  tdr |>
    purrr::assign_in("url", "https://geomarker.io") |>
    check_codec_tdr() |>
    expect_error(regexp = "not allowed: url")

  # more than one wrong descriptor
  tdr |>
    purrr::assign_in("url", "https://geomarker.io") |>
    purrr::assign_in("nope", "wrong") |>
    check_codec_tdr() |>
    expect_error(regexp = "not allowed: url")

  tdr |>
    purrr::assign_in(c("schema", "foo"), "bar") |>
    check_codec_tdr() |>
    expect_error(regexp = "not allowed: foo")

  tdr |>
    purrr::assign_in(c("schema", "fields", "census_tract_id", "foo"), "bar") |>
    check_codec_tdr() |>
    expect_error(regexp = "not allowed: foo")

  test_path("d_dups") |>
    read_tdr() |>
    expect_error(regexp = "Duplicate map key: 'id'")

  tdr |>
    purrr::assign_in("path", "not_the_file.csv") |>
    check_codec_tdr() |>
    expect_error(regexp = "does not match file in path:")

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

