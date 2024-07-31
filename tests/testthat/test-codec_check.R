test_that("check_name works", {
  expect_no_condition(check_name("codec_traffic"))
  check_name("CoDEC_traffic") |>
    expect_equal("`name` must be all lowercase")
  check_name(1) |>
    expect_equal("`name` must be a character string")
  check_name("census data") |>
    expect_equal("`name` must not contain spaces")
  check_name("census+data") |>
    expect_equal("`name` must only contain a-z, 0-9, -, _, .")
})

test_that("check census tract id", {
  skip_if_no_cincy()
  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    check_census_tract_id() |>
    expect_no_condition()

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::rename(census_tract_id = census_tract_id_2010) |>
    check_census_tract_id() |>
    expect_identical("must contain one census tract id column called census_tract_id_2000, census_tract_id_2010, or census_tract_id_2020")

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::slice_head(n = 5) |>
    check_census_tract_id() |>
    expect_identical("the census tract id column, census_tract_id_2010, does not contain every census tract in `cincy::tract_tigris_2010`")
})

test_that("check date", {
  tibble::tibble(year = 2010) |>
    check_date() |>
    expect_no_condition()

  tibble::tibble(year = 2010, month = 11) |>
    check_date() |>
    expect_no_condition()

  tibble::tibble(year = "foofy") |>
    check_date() |>
    expect_equal("the 'year' field must only contain integer years between 1970 and 2099")

  tibble::tibble(year = 2000, month = 14) |>
    check_date() |>
    expect_equal("the 'month' field  must only contain integer values 1-12")
})

test_that("codec S7 object", {
  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::mutate(year = 2021) |>
    as_codec_dpkg(name = "foofy", version = "0.0.0") |>
    expect_no_condition()

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    as_codec_dpkg(name = "foofy", version = "0.0.0") |>
    expect_error("contain a 'year' column")

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::slice_head(n = 5) |>
    dplyr::mutate(year = 2021) |>
    as_codec_dpkg(name = "foofy", version = "0.0.0") |>
    expect_error("does not contain")
})
