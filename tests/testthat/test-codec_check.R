test_that("check census tract id", {
  skip_if_no_cincy()
  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    check_census_tract_id() |>
    expect_no_condition()

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::rename(census_tract_id = census_tract_id_2010) |>
    check_census_tract_id() |>
    expect_identical("must contain one census tract id column called census_tract_id_2010 or census_tract_id_2020")

  readRDS(testthat::test_path("drivetime", "drivetime.rds")) |>
    dplyr::slice_head(n = 5) |>
    check_census_tract_id() |>
    expect_identical("the census tract id column, census_tract_id_2010, does not contain every census tract for that vintage; Check for missing census tract observations and check that you are using the correct vintage.")
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

test_that("as_codec_dpkg works", {
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

test_that("is_codec_dpkg works", {
  expect_false(is_codec_dpkg(mtcars))
  expect_true(is_codec_dpkg(get_codec_dpkg("drivetime-v0.2.2")))
})
