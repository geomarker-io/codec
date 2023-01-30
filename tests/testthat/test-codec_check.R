test_that("codec_tdr", {
  expect_snapshot(codec_tdr())
})

test_that("check codec_tdr_csv", {
  expect_identical(read_tdr_csv(test_path("hamilton_poverty_2020")),
                   check_codec_tdr_csv(test_path("hamilton_poverty_2020")))
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

test_that("check tdr name", {
  tdr <- list()
  tdr$name <- "census_data"
  expect_no_condition(check_tdr_name(tdr$name))
  expect_error(check_tdr_name(tdr$wrong))
  expect_error(check_tdr_name(1))
  expect_error(check_tdr_name("CensusData"))
  expect_error(check_tdr_name("census data"))
  expect_error(check_tdr_name("census+data"))
})

test_that("check tdr path", {

  })

test_that("check census tract id", {

  d_tdr <- read_tdr_csv(test_path("hamilton_poverty_2020"))

  expect_equal(check_census_tract_id(d_tdr), d_tdr)

  expect_error({
    d_tdr |>
      dplyr::select(-census_tract_id) |>
      check_census_tract_id()
  },
  regexp = "must contain a census tract id column called")

  expect_error({
    d_tdr |>
      dplyr::mutate(census_tract_id_2010 = census_tract_id) |>
      check_census_tract_id()
  },
  regexp = "must contain only one census tract id column")

  expect_error({
    d_tdr |>
      dplyr::select(-census_tract_vintage) |>
      check_census_tract_id()
  },
  regexp = "census_tract_vintage column must exist")

  expect_error({
    d_tdr |>
      dplyr::mutate(census_tract_vintage = "2009") |>
      check_census_tract_id()
  },
  regexp = "census_tract_vintage must be")

  expect_error({
    d_tdr |>
      dplyr::mutate(census_tract_vintage = 2009) |>
      check_census_tract_id()
  },
  regexp = "census_tract_vintage must be")

  expect_error({
    d_tdr |>
      dplyr::mutate(census_tract_vintage = c(d_tdr$census_tract_vintage[-1], "2010")) |>
      check_census_tract_id()
  },
  regexp = "census_tract_vintage column must have only one unique value")

  skip_on_ci()
  expect_error({
    d_tdr |>
      dplyr::slice(-1) |>
      check_census_tract_id()
  },
  regexp = "does not contain every census tract")
})

test_that("check files", {

  expect_silent(check_files(test_path("d")))

  # missing folder
  expect_error({
    check_files(test_path("dd"))
  },
  regexp = "cannot find")

  # missing file
  expect_error({
    check_files(test_path("d_empty"))
  }, regexp = "cannot find")

  # duplicated names
  expect_error({
    check_files(test_path("d_dups"))
  },
  regexp = "Names must be unique")

  # these characteristics will be tested by being able to read the CSV (with fixed locale)
  # - non-UTF-8 files
  # - wrong line ending type

  expect_error({
    check_files(test_path("sus"))
  },
  regexp = "read without error")

  # impossible to enforce missing value representation?

 })
