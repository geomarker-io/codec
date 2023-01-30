test_that("check for census tract identifier", {

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
