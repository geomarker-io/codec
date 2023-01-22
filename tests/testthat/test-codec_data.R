test_that("check for census tract identifier", {

  d_tdr <-
    read_tdr_csv("https://github.com/geomarker-io/hamilton_drivetime/releases/download/v0.1.0") |>
    dplyr::rename(census_tract_id_2010 = census_tract_id) |>
    CODECtools::add_type_attrs()

  expect_equal(check_census_tract_id(d_tdr), d_tdr)

  expect_error({
    d_tdr |>
      dplyr::select(-census_tract_id_2010) |>
      check_census_tract_id()
  },
  regexp = "must contain a census tract id column called")

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
  },
  regexp = "cannot find matching CSV data file")

  # duplicated names
  expect_error({
    check_files(test_path("d_dups"))
  },
  regexp = "Names must be unique")

  # these characteristics will be tested by being able to read the CSV (with fixed locale)
  # TODO add test for non-UTF-8 files
  # TODO add test for wrong line ending type

  expect_error({
    check_files(test_path("sus"))
  },
  regexp = "read without error")



 })
