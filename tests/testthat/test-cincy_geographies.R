test_that("cincy zcta works packaged", {
  d <- cincy_zcta_geo("2020")
  expect_equal(names(d), c("geoid", "s2_geography"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_equal(nrow(d), 55L)
  expect_equal(round(sum(s2::s2_area(d$s2_geography)), -3), 1083637000L)
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy zcta works unpackaged", {
  skip_on_ci()
  d <- cincy_zcta_geo("2020", packaged = FALSE)
  expect_equal(names(d), c("geoid", "s2_geography"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_equal(nrow(d), 55L)
  expect_equal(round(sum(s2::s2_area(d$s2_geography)), -3), 1083637000L)
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy census geo can utilize packaged data", {
  withr::local_envvar(list(R_USER_CACHE_DIR = tempdir()))
  d <- cincy_census_geo("tract", "2020", packaged = TRUE)
  expect_equal(nrow(d), 226)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy census geo can downloaded from source", {
  withr::local_envvar(list(R_USER_CACHE_DIR = tempdir()))
  d <- cincy_census_geo("tract", "2020", packaged = FALSE)
  expect_equal(nrow(d), 226)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("geography functions will error for years outside of 2013 - 2024", {
  cincy_census_geo("tract", "2012") |>
    expect_error("must be one of")
})

test_that("cincy neighborhoods packaged", {
  d <- cincy_neighborhood_geo("statistical_neighborhood_approximations")
  expect_equal(nrow(d), 50)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy neighborhoods not packaged", {
  skip_on_ci()
  d <- cincy_neighborhood_geo("community_council", packaged = FALSE)
  expect_equal(nrow(d), 75)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy addresses packaged", {
  d <- cincy_addr_geo()
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$cagis_address))
  expect_true(is.character(d$cagis_parcel_id))
  expect_true(is.logical(d$cagis_is_condo))
})

test_that("cincy addresses not packaged", {
  skip_on_ci()
  withr::local_envvar(list(R_USER_CACHE_DIR = tempdir()))
  d <- cincy_addr_geo(packaged = FALSE)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$cagis_address))
  expect_true(is.character(d$cagis_parcel_id))
  expect_true(is.logical(d$cagis_is_condo))
})
