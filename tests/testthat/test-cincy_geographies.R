test_that("cincy zcta works", {
  skip_on_ci()
  d <- cincy_zcta_geo("2024")
  expect_equal(names(d), c("geoid", "s2_geography"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_equal(nrow(d), 55L)
  expect_equal(round(sum(s2::s2_area(d$s2_geography)), -3), 1083637000L)
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))

  d <- cincy_zcta_geo("2019")
  expect_equal(names(d), c("geoid", "s2_geography"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_equal(nrow(d), 54L)
  expect_equal(round(sum(s2::s2_area(d$s2_geography)), -3), 1089190000L)
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy tracts and block groups can be downloaded and work", {

  withr::local_envvar(list(R_USER_DIR = tempdir()))
  skip_on_ci()

  d <- cincy_census_geo("tract", "2024")
  expect_equal(nrow(d), 226)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))

  d <- cincy_census_geo("tract", "2019")
  expect_equal(nrow(d), 222)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))

  d <- cincy_census_geo("bg", "2024")
  expect_equal(nrow(d), 678)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))

  d <- cincy_census_geo("bg", "2019")
  expect_equal(nrow(d), 697)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("geography functions will error for years outside of 2013 - 2024", {
  cincy_census_geo("tract", "2012") |>
    expect_error("must be one of")

  cincy_county_geo("2012") |>
    expect_error("must be one of")
})

test_that("cincy county", {
  d <- cincy_county_geo("2024")
  expect_equal(length(d), 1)
  expect_s3_class(d, c("s2_geography", "wk_vctr"))
  expect_equal(round(s2::s2_area(d), -3), 1067800000)
})

test_that("cincy city", {
  skip_on_ci()
  d <- cincy_city_geo()
  expect_equal(length(d), 1)
  expect_s3_class(d, c("s2_geography", "wk_vctr"))
  expect_equal(round(s2::s2_area(d), -3), 206352000L)
})

test_that("cincy neighborhoods", {
  skip_on_ci()
  d <- cincy_neighborhood_geo("statistical_neighborhood_approximations")
  expect_equal(nrow(d), 50)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))

  d <- cincy_neighborhood_geo("community_council")
  expect_equal(nrow(d), 75)
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$geoid))
})

test_that("cincy addresses", {
  skip_on_ci()
  d <- cincy_addr_geo()
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true(is.character(d$cagis_address))
  expect_true(is.character(d$cagis_parcel_id))
  expect_true(is.logical(d$cagis_is_condo))
})
