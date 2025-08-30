test_that("codec_as_sf", {
  skip("skipping for now")
  d <- codec_as_sf(get_codec_dpkg("acs_measures-v0.1.0"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true("geoid" %in% names(d))
})

test_that("cincy_block_weights", {
  skip("skipping for now")
  skip_on_ci()
  d <- cincy_block_weights()
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_equal(names(d), c("pop", "homes", "area", "s2_geography"))
})

test_that("codec_interpolate", {
  skip("skipping for now")
  skip_on_ci()

  codec_interpolate(
    get_codec_dpkg("acs_measures-v0.1.0"),
    to = cincy_zcta_geo("2020"),
    weights = "pop"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(54L)

  codec_interpolate(
    get_codec_dpkg("acs_measures-v0.1.0"),
    to = cincy_neighborhood_geo(),
    weights = "homes"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(51L)

  codec_interpolate(
    get_codec_dpkg("acs_measures-v0.1.0"),
    to = cincy_census_geo("bg", "2020"),
    weights = "area"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(678L)

  codec_interpolate(
    get_codec_dpkg("acs_measures-v0.1.0"),
    to = cincy_census_geo("tract", "2019"),
    weights = "area"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(222L)
})
