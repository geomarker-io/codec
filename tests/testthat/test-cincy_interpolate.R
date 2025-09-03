test_that("codec_as_sf", {
  d <- codec_as_sf(codec_read("acs_measures"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true("census_tract_id_2020" %in% names(d))
})

test_that("cincy_block_weights", {
  d <- cincy_block_weights()
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_equal(names(d), c("pop", "homes", "area", "s2_geography"))
})

test_that("codec_interpolate", {
  codec_interpolate(
    codec_read("acs_measures"),
    to = cincy_zcta_geo("2020"),
    weights = "pop"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(54L)

  codec_interpolate(
    codec_read("acs_measures"),
    to = cincy_neighborhood_geo(),
    weights = "homes"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(51L)

  codec_interpolate(
    codec_read("acs_measures"),
    to = cincy_census_geo("bg", "2020"),
    weights = "area"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(678L)

  codec_interpolate(
    codec_read("acs_measures"),
    to = cincy_census_geo("tract", "2019"),
    weights = "area"
  ) |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(222L)
})
