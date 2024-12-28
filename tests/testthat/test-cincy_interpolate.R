test_that("codec_as_sf", {
  d <- codec_as_sf(get_codec_dpkg("acs_measures-v0.1.0"))
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true("geoid" %in% names(d))
})

test_that("cincy_block_weights", {
  d <- cincy_block_weights()
  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_equal(names(d), c("pop", "homes", "area", "s2_geography"))
})

test_that("codec_interpolate", {

  codec_interpolate(get_codec_dpkg("acs_measures-v0.1.0"), to = "zcta", weights = "pop") |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(54L)

  codec_interpolate(get_codec_dpkg("acs_measures-v0.1.0"), to = "neigh", weights = "homes") |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(51L)

  codec_interpolate(get_codec_dpkg("acs_measures-v0.1.0"), to = "bg", weights = "area") |>
    expect_s3_class("tbl_df") |>
    nrow() |>
    expect_equal(678L)

})
