test_that("codec_read uses bundled CoDEC data by default", {
  board <- codec_board()

  expect_s3_class(board, "pins_board_folder")
  expect_true("acs_measures" %in% codec_list(board))

  d <- codec_read("acs_measures", board = board)
  raw <- pins::pin_read(board, "acs_measures")
  md <- pins::pin_meta(board, "acs_measures")

  expect_s3_class(d, "codec_tbl")
  expect_identical(attr(d, "name"), "acs_measures")
  expect_gt(nrow(d), 0L)
  expect_equal(
    d,
    as_codec_tbl(raw, name = md$name, description = md$description)
  )
})

test_that("codec_read can include source geography without interpolation", {
  d <- codec_read("acs_measures", include_geography = TRUE)
  expected <- codec_as_sf(codec_read("acs_measures"))

  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_true("census_tract_id_2020" %in% names(d))
  expect_equal(d, expected)
})

test_that("codec_read can interpolate while reading", {
  target <- cincy_neighborhood_geo()
  d <- codec_read(
    "acs_measures",
    to = target,
    weights = "homes"
  )
  expected <- codec_interpolate(
    codec_read("acs_measures"),
    to = target,
    weights = "homes"
  )

  expect_s3_class(d, "tbl_df")
  expect_false(inherits(d, "sf"))
  expect_equal(nrow(d), 51L)
  expect_true("geoid" %in% names(d))
  expect_equal(d, expected)
})

test_that("codec_read can interpolate and keep target geography", {
  target <- cincy_zcta_geo("2020")
  d <- codec_read(
    "acs_measures",
    to = target,
    weights = "pop",
    include_geography = TRUE
  )
  expected <-
    dplyr::right_join(
      target,
      codec_interpolate(
        codec_read("acs_measures"),
        to = target,
        weights = "pop"
      ),
      by = "geoid"
    )

  expect_s3_class(d, c("sf", "tbl_df"))
  expect_s3_class(d$s2_geography, "sfc")
  expect_equal(nrow(d), 54L)
  expect_true("geoid" %in% names(d))
  expect_equal(d, expected)
})

test_that("codec_read ignores weights unless interpolation is requested", {
  d_default <- codec_read("acs_measures")
  d_weighted <- codec_read("acs_measures", weights = "area")

  expect_s3_class(d_weighted, "codec_tbl")
  expect_equal(d_default, d_weighted)
})

test_that("codec_read can read an older online CoDEC board", {
  skip_if_offline()

  d <- codec_read("traffic", board = codec_board("v3.0.0"))

  expect_s3_class(d, "codec_tbl")
  expect_identical(attr(d, "name"), "traffic")
  expect_gt(nrow(d), 0L)
})
