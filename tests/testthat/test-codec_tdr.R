test_that("codec_data works", {

  codec_d <- codec_data("tract_indices")
  expect_equal(nrow(codec_d), 222)
  expect_identical(glimpse_attr(codec_d)$value[[2]], "tract_indices")

  codec_data("not_real") |>
    expect_error("not found in installed codec_data")

})

test_that("codec_data works with interpolation", {
  skip_if_no_cincy()

  # geometry = TRUE
  codec_d <- codec_data("hamilton_landcover",
                        geography = cincy::zcta_tigris_2010,
                        geometry = TRUE)
  expect_equal(nrow(codec_d), 54)
  expect_equal(ncol(codec_d), 7)
  expect_identical(glimpse_attr(codec_d)$value[[2]], "hamilton_landcover")
  expect_s3_class(codec_d, "sf")

  # geometry = FALSE
  codec_d <- codec_data("hamilton_landcover",
                        geography = cincy::zcta_tigris_2010,
                        geometry = FALSE)
  
  expect_equal(nrow(codec_d), 54)
  expect_equal(ncol(codec_d), 6)
  expect_identical(glimpse_attr(codec_d)$value[[2]], "hamilton_landcover")
  expect_error(expect_s3_class(codec_d, "sf"))
})


