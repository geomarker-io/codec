test_that("codec_data works", {

  codec_d <- codec_data("tract_indices")
  expect_equal(nrow(codec_d), 222)
  expect_identical(glimpse_attr(codec_d)$value[[2]], "tract_indices")

  codec_data("not_real") |>
    expect_error("not found in installed codec_data")

})
