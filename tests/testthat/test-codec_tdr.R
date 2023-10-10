test_that("codec_data works", {

  codec_d <- codec_data("hamilton_drivetime")
  expect_equal(nrow(as.data.frame(codec_d)), 222)
  expect_identical(attr(codec_d, "name"), "hamilton_drivetime")

  codec_data("not_real") |>
    expect_error("not found in installed codec_data")

})
