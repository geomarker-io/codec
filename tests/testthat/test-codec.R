test_that("codec_tdr", {
  expect_snapshot(codec_tdr())
})

test_that("read_codec", {
  skip("read_codec still experimental; don't download files for test for now")
  skip_on_ci()
  d <- read_codec("hh_acs_measures")
  expect_identical(attr(d, "name"), "hh_acs_measures")
  expect_true("census_tract_id" %in% names(d))
  expect_true("census_tract_vintage" %in% names(d))
  expect_true("year" %in% names(d))
  fs::dir_delete("codec-data")
  })
