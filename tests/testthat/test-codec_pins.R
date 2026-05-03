test_that("codec_read uses bundled CoDEC data by default", {
  board <- codec_board()

  expect_s3_class(board, "pins_board_folder")
  expect_true("acs_measures" %in% codec_list(board))

  d <- codec_read("acs_measures", board = board)

  expect_s3_class(d, "codec_tbl")
  expect_identical(attr(d, "name"), "acs_measures")
  expect_gt(nrow(d), 0L)
})

test_that("codec_read can read an older online CoDEC board", {
  skip_if_offline()

  d <- codec_read("traffic", board = codec_board("v3.0.0"))

  expect_s3_class(d, "codec_tbl")
  expect_identical(attr(d, "name"), "traffic")
  expect_gt(nrow(d), 0L)
})
