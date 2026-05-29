test_that("explorer field metadata does not download source geographies", {
  testthat::local_mocked_bindings(
    tiger_download = function(...) {
      stop("field metadata should not download source geographies", call. = FALSE)
    }
  )

  fields <- codec:::codec_latest_annual_fields()

  expect_s3_class(fields, "tbl_df")
  expect_gt(nrow(fields), 0L)
  expect_true(all(c("field", "source_table", "source_title") %in% names(fields)))
})
