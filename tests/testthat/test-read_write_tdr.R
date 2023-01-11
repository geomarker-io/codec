test_that("write_tdr", {
  write_tdr(d_attrs, "test.yaml")
  expect_snapshot_file("test.yaml")
  fs::file_delete("test.yaml")
})

test_that("read_tdr_csv", {
  skip_on_ci()
  # reads file where all levels of factors are not present in the data
  d_tdr <- read_tdr_csv(test_path("d", "tabular-data-resource.yaml"))
  expect_identical(levels(d_tdr$rating), c("best", "good", "better"))
  expect_identical(attr(d_tdr, "name"), "example")
  expect_identical(attr(d_tdr, "path"), "d.csv")
  # reads simple file with no factors
  d_tdr_simple <- read_tdr_csv(test_path("simple_data", "tabular-data-resource.yaml"))
  expect_identical(names(d_tdr_simple), c("a", "b", "c"))
  expect_identical(attr(d_tdr_simple, "name"), "example")
  expect_identical(attr(d_tdr_simple, "path"), "simple_data.csv")
  expect_identical(attr(d_tdr_simple, "title"), "Simple Example Data Set")
  # and reads schema
  expect_identical(attr(d_tdr_simple$a, "name"), "a")
  expect_identical(attr(d_tdr_simple$a, "type"), "number")
})

test_that("read_tdr_csv works with URL", {
  "https://github.com/geomarker-io/CODECtools/blob/main/tests/testthat/simple_data/simple_data.csv"
  lndcvr <-
    read_tdr_csv( "https://github.com/geomarker-io/hamilton_landcover/releases/download/v0.1.0/tabular-data-resource.yaml")
  expect_identical(attr(lndcvr, "name"), "hamilton_landcover")
  expect_identical(attr(lndcvr, "homepage"), "https://geomarker.io/hamilton_landcover")
  expect_identical(attr(lndcvr$census_tract_id, "name"), "census_tract_id")
})

test_that("write_tdr_csv", {
  skip_on_os("windows")
  write_tdr_csv(d_attrs, test_path())
  expect_snapshot_file(fs::path(test_path(), "example", "example.csv"))
  expect_snapshot_file(fs::path(
    test_path(),
    "example",
    "tabular-data-resource.yaml"
  ))
  fs::dir_delete(fs::path(test_path(), "example"))
})
