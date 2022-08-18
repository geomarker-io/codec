test_that("make_tdr_from_attr", {
  expect_snapshot(make_tdr_from_attr(d_attrs))
})

test_that("add_attr_from_tdr", {
  d_tdr <- add_attr_from_tdr(d, make_tdr_from_attr(d_attrs))
  expect_identical(attr(d_tdr, "name"), attr(d_attrs, "name"))
  expect_identical(attr(d_tdr, "path"), attr(d_attrs, "path"))
  expect_identical(attr(d_tdr, "title"), attr(d_attrs, "title"))
  expect_identical(attr(d_tdr, "license"), attr(d_attrs, "license"))
})


test_that("can save tabular-data-resource file", {
  save_tdr(d_attrs, "test.yaml")
  expect_snapshot_file("test.yaml")
  fs::file_delete("test.yaml")
})

test_that("can read example tdr file", {
  test_path("tabular-data-resource.yaml") |>
    read_tdr() |>
    expect_snapshot()
})

test_that("read_tdr_csv", {
  d_tdr <- read_tdr_csv(test_path("tabular-data-resource.yaml"))
  expect_identical(levels(d_tdr$rating), c("good", "best", "better"))
  expect_identical(attr(d_tdr, "name"), "example")
  expect_identical(attr(d_tdr, "path"), "d.csv")
})
