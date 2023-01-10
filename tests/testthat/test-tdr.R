test_that("make_tdr_from_attr", {
  d_tdr <- make_tdr_from_attr(d_attrs)
  expect_identical(names(d_tdr), c("name", "path", "title", "homepage", "schema"))
  expect_identical(d_tdr$schema$fields$rating$constraints$enum, c("good", "better", "best"))
})

test_that("add_attr_from_tdr", {
  d_tdr <- add_attr_from_tdr(.x = d, tdr = make_tdr_from_attr(d_attrs, codec = TRUE), codec = TRUE)
  expect_identical(names(d_tdr), names(d_attrs))
  expect_identical(attr(d_tdr, "name"), attr(d_attrs, "name"))
  expect_identical(attr(d_tdr, "path"), attr(d_attrs, "path"))
  expect_identical(attr(d_tdr, "title"), attr(d_attrs, "title"))
  expect_identical(attr(d_tdr, "homepage"), attr(d_attrs, "homepage"))
  expect_identical(attr(d_tdr$rating, "constraints"), attr(d_attrs$rating, "constraints"))
})

test_that("write_tdr", {
  write_tdr(d_attrs, "test.yaml")
  expect_snapshot_file("test.yaml")
  fs::file_delete("test.yaml")
})

test_that("read_tdr_csv", {
  skip_on_ci()
  # reads file where all levels of factors are not present in the data
  d_tdr <- read_tdr_csv(test_path("tabular-data-resource.yaml"))
  expect_identical(levels(d_tdr$rating), c("best", "good", "better"))
  expect_identical(attr(d_tdr, "name"), "example")
  expect_identical(attr(d_tdr, "path"), "d.csv")
  # reads simple file with no factors
  d_tdr_simple <- read_tdr_csv(test_path("simple_data", "tabular-data-resource.yaml"))
  expect_identical(names(d_tdr_simple), c("a", "b", "c"))
  expect_identical(attr(d_tdr_simple, "name"), "example")
  expect_identical(attr(d_tdr_simple, "path"), "simple_data.csv")
  expect_identical(attr(d_tdr_simple, "title"), "Simple Example Data Set")
})

test_that("read_tdr_csv works with URL", {
  lndcvr <-
    read_tdr_csv( "https://github.com/geomarker-io/hamilton_landcover/releases/download/v0.1.0/tabular-data-resource.yaml")
  expect_identical(names(lndcvr), c("census_tract_id", "pct_green_2019", "pct_impervious_2019", "pct_treecanopy_2016", "evi_2018"))
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

test_that("glimpse_attr()", {
  expect_equal(glimpse_attr(d_attrs)$name, c("name", "path", "title", "homepage"))
  expect_equal(nrow(glimpse_attr(data.frame())), 0)
})


test_that("glimpse_schema()", {
  expect_equal(glimpse_schema(d_attrs)$name, c("id", "date", "measure", "rating", "ranking", "impt"))
  expect_equal(glimpse_schema(d_attrs)$type, c("string", "date", "number", "string", "integer", "boolean"))
  expect_equal(glimpse_schema(d_attrs)$constraints, c(NA, NA, NA, "good, better, best", NA, NA))
})

test_that("glimpse_schema() works without constraints", {
  mtcars_schema <-
    mtcars |>
    add_type_attrs() |>
    glimpse_schema()
    expect_equal(
      mtcars_schema,
      tibble::tibble(
        name = names(mtcars),
        type = "number"
      )
    )
})
