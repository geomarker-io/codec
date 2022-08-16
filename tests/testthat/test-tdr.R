test_that("can translate between tdr and attributes", {
  expect_snapshot(make_tdr_from_attr(d_attrs))

})

test_that("get_descriptors() and get_schema()", {

  classy_attrs <-
    classy |>
    add_type_attrs() |>
    add_attrs(
      name = "classy",
      title = "The Classiest Data Set",
      year = "2022",
      description = "A toy data frame with many different column classes."
    )

    expect_equal(
      get_descriptors(classy_attrs),
      tibble::tribble(
        ~ name, ~ value,
        "name", "classy",
        "title", "The Classiest Data Set",
        "description", "A toy data frame with many different column classes."
      )
    )

    expect_snapshot(
      get_descriptors(classy_attrs, codec = FALSE) |>
        knitr::kable()
    )

    expect_equal(
      get_col_descriptors(classy_attrs$rating),
      tibble::tribble(
        ~ name, ~ value,
        "constraints", "c(\"good\", \"better\", \"best\")",
        "name", "rating",
        "type", "string"
      )
    )

    expect_snapshot(
      get_schema(classy_attrs, bind = FALSE) |>
        purrr::map(knitr::kable)
    )

    expect_snapshot(
      get_schema(classy_attrs)
    )

})

test_that("can create data resource metadata from attributes", {
  my_mtcars |>
    make_tdr_from_attr() |>
    expect_snapshot()
  classy |>
    make_tdr_from_attr() |>
    expect_snapshot()
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

test_that("add_attr_from_tdr", {
  my_tdr <-
    test_path("tabular-data-resource.yaml") |>
    read_tdr()
  d_my_attrs <- add_attr_from_tdr(d, my_tdr)
  expect_identical(get_descriptors(d_my_attrs), get_descriptors(d_attrs))
})

test_that("read_tdr_csv", {
  d_tdr <- read_tdr_csv(test_path("tabular-data-resource.yaml"))
  expect_identical(levels(d_tdr$rating), c("good", "best", "better"))
  expect_snapshot(get_schema(d_tdr))
  expect_snapshot(get_descriptors(d_tdr))
})
