test_that("can translate between tdr and attributes", {
  expect_snapshot(make_tdr_from_attr(d))

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
      get_schema(classy_attrs) |>
        purrr::map(knitr::kable)
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
  save_tdr(d)
  expect_snapshot_file("tabular-data-resource.yaml")
  fs::file_delete("tabular-data-resource.yaml")
})

