test_that("can create data resource metadata from attributes", {
  my_mtcars |>
    make_tdr_from_attr() |>
    expect_snapshot()
  classy |>
    make_tdr_from_attr() |>
    expect_snapshot()
})

test_that("can save tabular-data-resource file", {
  classy |>
    add_type_attrs() |>
    add_attrs(
      name = "The Classiest Data Set",
      year = "2022",
      description = "A toy data frame with many different column classes."
    ) |>
  save_tdr()
  expect_snapshot_file("tabular-data-resource.yaml")
  fs::file_delete("tabular-data-resource.yaml")
})

test_that("get_descriptors works", {
  my_mtcars |>
    get_descriptors() |>
    expect_identical(
      tibble::tibble(
        name = c("name"),
        value = c("Motor Trend Cars")
      )
    )
  my_mtcars |>
    get_descriptors(codec = FALSE) |>
    expect_snapshot()
})
