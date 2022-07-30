test_that("can create data resource metadata from attributes", {
  my_mtcars |>
    make_data_resource_from_attr() |>
    expect_snapshot()
  classy |>
    make_data_resource_from_attr() |>
    expect_snapshot()
})

test_that("can create data resource metadata file from attributes", {
  on.exit(fs::file_delete("my_mtcars_tabular-data-resource.yaml"))
  my_mtcars |>
    write_metadata("my_mtcars_tabular-data-resource.yaml")
  expect_snapshot_file("my_mtcars_tabular-data-resource.yaml")
  on.exit(fs::file_delete("classy_tabular-data-resource.yaml"))
  classy |>
    write_metadata("classy_tabular-data-resource.yaml")
  expect_snapshot_file("classy_tabular-data-resource.yaml")
})

test_that("get_descriptors works", {
  my_mtcars |>
    get_descriptors() |>
    expect_identical(
      tibble::tibble(
        name = c("name", "year", "class"),
        value = c("Motor Trend Cars", "1974", "data.frame")
      )
    )
})

