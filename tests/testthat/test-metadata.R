test_that("can create metadata from attributes", {
  make_my_mtcars() |>
    make_metadata_from_attr() |>
    expect_snapshot()
})

test_that("can create metadata file from attributes", {
  on.exit(fs::file_delete("my_mtcars_tabular-data-resource.yaml"))
  make_my_mtcars() |>
    write_metadata("my_mtcars_tabular-data-resource.yaml")
  expect_snapshot_file("my_mtcars_tabular-data-resource.yaml")
})



