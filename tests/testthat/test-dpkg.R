test_that("can read codec dpkg from disk", {
  dpkg_read(testthat::test_path("drivetime")) |>
    expect_identical(readRDS(testthat::test_path("drivetime", "drivetime.rds")))
})

test_that("can write dpkg to disk", {
  dpkg_read(testthat::test_path("drivetime")) |>
    dpkg_write(
      name = "drivetime",
      version = "2.0.0",
      dir = tempdir(),
      readme_file = fs::path_package("codec", "drivetime", "README.md")
    )

  dpkg_fl_info <- fs::dir_info(fs::path(tempdir(), "drivetime"))
  expect_identical(nrow(dpkg_fl_info), 2L)
  dpkg_fl_info$path |>
    fs::path_file() |>
    expect_identical(c("datapackage.yaml", "drivetime.rds"))
})
