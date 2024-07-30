test_that("can read codec dpkg from disk", {
  dpkg_read(testthat::test_path("drivetime")) |>
    expect_identical(readRDS(testthat::test_path("drivetime", "drivetime.rds")))

  dpkg_read(testthat::test_path("drivetime"), "path") |>
    expect_identical(testthat::test_path("drivetime", "drivetime.rds"))

  dpkg_read(testthat::test_path("drivetime"), "metadata") |>
    expect_identical(
      list(
        `$schema` = "https://datapackage.org/profiles/2.0/datapackage.json",
        homepage = NULL, version = list(c(0L, 2L, 0L)), created = 1722223000,
        resources = list(resource = list(
          `$schema` = "https://datapackage.org/profiles/2.0/dataresource.json",
          name = "drivetime", path = "drivetime.rds", title = "Average Drive Time to Cincinnati Children's",
          description = "# Average Drive Time to Cincinnati Children's\n\nA census tract-level measure of drive time to Cincinnati Children's Hospital Medical Center is derived using 6-minute interval drive time isochrones obtained from [openroute service](https://classic-maps.openrouteservice.org/reach?n1=38.393339&n2=-95.339355&n3=5&b=0&i=0&j1=30&j2=15&k1=en-US&k2=km).\nEach tract-level drive time is an area-weighted average of drive times.",
          format = "rds", type = NULL, bytes = 1260L, hash = "2bf9cebe5915601985c8febd3d3d37d1"
        )),
        sources = list(source = list(path = "source.R"))
      )
    )
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
