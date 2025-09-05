test_that("as_codec_tbl works", {
  d <- readRDS(testthat::test_path("d_ex_for_codec_tbl.rds"))
  codec_check_census_tract_id(d) |>
    expect_identical(d)
  codec_check_date(d) |>
    expect_identical(d)
  d |>
    dplyr::rename(census_tract_id = census_tract_id_2020) |>
    codec_check_census_tract_id() |>
    expect_error("must contain")
  d |>
    dplyr::slice_head(n = 5) |>
    codec_check_census_tract_id() |>
    expect_error(
      "for that vintage"
    )
  d |>
    dplyr::select(-year) |>
    codec_check_date() |>
    expect_error("contain a 'year' column")
  d |>
    dplyr::mutate(year = 1899L) |>
    codec_check_date() |>
    expect_error("between 1970 and 2099")
  d |>
    dplyr::mutate(year = "2024") |>
    codec_check_date() |>
    expect_error("must be <integer>")
  d |>
    dplyr::mutate(month = "6") |>
    codec_check_date() |>
    expect_error("must be <integer>")
  d |>
    dplyr::mutate(month = 14L) |>
    codec_check_date() |>
    expect_error("values 1-12")

  as_codec_tbl(
    d,
    "foofy",
    description = "# My neato table\n This is my table."
  ) |>
    expect_silent()

  as_codec_tbl(
    d,
    "foofy/",
    description = "# My neato table\n This is my table."
  ) |>
    expect_error("alphanumeric")

  as_codec_tbl(
    d,
    "foofy",
    description = "# My neato table that I have a really long title for My neato table that I have a really long title for\n This is my table."
  ) |>
    expect_error("less than 80 characters")

  as_codec_tbl(
    d,
    "foofy",
    description = "Incorrectly formatted description string"
  ) |>
    expect_error("begin with `#`")

  # todo name is not all lowercase
  # length 0 and 2 for required / not required labels
  # label is not class character (glue::glue?)
  # test for a dataset that uses the 2010 tracts
})
