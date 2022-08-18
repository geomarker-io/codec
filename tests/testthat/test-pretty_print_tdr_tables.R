test_that("get_descriptors() and get_schema()", {
  testthat::skip("are get_* functions even necessary?")

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
        "name", "rating",
        "type", "string",
        "constraints", c("good", "better", "best")
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
