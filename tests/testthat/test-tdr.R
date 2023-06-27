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
  expect_identical(attr(d_tdr$id, "description"), "a unique identifier")
})

test_that("glimpse_attr()", {
  expect_equal(glimpse_attr(d_attrs)$name, c("name", "path", "title", "homepage"))
  expect_equal(nrow(glimpse_attr(data.frame())), 0)
})

test_that("glimpse_schema()", {
  expect_equal(glimpse_schema(d_attrs)$name, c("id", "date", "measure", "rating", "ranking", "impt"))
  expect_equal(glimpse_schema(d_attrs)$description, c("a unique identifier", NA, NA, NA, NA, NA))
  expect_equal(glimpse_schema(d_attrs)$title, c("Identifier", NA, NA, NA, NA, NA))
  expect_equal(glimpse_schema(d_attrs)$type, c("string", "date", "number", "string", "integer", "boolean"))
  expect_equal(glimpse_schema(d_attrs)$constraints, c(NA, NA, NA, "good, better, best", NA, NA))
})

test_that("glimpse_tdr()", {
  the_glimpse <- glimpse_tdr(d_attrs)
  expect_equal(length(the_glimpse), 2)
  expect_equal(names(the_glimpse), c("attributes", "schema"))
  expect_equal(the_glimpse$attributes$name, c("name", "path", "title", "homepage"))
  expect_equal(the_glimpse$schema$name, c("id", "date", "measure", "rating", "ranking", "impt"))
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

test_that("codec_data() with interpolation retains metadata", {
  landcover_attrs <-
    codec_data("hamilton_landcover") |>
    glimpse_attr()
  expect_equal(
    landcover_attrs,
    codec_data("hamilton_landcover", interpolate_to = cincy::zcta_tigris_2010) |>
      glimpse_attr()
    )
})

# test_that("codec_data() with interpolation retains schema metadata", {
#   landcover_attrs <-
#     codec_data("hamilton_landcover") |>
#     glimpse_schema()
#   expect_equal(
#     landcover_attrs,
#     codec_data("hamilton_landcover", interpolate_to = cincy::zcta_tigris_2010) |>
#       glimpse_schema()
#   )
# })

