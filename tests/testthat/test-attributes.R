test_that("add_attrs() works", {
  my_mtcars <- add_attrs(mtcars, title = "Motor Trend Cars", year = "1974")
  expect_equal(attr(my_mtcars, "title"), "Motor Trend Cars")
  expect_equal(attr(my_mtcars, "year"), "1974")
})

test_that("add_col_attrs() works with one col", {
  my_mtcars <- add_col_attrs(mtcars, mpg, title = "MPG", description = "Miles Per Gallon")
  expect_equal(attr(my_mtcars$mpg, "title"), "MPG")
  expect_equal(attr(my_mtcars$mpg, "description"), "Miles Per Gallon")
})

# test_that("add_col_attrs() works with multiple cols at once", {
#   my_mtcars <- add_col_attrs(mtcars, c(mpg, cyl), description = "car stuff", foo = "bar")
#   expect_equal(attr(my_mtcars$mpg, "description"), "car stuff")
#   expect_equal(attr(my_mtcars$cyl, "description"), "car stuff")
# })

test_that("add_type_attrs() works", {

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
    sapply(classy_attrs, attr, "type"),
    c(
      id = "string", date = "date",
      measure = "number", rating = "string",
      ranking = "integer", awesomeness = "boolean",
      datetime = "datetime", timesince = "number"
    )
  )

  # sets name correctly
  expect_equal(attr(classy_attrs$measure, "name"), "measure")

  # doesn't add enum constraints for non-factor column
  expect_equal(attr(classy_attrs$id, "constraints"), NULL)

  # does add enum constraints for factor column
  expect_equal(
    attr(classy_attrs$rating, "constraints"),
    list(enum = c("good", "better", "best"))
  )
})
