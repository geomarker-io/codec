test_that("set_attrs() works", {
  my_mtcars <- set_attrs(mtcars, name = "Motor Trend Cars", year = "1974")
  expect_equal(attr(my_mtcars, "name"), "Motor Trend Cars")
  expect_equal(attr(my_mtcars, "year"), "1974")
})

test_that("set_col_attrs() works with one col", {
  my_mtcars <- set_col_attrs(mtcars, mpg, name = "MPG", description = "Miles Per Gallon")
  expect_equal(attr(my_mtcars$mpg, "name"), "MPG")
  expect_equal(attr(my_mtcars$mpg, "description"), "Miles Per Gallon")
})

# test_that("set_col_attrs() works with multiple cols at once", {
#   my_mtcars <- set_col_attrs(mtcars, c(mpg, cyl), description = "car stuff", foo = "bar")
#   expect_equal(attr(my_mtcars$mpg, "description"), "car stuff")
#   expect_equal(attr(my_mtcars$cyl, "description"), "car stuff")
# })

test_that("add_type_attrs() works", {
  classy <- tibble::tibble(
    id = c("A01", "A02", "A03"),
    date = as.Date(c("2022-07-25", "2018-07-10", "2013-08-15")),
    measure = c(12.8, 13.9, 15.6),
    rating = factor(c("good", "best", "best"), levels = c("good", "better", "best")),
    ranking = as.integer(c(14, 17, 19)),
    awesomeness = TRUE,
    datetime = as.POSIXct(Sys.time(), "America/New_York") + c(1:3),
    timesince = datetime - Sys.time()
  )

  classy_attrs <- add_type_attrs(classy)

  expect_equal(
    sapply(classy_attrs, attr, "type"),
    c(
      id = "string", date = "date",
      measure = "number", rating = "string",
      ranking = "integer", awesomeness = "boolean",
      datetime = "datetime", timesince = "number"
    )
  )

  # doesn't add enum constraints for non-factor column
  expect_equal(attr(classy_attrs$id, "constraints"), NULL)

  # does add enum constraints for factor column
  expect_equal(
    attr(classy_attrs$rating, "constraints"),
    list(enum = c("good", "better", "best"))
  )
})
