my_mtcars <-
  mtcars |>
  add_attrs(name = "Motor Trend Cars", year = "1974") |>
  add_type_attrs() |>
  add_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon")

classy <-
  tibble::tibble(
    id = c("A01", "A02", "A03"),
    date = as.Date(c("2022-07-25", "2018-07-10", "2013-08-15")),
    measure = c(12.8, 13.9, 15.6),
    rating = factor(c("good", "best", "best"), levels = c("good", "better", "best")),
    ranking = as.integer(c(14, 17, 19)),
    awesomeness = TRUE,
    datetime = as.POSIXct(Sys.time(), "America/New_York") + c(1:3),
    timesince = datetime - Sys.time()
  )

d <-
  tibble::tibble(
    id = c("A01", "A02", "A03"),
    date = as.Date(c("2022-07-25", "2018-07-10", "2013-08-15")),
    measure = c(12.8, 13.9, 15.6),
    rating = factor(c("good", "best", "best"), levels = c("good", "better", "best")),
    ranking = as.integer(c(14, 17, 19)),
    impt = c(FALSE, TRUE, TRUE)
  ) 

d_attrs <-
  d |>
  add_attrs(
    name = "example",
    title = "Example Data Set",
    path = "./inst/example_data.csv",
    license = "MIT"
  ) |>
  add_type_attrs()
