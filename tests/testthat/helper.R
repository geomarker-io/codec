make_my_mtcars <- function() {
  mtcars |>
    add_attrs(name = "Motor Trend Cars", year = "1974") |>
    add_type_attrs() |>
    add_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon")
}
