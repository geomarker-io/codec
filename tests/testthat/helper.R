make_my_mtcars <- function() {
  mtcars |>
    set_attrs(name = "Motor Trend Cars", year = "1974") |>
    add_type_attrs() |>
    set_col_attrs(mpg, name = "MPG", description = "Miles Per Gallon")
}
