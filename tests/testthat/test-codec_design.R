test_that("codec colors", {

  expect_identical(codec_colors(),
                   c("dark blue" = "#396175",
                     "darkish blue" = "#58829C",
                     "light blue" = "#8CB4C3",
                     "grey blue" = "#CBD6D5",
                     "white" = "#F6EDDE",
                     "pink" = "#EACEC5",
                     "orange" = "#E49865",
                     "red" = "#C28273"))

  expect_identical(codec_colors(2), c("darkish blue" = "#58829C"))

  expect_identical(codec_colors("darkish blue"), c("darkish blue" = "#58829C"))

  expect_identical(codec_colors(c(2, 4)),
                   c("darkish blue" = "#58829C",
                     "grey blue" = "#CBD6D5" ))

  expect_identical(codec_colors(c("darkish blue", "grey blue")),
                   c("darkish blue" = "#58829C",
                     "grey blue" = "#CBD6D5" ))

})
