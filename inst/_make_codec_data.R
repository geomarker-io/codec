library(codec)

fs::path_package("codec") |>
  fs::dir_ls(glob = "*hamilton_*.R") |>
  purrr::walk(callr::rscript, .progress = interactive())
