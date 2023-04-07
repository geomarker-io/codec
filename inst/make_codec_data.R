library(codec)

fs::path_package("CoDEC") |>
  fs::dir_ls(glob = "*codec_*.R") |>
  purrr::walk(callr::rscript, .progress = interactive())
