devtools::load_all()

fs::path_package("codec") |>
  fs::dir_ls(glob = "*hamilton_*.R") |>
  purrr::walk(callr::rscript, .progress = interactive())
