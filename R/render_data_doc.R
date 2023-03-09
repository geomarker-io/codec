#' render interactive data documentation
#'
#' @param data_name name of tabular data resource
#' @param data_version tabular data resource version
#' @return html file
render_data_doc = function(data_name, data_version) {
  # read in data from virtual location (GitHub release, for now)
  d <- read_tdr_csv(glue::glue("https://github.com/geomarker-io/{data_name}/releases/download/v{data_version}")) |>
    dplyr::rename(census_tract_id_2010 = census_tract_id) |>
    dplyr::mutate(year = as.integer(2019)) |>
    CODECtools::add_col_attrs(year, name = "year", title = "Year", description = "data year") |>
    CODECtools::add_type_attrs()

  # data-specific functions to pass checks

  # write to tmp folder for check
  CODECtools::write_tdr_csv(d, dir = "tmp")
  check_codec_tdr_csv(fs::dir_ls("tmp"))
  fs::dir_delete("tmp")

  # write to data folder (if check passes)
  write_tdr_csv(d, "codec_data/")

  # render documentation
  rmarkdown::render(
    fs::path(fs::path_package("CODECtools"), "data_doc_template.Rmd"),
    params = list(
      data = d
    ),
    output_file = glue::glue("{data_name}.html")
  )
}

# render_data_doc(data_name = "hamilton_landcover", data_version = "0.1.0")
#
# render_data_doc(data_name = "hamilton_traffic", data_version = "0.1.0")
