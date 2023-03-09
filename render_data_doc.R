# make this a package function? how to render from inst folder?

render_data_doc = function(data_name, data_version) {
  rmarkdown::render(
    fs::path(fs::path_package("CODECtools"), "data_doc_template.Rmd"),
    params = list(
      data_name = data_name,
      data_version = data_version
    ),
    output_file = glue::glue("doc-{data_name}-{data_version}.html")
  )
}

render_data_doc(data_name = "hamilton_landcover",
                data_version = "0.1.0")

render_data_doc(data_name = "hamilton_traffic",
                data_version = "0.1.0")
