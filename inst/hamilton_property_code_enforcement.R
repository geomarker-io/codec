devtools::load_all()
name <- "hamilton_property_code_enforcement"
version <- "0.1.3"

rd <-
  fr::read_fr_tdr(glue::glue(
    "https://github.com/geomarker-io/",
    "{name}/releases/download/{version}/"
  ))

md <- as.list(rd)

d <-
  rd |>
  tibble::as_tibble() |>
  dplyr::mutate(year = 2022)

d_tdr <-
  purrr::reduce2(
    names(d),
    md$schema$fields[names(d)],
    \(accum, xx, yy) fr::update_field(x = accum, field = xx, !!!yy),
    .init = fr::as_fr_tdr(
      d,
      name = name,
      version = version,
      title = "Hamilton County Property Code Enforcement",
      description = "Number of property code enforcements per household by census tract",
      homepage = "https://geomarker.io/hamilton_property_code_enforcement"
    )
  ) |>
  fr::update_field("year", title = "Year", description = "data year")

fr::write_fr_tdr(d_tdr, fs::path_package("codec", "codec_data"))
check_codec_tdr_csv(fs::path_package("codec", "codec_data", name))
