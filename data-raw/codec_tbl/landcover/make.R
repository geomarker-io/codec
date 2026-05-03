devtools::load_all()
codec_name <- "landcover"

d <- cincy_census_geo("tract", "2020")

get_dv_url <- function(
  persistent_id,
  filename = NULL,
  version = "latest",
  server_url = "https://dataverse.harvard.edu"
) {
  stopifnot(is.character(persistent_id))
  stopifnot(is.character(version))
  stopifnot(is.character(server_url))
  if (!substr(persistent_id, 1, 4) == "doi:") {
    stop("`persistent_id` must begin with 'doi:'", call. = FALSE)
  }
  if (version == "latest") {
    version <- ":latest"
  }

  req <-
    httr2::request(server_url) |>
    httr2::req_user_agent("pcog (https://github.com/geomarker-io/pcog)") |>
    httr2::req_url_path_append(
      "api",
      "datasets",
      ":persistentId",
      "versions",
      version
    ) |>
    httr2::req_url_query("persistentId" = persistent_id) |>
    httr2::req_error(
      is_error = function(resp) httr2::resp_status(resp) != 200,
      body = function(resp) {
        glue::glue(
          "version {version} of {persistent_id} not found at {server_url}"
        )
      }
    )
  resp <- httr2::req_perform(req)

  if (httr2::resp_content_type(resp) == "application/xhtml+xml") {
    stop(
      "This dataverse is returning xhtml+xml content and likey running a demo during maintainence; try again later",
      call. = FALSE
    )
  }
  the_files <-
    httr2::resp_body_json(resp)$data$files |>
    vapply(\(.) .$dataFile[["id"]], integer(1))

  names(the_files) <-
    httr2::resp_body_json(resp)$data$files |>
    vapply(\(.) .$dataFile[["filename"]], character(1))

  if (length(filename) == 1 && filename %in% names(the_files)) {
    file_id <- the_files[[filename]]
  } else {
    message(
      "available files for ",
      persistent_id,
      " include: \n  ",
      paste(names(the_files), collapse = "\n  ")
    )
    if (length(filename) == 0) {
      stop("no filename requested", call. = FALSE)
    }
    stop("filename ", filename, " not found.", call. = FALSE)
  }

  cog_url <- glue::glue(
    "https://dataverse.harvard.edu/api/access/datafile/{file_id}"
  )
  return(as.character(cog_url))
}


for (nlcd_year in as.character(2023:2020)) {
  message("extracting data for ", nlcd_year)
  r <-
    get_dv_url(
      persistent_id = "doi:10.7910/DVN/KXETFC",
      filename = glue::glue("Annual_NLCD_FctImp_{nlcd_year}_CU_C1V0_COG.tif"),
      version = "latest"
    ) |>
    terra::rast(vsi = TRUE)
  d[[glue::glue("mean_pct_impervious_{nlcd_year}")]] <- terra::extract(
    r,
    terra::vect(d),
    fun = "mean",
    ID = FALSE
  )[[1]]
}

out <- d |>
  sf::st_drop_geometry() |>
  tidyr::pivot_longer(
    -geoid,
    names_to = "year",
    names_prefix = "mean_pct_impervious_",
    values_to = "mean_pct_impervious"
  ) |>
  dplyr::rename(census_tract_id_2020 = geoid)

out$year <- as.integer(out$year)

out |>
  as_codec_tbl(
    name = codec_name,
    description = paste(
      readLines(fs::path_package(
        "codec",
        "data-raw",
        "codec_tbl",
        codec_name,
        "README.md"
      )),
      collapse = "\n"
    )
  ) |>
  write_codec_pin()
