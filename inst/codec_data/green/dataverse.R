#' get_dv_url
#' get the download URL for a file hosted on a dataverse instance
#' @param persistent_id the dataset's unique, persistent identifier
#' @param filename the name of the file in the dataset
#' @param version the dataset's version
#' @param server_url the dataverse instance's URL
#' @export
get_dv_url <- function(persistent_id, filename = NULL, version = "latest", server_url = "https://dataverse.harvard.edu") {

  stopifnot(is.character(persistent_id))
  stopifnot(is.character(version))
  stopifnot(is.character(server_url))
  if (! substr(persistent_id, 1, 4) == "doi:") stop("`persistent_id` must begin with 'doi:'", call. = FALSE)
  if (version == "latest") version <- ":latest"

  req <-
    httr2::request(server_url) |>
    httr2::req_user_agent("pcog (https://github.com/geomarker-io/pcog)") |>
    httr2::req_url_path_append("api", "datasets", ":persistentId", "versions", version) |>
    httr2::req_url_query("persistentId" = persistent_id) |>
    httr2::req_error(
      is_error = function(resp) httr2::resp_status(resp) != 200,
      body = function(resp) glue::glue("version {version} of {persistent_id} not found at {server_url}")
    )
  resp <- httr2::req_perform(req)

  the_files <-
    httr2::resp_body_json(resp)$data$files |>
    vapply(\(.) .$dataFile[["id"]], integer(1))

  names(the_files) <-
    httr2::resp_body_json(resp)$data$files |>
    vapply(\(.) .$dataFile[["filename"]], character(1))

  if (length(filename) == 1 && filename %in% names(the_files)) {
    file_id <- the_files[[filename]]
  } else {
    message("available files for ", persistent_id, " include: \n  ", paste(names(the_files), collapse = "\n  "))
    if (length(filename) == 0) stop("no filename requested", call. = FALSE)
    stop("filename ", filename, " not found.", call. = FALSE)
  }

  cog_url <- glue::glue("https://dataverse.harvard.edu/api/access/datafile/{file_id}")
  return(as.character(cog_url))
}