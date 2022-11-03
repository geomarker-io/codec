#' codec_tdr
#' 
#' Defines the list of names (and descriptions) of valid CODEC *property*, *schema*, and *fields* descriptors
#' @return a list of named character vectors, one for each of "property", "schema", and "fields"
#' @examples
#' codec_tdr()$property
#' codec_tdr()$schema
#' codec_tdr()$fields
#' @export
codec_tdr <- function() {
  list(
    property = c(
      "profile" = "profile of this descriptor (always set to `tabular-data-resource` here)",
      "name" = "an identifier string composed of lower case alphanumeric characters, `_`, `-`, and `.`",
      "path" = "location of data associated with resource as a [POSIX path](https://en.wikipedia.org/wiki/Path_%28computing%29#POSIX_pathname_definition) relative to the `tabular-data-resource.yaml` file or a fully qualified URL",
      "version" = "semantic [version](https://specs.frictionlessdata.io/patterns/#data-package-version) of the data resource",
      "title" = "human-friendly title of the resource",
      "homepage" = "homepage on the web related to the data; ideally a code repository used to create the data",
      "description" = "additional notes about the resource",
      "schema" = "a list object containing items in schema",
      "_s3VersionId" = "the VersionId of the file stored on AWS S3; not user-generated"
    ),
    schema = c(
      "fields" = "a list object as long as the number of fields each containing the items in fields",
      "primaryKey" = "a field or set of fields that uniquely identifies each row",
      "foreignKey" = "a field or set of fields that connect to a separate table",
      "missingValues" = "a list object, one for each column, containing the following"
    ),
    fields = c(
      "name" = "machine-friendly name of field/column; must be identical to name of column in data CSV file",
      "title" = "human-friendly name of field/column",
      "description" = "any additional notes about the field/column",
      "type" = "[Frictionless type](https://specs.frictionlessdata.io/table-schema/#types-and-formats) of the field/column (e.g., string, number, boolean)",
      "constraints" = "[Frictionless constraints](https://specs.frictionlessdata.io/table-schema/#constraints), including `enum`, an array of possible values or factor levels"
    )
  )
}

#' read a CODEC tabular-data-resource from the cloud
#'
#' If not available locally, the CODEC tabular-data-resource will be
#' downloaded from `s3://codec-data/`.
#' @param name name of CODEC tabular data resource
#' @param force ignore existing data and redownload data?
#' @return tibble with added tabular-data-resource attributes
#' @export
read_codec <- function(name, force = FALSE) {
  if (force | (!fs::dir_exists(fs::path("codec-data", name)))) {
    cli::cli_alert_info("{name} not found locally; downloading...")
    fs::dir_create(fs::path("codec-data", name))
    utils::download.file(glue::glue("https://codec-data.s3.amazonaws.com/{name}/tabular-data-resource.yaml"),
                         fs::path(getwd(), "codec-data", name, "tabular-data-resource.yaml"))
    utils::download.file(glue::glue("https://codec-data.s3.amazonaws.com/{name}/{name}.csv"),
                         fs::path(getwd(), "codec-data", name, glue::glue("{name}.csv")))
    cli::cli_alert_success("downloaded CODEC tabular-data-resource")
  }
  read_tdr_csv(fs::path(getwd(), "codec-data", name))
}

#' experimental!: release a CODEC tdr
#'
#' If releasing to AWS S3, the CSV data file will be uploaded first and its
#' VersionId from AWS S3 metadata will be added to the properties of the uploaded
#' tabular-data-resource.yaml file
#' @param .x a CODEC tdr
#' @param version the version number to be used for the release
#' @param s3 logical; create and upload CODEC tdr to s3://codec-data/ ?
#' @param gh logical; create and upload CODEC tdr to a new GitHub release?
release_codec_tdr <- function(.x, version, s3 = FALSE, gh = FALSE) {

  stopifnot(!is.null(version))
  stopifnot(s3 | gh)

  # TODO check schema and data

  tdr_name <- attr(.x, "name")
  d <- add_attrs(.x, version = version)
  tf_csv <- tempfile(tdr_name, fileext = ".csv")
  tf_tdr <- tempfile("tabular-data-resource", fileext = ".yaml")
  readr::write_csv(d, tf_csv)

  # TODO: make someone answer a question to be sure they are going to upload XXXX as version XXX to S3

  if (s3) {
  # upload CSV file
  system2("aws",
          c("s3", "cp",
            "--acl public-read",
            glue::glue("--metadata version={version}"),
            "--only-show-errors",
            tf_csv,
            glue::glue("s3://codec-data/{tdr_name}/{tdr_name}.csv")))

  # get s3 version id and save with tdr.yaml file
  Sys.sleep(1)
  s3_version_id <- 
    system2("aws", c("s3api", "list-object-versions",
                     "--bucket codec-data",
                     glue::glue("--prefix {tdr_name}/{tdr_name}.csv")),
            stdout = TRUE, stderr = TRUE) |>
    paste(collapse = "\n") |>
    jsonlite::fromJSON() |>
    purrr::pluck("Versions") |>
    dplyr::filter(IsLatest) |>
    dplyr::pull(VersionId)

  d |>
    add_attrs(id = s3_version_id) |>
    write_tdr(tf_tdr)

    # upload tdr.yaml file
    system2("aws",
            c("s3", "cp",
              "--acl public-read",
              glue::glue("--metadata version={version}"),
              "--only-show-errors",
              tf_tdr,
              glue::glue("s3://codec-data/{tdr_name}/tabular-data-resource.yaml")))
  }

  if (gh & FALSE) {
    ## gh_rl <-
    ##   gh::gh(glue::glue("POST /repos/geomarker-io/{tdr_name}/releases"),
    ##          name = glue::glue("{tdr_name} {version}"),
    ##          tag_name = glue::glue("v{version}"),
    ##          target_commitish = gert::git_info()$commit,
    ##          draft = TRUE)
    ## upload_url <- gsub("\\{[^\\}]+}", "", gh_rl$upload_url)
    ## gh::gh(glue::glue("POST {upload_url}?name=tdr.yaml"),
    ##        readBin(tf_tdr, raw(), file.info(tf_tdr)$size),
    ##        .send_headers = c("Content-Type" = "text/yaml"))
    ## Sys.sleep(1)
    ## browseURL(gh_rl$html_url)
  }
}

## # get AWS S3 metdata on object
## system2("aws", c("s3api", "head-object",
##                  "--bucket codec-data",
##                  glue::glue("--key {tdr_name}/{tdr_name}.csv")),
##         stdout = TRUE, stderr = TRUE) |>
##   paste(collapse = "\n") |>
##   jsonlite::fromJSON()

## versions <-
##   system2("aws", c("s3api", "list-object-versions",
##                    "--bucket codec-data",
##                    glue::glue("--prefix {tdr_name}/{tdr_name}.csv")),
##           stdout = TRUE, stderr = TRUE) |>
##   paste(collapse = "\n") |>
##   jsonlite::fromJSON()
# could write code to get all `VersionID`s of a CODEC tdr and
# then lookup associated versions in the metadata of each file
# in order to download older versions
