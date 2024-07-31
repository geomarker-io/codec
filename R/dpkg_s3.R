has_internet <- function() {
  !is.null(curl::nslookup("captive.apple.com", error = FALSE))
}

#' Put a dpkg into the public CoDEC S3 bucket
#'
#' Ensure paws can connect via the usual methods;
#' see the developer guide at https://www.paws-r-sdk.com/developer_guide/credentials/.
#' The resulting data package will be available publicly.
#' @param x path to datapackage.yaml file
#' @returns NULL
#' @export
#' @examples
#' \dontrun{
#' # use credentials in .env file to authenticate with aws
#' library(dotenv)
#' # alternatively, use aws command line to login interactively via profile sso account
#' system2("aws", c("sso", "login", "--profile", "geomarker-io"))
#' # make sure to set AWS_PROFILE so paws can authenticate
#' Sys.setenv("AWS_PROFILE" = "geomarker-io")
#' }
dpkg_s3_put <- function(x) {
  dpkg_folder <- fs::path_dir(x)
  dpkg_name <- yaml::read_yaml(x)$resources$resource$name
  fls <- fs::dir_ls(dpkg_folder)
  for (f in fls) {
    paws.storage::s3()$put_object(
      Body = f,
      Bucket = "geomarker-io",
      Key = glue::glue("codec_data/{dpkg_name}/", fs::path_file(f)),
      ACL = "public-read"
    )
  }
  return(invisible(NULL))
}

#' Get a dpkg from the public CoDEC S3 bucket
#'
#' Public data packages are downloaded from `s3://geomarker-io/codec_data`
#' @param x name of CoDEC dpkg
#' @param dir directory in which to write the data package
#' @returns path to downloaded datapackage directory
#' @export
#' @examples
#' dpkg_s3_get("drivetime") |>
#'   dpkg_read()
dpkg_s3_get <- function(x, dir = tools::R_user_dir("codec", "data")) {
  dpkg_folder <- fs::path(dir, x)
  fs::dir_create(dpkg_folder)
  resp <-
    paws.storage::s3(credentials = list(anonymous = TRUE))$get_object(
      Bucket = "geomarker-io",
      Key = glue::glue("codec_data/{x}/datapackage.yaml")
    )
  writeBin(resp$Body, con = fs::path(dpkg_folder, "datapackage.yaml"))
  resp2 <-
    paws.storage::s3(credentials = list(anonymous = TRUE))$get_object(
      Bucket = "geomarker-io",
      Key = glue::glue("codec_data/{x}/{x}.rds")
    )
  writeBin(resp2$Body, con = fs::path(dpkg_folder, x, ext = "rds"))
  return(dpkg_folder)
  ## resp2$VersionId
}
