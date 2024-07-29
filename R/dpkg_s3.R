#' Put a dpkg into the CoDEC S3 bucket under `/data`
#'
#' Ensure {paws} can connect via the usual methods.
#' See the developer guide at https://www.paws-r-sdk.com/developer_guide/credentials/.
#' @param x path to datapackage.yaml file
#' @returns NULL
#' @examples
#' \dontrun{
#' # use credentials in .env file to authenticate with aws
#' library(dotenv)
#' # alternatively, use aws command line to login interactively via profile sso account
#' system2("aws", c("sso", "login", "--profile", "geomarker-io"))
#' # make sure to set AWS_PROFILE so {paws} can authenticate
#' Sys.setenv("AWS_PROFILE" = "geomarker-io")
#' }
dpkg_s3_put <- function(x) {
  dpkg_folder <- fs::path_dir(x)
  dpkg_name <- yaml::read_yaml(x)$resources$resource$name
  fls <- fs::dir_ls(dpkg_folder)
  for (f in fls) {
    paws::s3()$put_object(
      Body = f,
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{dpkg_name}/", fs::path_file(f))
    )
  }
  return(invisible(NULL))
}

#' Get a dpkg file from the CoDEC S3 bucket under `/data`
#' @param x name of CoDEC dpkg
#' @param dir directory in which to write the data package
#' @returns path to downloaded datapackage directory
#' @examples
#' dpkg_s3_get("aadt") |>
#'   dpkg_read()
dpkg_s3_get <- function(x = "aadt", dir = tools::R_user_dir("codec", "data")) {
  dpkg_folder <- fs::path(dir, x)
  fs::dir_create(dpkg_folder)
  resp <-
    paws.storage::s3()$get_object(
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{x}/datapackage.yaml")
    )
  writeBin(resp$Body, con = fs::path(dpkg_folder, "datapackage.yaml"))
  resp2 <-
    paws.storage::s3()$get_object(
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{x}/{x}.rds")
    )
  writeBin(resp2$Body, con = fs::path(dpkg_folder, x, ext = "rds"))
  return(dpkg_folder)
  ## resp2$VersionId
}
