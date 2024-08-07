#' Put a dpkg into the public CoDEC S3 bucket
#'
#' Ensure paws can connect via the usual methods;
#' see the developer guide at https://www.paws-r-sdk.com/developer_guide/credentials/.
#' The resulting data package will be available publicly.
#' @param x a data package (`dpkg::dpkg`) object
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
codec_dpkg_s3_put <- function(x) {
  if (!inherits(x, "dpkg::dpkg")) rlang::abort("x must be a dpkg object")

  written_path <- dpkg::write_dpkg(x, tempdir())
  paws.storage::s3()$put_object(
    Body = written_path,
    Bucket = "geomarker-io",
    Key = fs::path("codec_data", glue::glue("{x@name}-v{x@version}"), ext = "parquet"),
    ACL = "public-read"
  )
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
stow_codec_dpkg <- function(codec_dpkg) {
  resp <-
    paws.storage::s3(credentials = list(anonymous = TRUE))$get_object(
      Bucket = "geomarker-io",
      Key = glue::glue("codec_data/{codec_dpkg}.parquet")
    )
  out_path <- dpkg::stow_path(paste0(codec_dpkg, ".parquet"))
  writeBin(resp$Body, con = out_path)
  return(out_path)
}
