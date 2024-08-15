#' Put a dpkg into the public CoDEC S3 bucket
#'
#' The [AWS CLI](https://aws.amazon.com/cli/) tool must be installed and authenticated to
#' write to `s3://geomarker-io/codec_data`.
#' The resulting data package will be available publicly.
#' @param x a data package (`dpkg`) object
#' @returns character string URI of uploaded resource
#' @export
#' @examples
#' \dontrun{
#' # use aws command line to login interactively via profile sso account"
#' system2("aws", c("sso", "login", "--profile", "geomarker-io"))
#' # make sure to set AWS_PROFILE so the AWS CLI tool knows to use it by default"
#' Sys.setenv("AWS_PROFILE" = "geomarker-io")
#' }
codec_dpkg_s3_put <- function(x) {
  if (!inherits(x, "dpkg")) rlang::abort("x must be a dpkg object")
  the_file <- dpkg::write_dpkg(as_codec_dpkg(x), tempdir())
  out <-
    system2(
      "aws",
      c(
        "s3", "cp", the_file,
        glue::glue("s3://geomarker-io/codec_data/{attr(x, 'name')}-v{attr(x, 'version')}.parquet"),
        "--acl public-read"
      )
    )
  if (!out == 0L) rlang::abort("aws s3 cp command failed")
  return(as.character(glue::glue("s3://geomarker-io/codec_data/{attr(x, 'name')}-v{attr(x, 'version')}.parquet")))
}

#' Read a dpkg from the public CoDEC repository into R
#'
#' Public data packages are downloaded from `s3://geomarker-io/codec_data`.
#' `dpkg::stow()` is used to cache a local copy in the user's data directory so that
#' it is available later without having to redownload it.
#' @param codec_dpkg name of CoDEC dpkg
#' @param overwrite logical; re-download the remote file even though
#' a local file with the same name exists?
#' @returns for `get_codec_dkg()`, an data package (`dpkg::as_dpkg()`) object; for
#' `stow_codec_dpkg()`, the path to the downloaded data package parquet file
#' @export
#' @examples
#' get_codec_dpkg("drivetime-v0.2.2")
#' 
#' stow_codec_dpkg("drivetime-v0.2.2")
get_codec_dpkg <- function(codec_dpkg, overwrite = FALSE) {
  dpkg::read_dpkg(stow_codec_dpkg(codec_dpkg = codec_dpkg, overwrite = overwrite))
}

#' Stow a dpkg from the public CoDEC S3 bucket
#' @rdname get_codec_dpkg
#' @export
stow_codec_dpkg <- function(codec_dpkg, overwrite = FALSE) {
  dpkg::stow_url(
    paste0("https://geomarker-io.s3.us-east-2.amazonaws.com/codec_data/", codec_dpkg, ".parquet"),
    overwrite = overwrite
  )
}

list_codec_dpkg <- function() {
  system2("aws", c("s3", "ls", "s3://geomarker-io/codec_data/"))
}
