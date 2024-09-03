#' Put a dpkg into the public CoDEC S3 bucket
#'
#' The [AWS CLI](https://aws.amazon.com/cli/) tool must be installed and authenticated to
#' write to `s3://geomarker-io/codec_data`.
#' The resulting data package will be available publicly.
#' @param x a data package (`dpkg`) object, ideally created with `as_codec_dpkg()` to ensure
#' the data meets CoDEC specifications
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
  the_file <- dpkg::write_dpkg(x, tempdir())
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

