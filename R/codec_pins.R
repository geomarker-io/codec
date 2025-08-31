#' CoDEC pin board hosted on GitHub
#'
#' A shortcut to create a pins board using the url of the CoDEC pins
#' board manifest file on GitHub.
#' Use this function with the pins package to get CoDEC data tables
#' from the online CoDEC data catalog.
#'
#' @export
#' @return a pins_board object
#' @inheritParams pins::board_url
#' @examples
#' library(pins)
#' pin_list(codec_board())
#' pin_read(codec_board(), "property_code_enforcements")
#' pin_meta(codec_board(), "property_code_enforcements")
codec_board <- function(cache, use_cache_on_failure, headers)
  pins::board_url(
    "https://raw.githubusercontent.com/geomarker-io/codec/refs/heads/pins/assets/data/"
  )

#' Read a CoDEC table from the online data catalog
#'
#' This function uses the pins package to read from the online CoDEC data catalog,
#' ensuring that metadata is present in the returned codec_tbl object.
#'
#' Read from the online data without installing this package directly
#' using the pins package with `codec_board()`.
#' @param name name of CoDEC table in the online CoDEC data catalog
#' @return a codec_tbl object (see `as_codec_tbl()`)
#' @export
#' @examples
#' d <- codec_read("traffic")
#' head(d)
#' attr(d, "title")
#' attr(d, "description")
codec_read <- function(name) {
  stopifnot(length(name) == 1, inherits(name, "character"))
  codec_pins <- pins::pin_list(codec_board())
  stopifnot(name %in% codec_pins)
  d <- pins::pin_read(codec_board(), name)
  md <- pins::pin_meta(codec_board(), name)
  as_codec_tbl(d, name = md$name, description = md$description)
}


#' @rdname write_codec_pin
codec_board_local_dev <- function() {
  pins::board_folder("assets/data")
}


#' Write a CoDEC data table to the local CoDEC board directory
#'
#' This function is used by developers creating and updating CoDEC data tables.
#' @param x a codec_tbl object created with as_codec_tbl()
#' @keywords internal
write_codec_pin <- function(x) {
  if (!inherits(x, "codec_tbl")) {
    rlang::abort("x must be a codec_tbl object created with as_codec_tbl()")
  }
  pins::pin_write(
    board = codec_board_local_dev(),
    x = x,
    type = "json",
    name = attr(x, "name"),
    title = attr(x, "title"),
    metadata = list(sesh = sesh()),
    urls = c("https://github.com/geomarker-io/codec"),
    versioned = TRUE,
    description = attr(x, "description"),
    force_identical_write = TRUE
  )

  pins::write_board_manifest(codec_board_local_dev())

  rlang::inform(c(
    " ",
    "Board manifest updated; versions include:",
    " ",
    knitr::kable(pins::pin_versions(codec_board_local_dev(), attr(x, "name")))
  ))
}

sesh <- function() {
  list(
    r_version = R.Version()$version.string,
    platform = R.Version()$platform,
    date = Sys.Date(),
    loaded_packages = sapply(sessionInfo()$otherPkgs, function(pkg) pkg$Version)
  )
}
