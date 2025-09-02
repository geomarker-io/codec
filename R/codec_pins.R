#' CoDEC online data catalog
#'
#' The CoDEC online data catalog is hosted on GitHub alongside
#' the source code for this package.
#' - Use `codec_read()` as a shortcut to read a CoDEC table
#' into R as a codec_tbl object (see `?as_codec_tbl`)
#' - Use `codec_list()` as a shortcut to list available CoDEC table pins
#' - `codec_board()` can be used to create a pin board
#' object (see `?pins::pins`) based on a specific version of the codec package
#'
#' The pin for each CoDEC table has versions (see `?pins::pin_versions`),
#' but `codec_board()` can be used to specify a state of the online data catalog
#' based on the version of the codec package. (See examples)
#' @export
#' @param name The name of the CoDEC table in the online CoDEC data catalog.
#' @param board a pins board object; create with `codec_board()` to read from earlier versions of the catalog
#' or to change the caching behavior of the pins package
#' @return For `codec_read()`, a codec_tbl object (see `as_codec_tbl()`)
#' @export
#' @examples
#' # list available CoDEC tables
#' codec_list()
#'
#' # read a CoDEC table and inspect its metadata
#' d <- codec_read("traffic")
#' head(d)
#' attr(d, "title")
#' message(attr(d, "description"))
#'
#' # change the defaults for codec_board() to read from older versions of the board
#' codec_board() |>
#'   pins::pin_versions("crime")
#' codec_board("v3.0.0-rc1") |>
#'   pins::pin_versions("crime")
codec_read <- function(
  name,
  board = codec_board()
) {
  stopifnot(length(name) == 1, inherits(name, "character"))
  stopifnot(inherits(board, "pins_board_url"))
  codec_pins <- pins::pin_list(board)
  stopifnot(name %in% codec_pins)
  d <- pins::pin_read(board, name)
  md <- pins::pin_meta(board, name)
  as_codec_tbl(d, name = md$name, description = md$description)
}

#' @rdname codec_read
#' @return For `codec_list()`, a character vector CoDEC table names
#' @export
codec_list <- function(board = codec_board()) {
  pins::pin_list(board)
}

#' @rdname codec_read
#' @return For `codec_board()`, a pins_board object
#' @param version specify a version of the online data catalog using a commit SHA, tag, or branch of geomarker-io/codec;
#' syncs with the version of the installed package by default
#' @inheritParams pins::board_url
#' @export
codec_board <- function(
  version = paste0("v", packageVersion("codec")),
  cache = NULL,
  use_cache_on_failure = rlang::is_interactive(),
  headers = NULL
) {
  codec_board_url <-
    glue::glue(
      "https://raw.githubusercontent.com/",
      "geomarker-io/codec/{ version }/assets/data/"
    )
  pins::board_url(as.character(codec_board_url))
}

#' @rdname write_codec_pin
codec_board_local_dev <- function() {
  pins::board_folder(here::here("assets/data"))
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

  rlang::check_installed("knitr", "pretty printing tables")

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
    loaded_packages = utils::sessionInfo()$otherPkgs |>
      lapply(\(x) x[c("Package", "Version")]) |>
      vapply(\(x) glue::glue("{x$Package}-v{x$Version}"), character(1))
  )
}
