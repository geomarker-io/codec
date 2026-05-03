#' CoDEC online data catalog
#'
#' The CoDEC data catalog is shipped with the package and older versions
#' can be read from GitHub alongside the source code for this package.
#' - Use `codec_read()` as a shortcut to read a CoDEC table
#'   into R as a `codec_tbl` object (see `?as_codec_tbl`)
#' - Use `codec_list()` as a shortcut to list available CoDEC table pins
#' - `codec_board()` can be used to create a pin board object
#'   (see `?pins::pins`) based on a specific version of the codec package
#'
#' The pin for each CoDEC table has versions (see `?pins::pin_versions`),
#' but `codec_board()` can be used to specify a state of the online data
#' catalog based on the version of the codec package. (See examples)
#' @export
#' @param name the name of the CoDEC table in the CoDEC data catalog.
#' @param board a pins board object; create with `codec_board()` to read
#' from the bundled catalog or earlier versions of the catalog, or to
#' change the caching behavior of the pins package
#' @param to optional target geography for interpolation; supply the output
#' of `cincy_census_geo()`, `cincy_neighborhood_geo()`, or `cincy_zcta_geo()`
#' to interpolate the table while reading
#' @param weights which census block-level weights to use when `to` is
#' supplied; passed to `codec_interpolate()`
#' @param include_geography logical; include the `s2_geography` column in
#' the result? Defaults to `FALSE`
#' @return For `codec_read()`, a `codec_tbl` by default, or an interpolated
#' tibble / simple-features tibble when `to` or `include_geography` is used
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
#' # interpolate while reading
#' codec_read("acs_measures", to = cincy_neighborhood_geo())
#' codec_read("acs_measures", include_geography = TRUE)
#'
#' # inspect the bundled board or read from an older online version
#' codec_board() |>
#'   pins::pin_versions("crime")
#' codec_board("v3.0.0-rc1") |>
#'   pins::pin_versions("crime")
codec_read <- function(
  name,
  board = codec_board(),
  to = NULL,
  weights = c("pop", "homes", "area"),
  include_geography = FALSE
) {
  stopifnot(length(name) == 1, inherits(name, "character"))
  stopifnot(
    inherits(board, "pins_board_url") | inherits(board, "pins_board_folder")
  )
  stopifnot(is.logical(include_geography), length(include_geography) == 1)
  codec_pins <- pins::pin_list(board)
  stopifnot(name %in% codec_pins)
  d <- pins::pin_read(board, name)
  md <- pins::pin_meta(board, name)
  d <- as_codec_tbl(d, name = md$name, description = md$description)

  if (is.null(to)) {
    if (isTRUE(include_geography)) {
      return(codec_as_sf(d))
    }
    return(d)
  }

  out <- codec_interpolate(codec_as_sf(d), to = to, weights = weights)
  if (isTRUE(include_geography)) {
    return(dplyr::right_join(to, out, by = "geoid"))
  }
  out
}

#' @rdname codec_read
#' @return For `codec_list()`, a character vector CoDEC table names
#' @export
codec_list <- function(board = codec_board()) {
  pins::pin_list(board)
}

#' @rdname codec_read
#' @return For `codec_board()`, a pins_board object
#' @param version specify a version of the online data catalog using a
#' commit SHA, tag, or branch of geomarker-io/codec; uses the bundled board
#' for the installed package version by default
#' @inheritParams pins::board_url
#' @export
codec_board <- function(
  version = paste0("v", utils::packageVersion("codec")),
  cache = NULL,
  use_cache_on_failure = rlang::is_interactive(),
  headers = NULL
) {
  current_version <- paste0("v", utils::packageVersion("codec"))
  if (identical(as.character(version), current_version)) {
    return(codec_board_local())
  }
  codec_board_remote(
    version = version,
    cache = cache,
    use_cache_on_failure = use_cache_on_failure,
    headers = headers
  )
}

codec_board_remote <- function(
  version,
  cache = NULL,
  use_cache_on_failure = rlang::is_interactive(),
  headers = NULL
) {
  board_paths <- c("inst/board", "assets/data")
  for (board_path in board_paths) {
    board <- tryCatch(
      {
        codec_board_url <-
          glue::glue(
            "https://raw.githubusercontent.com/",
            "geomarker-io/codec/{ version }/{ board_path }/"
          )
        pins::board_url(
          as.character(codec_board_url),
          cache = cache,
          use_cache_on_failure = use_cache_on_failure,
          headers = headers
        )
      },
      error = function(...) NULL
    )
    if (!is.null(board)) {
      return(board)
    }
  }
  rlang::abort(glue::glue(
    "No CoDEC board found online for version `{version}`."
  ))
}

codec_board_local <- function() {
  installed_path <- system.file("board", package = "codec")
  if (nzchar(installed_path)) {
    return(pins::board_folder(installed_path))
  }

  repo_path <- here::here("inst/board")
  if (file.exists(file.path(repo_path, "_pins.yaml"))) {
    return(pins::board_folder(repo_path))
  }

  rlang::abort("Local CoDEC board not found.")
}

#' @rdname write_codec_pin
codec_board_local_dev <- function() {
  codec_board_local()
}


#' Write a CoDEC data table to the local CoDEC board directory
#'
#' This function is used by developers creating and updating CoDEC data
#' tables.
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
