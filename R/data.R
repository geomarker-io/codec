#' CoDEC latest annual
#'
#' CoDEC data is harmonized as the most recently available annual (or annual average of monthly) values
#' at the census tract 2020 geography.
#' The year represents the year this table was assembled and mainly used as a placeholder to this object
#' can be used with functions that take a codec_tbl object (see `?as_codec_tbl`).
#' See the description metadata for the actual latest years used for each CoDEC table.
#' @examples
#' glue::glue(attr(codec_latest_annual, "description"))
#' tibble::glimpse(codec_latest_annual)
"codec_latest_annual"
