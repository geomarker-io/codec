#' if `to` is NULL, just add geography to `from`
#' @param from a CoDEC data package
#' @param to ; if NULL, tract geographies are returned without data interpolation
#' @details Tract identifers do not change across decennial censuses, but the digital representation of their boundaries
#' may be improved over time.  Here, data packages using 2010 tract identifers use the TIGER/Line 2019 tract shapefiles
#' and data packages using 2020 tract identifiers use the TIGER/Line 2020 tract shapefiles
#' @returns a simple features object with a geographic identifier column (`geoid`)
#' and a geometry column (`s2_geography`) in addition to the (interpolated) columns in `from`
#' @examples
#' codec_interpolate(get_codec_dpkg("property_code_enforcements-v0.2.0"))
codec_interpolate <- function(from, to = NULL) {
  if (!is_codec_dpkg(from)) rlang::abort("from must be a CoDEC data package")
  ## d <- tibble::as_tibble(from)
  md <- dpkg::dpkg_meta(from)
  gd_vintage <- ifelse(any(grepl("census_tract_id_2010", names(from), fixed = TRUE)), "2019", "2020")
  gd <-
    cincy_census_geo("tract", gd_vintage) |>
    dplyr::left_join(from, by = c("geoid" = paste0("census_tract_id_", ifelse(gd_vintage == "2019", "2010", "2020"))))
  if (is.null(to)) {
    return(gd)
  }
}

