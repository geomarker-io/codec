prop_label <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (length(value) != 1L) "must be length 1"
  }
)

prop_label_maybe <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (length(value) > 1L) "must be length 1 (or 0)"
  }
)

new_codec_dpkg <- S7::new_class(
  name = "codec_dpkg",
  parent = S7::class_data.frame,
  package = "codec",
  properties = list(
    name = prop_label,
    version = prop_label,
    homepage = prop_label_maybe
  ),
  validator = function(self) {
    if (length(self@homepage) == 1 && !grepl("^((http|ftp)s?|sftp)://", self@homepage)) {
      "homepage must be a valid http, https, or sftp URL"
    }
    if (length(self@version) != 1) "version must be length 1"
    if (!is.package_version(as.package_version(self@version))) "version should be coercable with `as.package_version()`"
    check_date(S7::S7_data(self))
    check_census_tract_id(S7::S7_data(self))
    check_name(self@name)
    # check URL?
  }
)

#' check_codec_dpkg
#'
#' Check a data package according to the CoDEC specifications:
#'
#' 1. The data includes a [census tract](https://www2.census.gov/geo/pdfs/education/CensusTracts.pdf) identifier column (i.e., `census_tract_id_{2000,2010,2020}`). The column must contain 11-digit [GEOID](https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html) identifiers for every census tract in Hamilton County, OH.
#' 2. The data includes a year column (`year`), an integer year representing the vintage of the data (e.g. "2020"). The data can optionally include a month column (`month`), an integer month of the year. Data must be structured in a tidy format such that each row is an observation for a specific census tract at a specific year (and month).
#'
#' Use this function to check the structure of a CoDEC data package before writing it to disk or uploading it to a repository.
#' @param x data.frame or tibble meeting CoDEC data specifications above
#' @param name a lowercase character string consisting of only `a-z`, `0-9`, `-`, `_`, or `.`
#' @param version a character string representing a [semantic version](https://datapackage.org/recipes/data-package-version/) (e.g., "0.2.1")
#' @param homepage (optional) a URL that links to a webpage with code or descriptions related to creation of the data package
#' @returns x (coerced to a tibble)
#' @export
as_codec_dpkg <- function(x, name, version, homepage = character()) {
  new_codec_dpkg(x, name = name, version = version, homepage = homepage) |>
    tibble::as_tibble()
}
