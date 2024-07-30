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

## as_codec_dpkg <- function(x, name, version, homepage = character()
