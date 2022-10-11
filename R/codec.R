#' valid names for CODEC descriptor, schema, and fields
#' @return a list of character vectors, one for each of "descriptor", "schema", and "fields"
#' @examples
#' codec_names()
#' @export
codec_names <- function() {
  list(
    descriptor = c(
      "profile",
      "name",
      "path",
      "version",
      "title",
      "homepage",
      "description",
      "schema",
      "_s3VersionId"
    ),
    schema = c(
      "fields",
      "primaryKey",
      "foreignKey",
      "missingValues"
    ),
    fields = c(
      "name",
      "title",
      "description",
      "type",
      "constraints"
    )
  )
}
