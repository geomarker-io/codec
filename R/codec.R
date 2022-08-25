#' valid names for CODEC descriptor, schema, and fields
#' @return a list of character vectors, one for each of "descriptor", "schema", and "fields"
#' @examples
#' codec_names()
#' @export
codec_names <- function() {
  list(
    descriptor = c(
      "name",
      "path",
      "title",
      "description",
      "url",
      "license",
      "schema"
    ),
    schema = c(
      "fields",
      "missingValues",
      "primaryKey",
      "foreignKey"
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
