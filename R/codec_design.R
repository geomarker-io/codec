#' CoDEC colors
#' @param n a numeric vector of color numbers or character vector of color names; if NULL returns named vector of available colors
#' @export
codec_colors <- function(n = NULL) {
  codec_col <- c(
    "dark blue" = "#396175",
    "darkish blue" = "#58829C",
    "light blue" = "#8CB4C3",
    "grey blue" = "#CBD6D5",
    "white" = "#F6EDDE",
    "pink" = "#EACEC5",
    "orange" = "#E49865",
    "red" = "#C28273"
  )
  if (!is.null(n)) return(codec_col[n])
  return(codec_col)
}
