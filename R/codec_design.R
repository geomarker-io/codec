#' CoDEC colors
#' @param n a numeric vector of color numbers or character vector of color names; if NULL returns named vector of available colors
#' @export
#' @examples
#' plot(1:8, rep(1, 8), col = codec_colors(1:8), pch = 19, cex = 10, axes = FALSE, xlab = "", ylab = "")
#' text(1:8, rep(1.1, 8), labels = names(codec_colors()))
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
