skip_if_no_cincy <- function() {
  cincy_installed <- requireNamespace("cincy", quietly = TRUE)
  if (!cincy_installed) skip("cincy package not available for interpolation testing")
  }
