check_tdr_name <- function(name) {
  # metadata has a name field
  if(is.null(name)) stop("Metadata must have a field called 'name'.")
  # name is a character string
  if(!is.character(name)) stop("'name' must be character string.")
  # name does not have uppercase letters
  if(stringr::str_detect(name, "[[:upper:]]")) stop("'name' must be all lowercase.")
  # name does not have spaces
  if(stringr::str_detect(name, " ")) stop("'name' must not contain spaces.")
  # nonalphanumeric characters are either -, _, or .
  if(!all(stringr::str_detect(unlist(stringr::str_extract_all(name, "[^[:alnum:]]")), "[_.-]"))) {
    stop("Accepted non-alphanumeric characters for 'name' are '-', '_', and '.'")
  }
}


