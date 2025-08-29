#' Convert a data frame, name, and description into a CoDEC table
#'
#' **CoDEC Specifications:**
#' 1. The data must include a [census tract](https://www2.census.gov/geo/pdfs/education/CensusTracts.pdf)
#' identifier column (i.e., `census_tract_id_2010`, or `census_tract_id_2020`).
#' The column must contain 11-digit
#' [GEOID](https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html)
#' identifiers for every census tract in Hamilton County, OH.
#' 2. Data must be structured in a tidy format such that each row is an observation
#' for a specific census tract at a specific year (and month). This means that the data
#' includes a year column (`year`), an integer year representing the vintage of the data (e.g. `2021`).
#' The data can optionally include a month column (`month`), an integer month of the year.
#' 3. The name must only contain lower case alphanumeric characters, `-`, or `_`
#' 4. The description should be markdown text and the first line must contain the title of
#' the CoDEC data table as a level one header (e.g., `# My Community Data`). Titles must be less than 80 characters.
#'
#' @param x data.frame or tibble meeting CoDEC data specifications above
#' @param name name of CoDEC table
#' @param description markdown text describing the CoDEC table
#' @returns a codec_tbl object
#' @export
as_codec_tbl <- function(x, name, description = character()) {
  chk1 <- check_census_tract_id(x)
  if (!is.null(chk1)) rlang::abort(chk1)
  chk2 <- check_date(x)
  if (!is.null(chk2)) rlang::abort(chk2)
  name <- check_label(name, "name", required = TRUE)
  if (!identical(tolower(name), name)) {
    rlang::abort("name must be all lowercase")
  } else if (grepl("[^a-zA-Z0-9._-]", name)) {
    rlang::abort(
      "name must only contain alphanumeric characters, except for `-`, `_`, and `.`"
    )
  }
  check_label(description, "description", required = TRUE)
  # todo check for first character to be # for more helpful message? write tests for this
  title <- sub("^#\\s*", "", strsplit(description, "\n")[[1]][1])
  check_label(title, "title", required = TRUE)
  if (nzchar(title) > 80) {
    rlang::abort("title must be less than 80 characters in length")
  }
  out <- tibble::new_tibble(
    tibble::validate_tibble(x),
    class = "codec_tbl",
    name = name,
    title = title,
    description = description
  )
  return(out)
}

codec_board <- function() {
  pins::board_folder("assets/data")
}

#' Write a CoDEC data table to the CoDEC board directory
#'
#' This function is used by developers creating and updating CoDEC data tables.
#' @param x a codec_tbl object created with as_codec_tbl()
write_codec_pin <- function(x) {
  if (!inherits(x, "codec_tbl")) {
    rlang::abort("x must be a codec_tbl object created with as_codec_tbl()")
  }
  pins::pin_write(
    board = codec_board(),
    x = x,
    type = "json",
    name = attr(x, "name"),
    title = attr(x, "title"),
    urls = c("https://github.com/geomarker-io/codec"),
    versioned = TRUE,
    description = attr(x, "description")
  )

  pins::write_board_manifest(codec_board())

  rlang::inform(c(
    " ",
    "Board manifest updated; versions now include:",
    knitr::kable(pins::pin_versions(codec_board(), attr(x, "name"))),
    " ",
    "When you are ready, commit the changes and push to GitHub to update the data catalog."
  ))
}

check_label <- function(x, label_name, required = FALSE) {
  if (!is.character(x)) {
    rlang::abort(glue::glue(
      "`{label_name}` must be <character>, not <{class(x)}>"
    ))
  } else if (required && length(x) != 1) {
    rlang::abort(glue::glue("`{label_name}` must be length 1"))
  } else if (length(x) > 1) {
    rlang::abort(glue::glue("`{label_name}` must be length 1 (or 0)"))
  }
  return(invisible(x))
}

check_census_tract_id <- function(x) {
  census_tract_id_names <- paste0("census_tract_id", c("_2010", "_2020"))
  # has census_tract_id_{year} or census_tract_id column
  if (sum(names(x) %in% census_tract_id_names) != 1) {
    return(
      "must contain one census tract id column called census_tract_id_2010 or census_tract_id_2020"
    )
  }
  census_tract_id_name <- census_tract_id_names[
    census_tract_id_names %in% names(x)
  ]
  census_tract_id_year <- stringr::str_extract(census_tract_id_name, "[0-9]+")
  if (census_tract_id_year == "2010") {
    required_census_tract_ids <- cincy_census_geo("tract", "2019")$geoid
  } else if (census_tract_id_year == "2020") {
    required_census_tract_ids <- cincy_census_geo("tract", "2020")$geoid
  }

  if (!all(required_census_tract_ids %in% x[[census_tract_id_name]])) {
    return(glue::glue(
      "the census tract id column, {census_tract_id_name},",
      "does not contain every census tract for that vintage;",
      "Check for missing census tract observations and",
      "check that you are using the correct vintage.",
      .sep = " "
    ))
  }
  return(invisible(NULL))
}

check_date <- function(x) {
  if (!"year" %in% names(x)) {
    return("must contain a 'year' column")
  }
  years <- unique(x$year)
  if (!all(years %in% 1970:2099)) {
    return(
      "the 'year' field must only contain integer years between 1970 and 2099"
    )
  }
  if ("month" %in% names(x)) {
    if (!all(x$month %in% 1:12)) {
      return("the 'month' field  must only contain integer values 1-12")
    }
  }
  return(invisible(NULL))
}
