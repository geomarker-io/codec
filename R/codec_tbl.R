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
#' @examples
#' tibble::tibble(
#'   census_tract_id_2020 = cincy_census_geo("tract", "2020")$geoid,
#'   n_things = 823,
#'   year = 2024L
#' ) |>
#'   as_codec_tbl(
#'     name = "n_things",
#'     "# Number of Things\n Number of things were averaged by census tract using the survey from 2024"
#'   )
as_codec_tbl <- function(x, name, description = character()) {
  if (inherits(x, "codec_tbl")) return(x)
  codec_check_census_tract_id(x)
  codec_check_date(x)
  name <- codec_check_label(name, "name", required = TRUE)
  if (!identical(tolower(name), name)) {
    rlang::abort("name of codec_tbl must be all lowercase")
  } else if (grepl("[^a-zA-Z0-9._-]", name)) {
    rlang::abort(
      "name of codec_tbl must only contain alphanumeric characters, except for `-`, `_`, and `.`"
    )
  }
  codec_check_label(description, "description", required = TRUE)
  desc_first_line <- strsplit(description, "\n")[[1]][1]
  if (!grepl("#", desc_first_line)) {
    rlang::abort(glue::glue("description of codec_tbl must begin with `#`"))
  }
  title <- sub("^#\\s*", "", desc_first_line)
  codec_check_label(title, "title", required = TRUE)
  if (nchar(title) > 80) {
    rlang::abort(c(
      "title of codec_tbl must be less than 80 characters in length",
      "Is your description string markdown with a level one header on the first line containing the title?",
      "For example:",
      "\"# My Table Title \\n The description about my table.\""
    ))
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

# check_codec_tbl <- function(x) {
#   if (!inherits(x, "codec_tbl")) {
#     rlang::abort("x must be a codec_tbl object")
#   }
# }

codec_check_label <- function(x, label_name, required = FALSE) {
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

codec_check_census_tract_id <- function(x) {
  census_tract_id_names <- paste0("census_tract_id", c("_2010", "_2020"))
  # has census_tract_id_{year} or census_tract_id column
  if (sum(names(x) %in% census_tract_id_names) != 1) {
    rlang::abort(
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
    rlang::abort(glue::glue(
      "the census tract id column, {census_tract_id_name},",
      "does not contain every census tract for that vintage;",
      "Check for missing census tract observations and",
      "check that you are using the correct vintage.",
      .sep = " "
    ))
  }
  return(invisible(x))
}

codec_check_date <- function(x) {
  if (!"year" %in% names(x)) {
    rlang::abort("codec_tbl must contain a 'year' column")
  }
  if (!is.integer(x$year)) {
    rlang::abort(glue::glue(
      "`year` field must be <integer>, not <{class(x$year)}>"
    ))
  }
  years <- unique(x$year)
  if (!all(years %in% 1970:2099)) {
    rlang::abort(
      "'year' field must be between 1970 and 2099"
    )
  }
  if ("month" %in% names(x)) {
    if (!is.integer(x$month)) {
      rlang::abort(glue::glue(
        "'month' must be <integer>, not {class(x$month)}"
      ))
    }
    if (!all(x$month %in% 1:12)) {
      rlang::abort("the 'month' field  must only contain integer values 1-12")
    }
  }
  return(x)
}
