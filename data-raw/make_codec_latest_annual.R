devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

# average values harmonized to 2020 tracts from all months in the latest year
# if second_latest is true, when the most recent year has less than 12 monthly values, the second latest year will be used instead
codec_harmonize_latest_annual <- function(x, second_latest = FALSE) {
  if (!inherits(x, "codec_tbl")) rlang::abort("x must be a codec_tbl object")
  x_2020_tract <- codec_interpolate(x, cincy_census_geo("tract", "2020"))
  latest_year <- max(x_2020_tract$year)
  x_latest <-
    x_2020_tract |>
    filter(year == latest_year)
  if ("month" %in% names(x)) {
    n_month_latest_year <- n_distinct(x_latest$month)
    if (n_month_latest_year != 12L) {
      rlang::warn(glue::glue(
        "averaging {n_month_latest_year} monthly values for the latest year ({latest_year}) in codec_tbl `{attr(x, 'name')}`"
      ))
      if (second_latest) {
        second_latest_year <- sort(
          unique(x_2020_tract$year),
          decreasing = TRUE
        )[2]
        rlang::warn(glue::glue(
          "`second_latest = TRUE`; using second latest year ({second_latest_year}) instead"
        ))
        x_latest <- filter(x_2020_tract, year == second_latest_year)
      }
    }
    x_latest <-
      summarize(
        x_latest,
        across(-c(year, month), mean, .names = "{.col}_per_month"),
        .by = c(geoid)
      )
  }
  out <-
    x_latest |>
    rename(census_tract_id_2020 = geoid) |>
    mutate(year = latest_year) |>
    as_codec_tbl(
      name = glue::glue("{ attr(x, 'name') }-latest_annual_{latest_year}"),
      description = attr(x, "description")
    )
  return(out)
}

d <-
  purrr::map(
    pins::pin_list(codec_board_local_dev()),
    \(x)
      codec_harmonize_latest_annual(
        codec_read(x, codec_board_local_dev()),
        second_latest = TRUE
      ),
    .progress = "harmonizing CoDEC tables to latest annual for 2020 tracts"
  )


codec_latest_annual <-
  d |>
  purrr::map(~ tibble::as_tibble(select(., -year))) |>
  purrr::reduce(left_join, by = "census_tract_id_2020") |>
  mutate(year = 2025L) |>
  as_codec_tbl(
    name = "codec_latest_annual",
    description = glue::glue(
      "# CoDEC latest annual",
      "CoDEC data is harmonized as the most recently available annual (or annual average of monthly) values at the census tract 2020 geography.",
      "The year represents the year this table was assembled and mainly used as a placeholder so this object can be used with codec_*() functions.",
      "The actual latest years used for each CoDEC table are:",
      paste0(sapply(d, \(x) attr(x, "name")), collapse = "\n"),
      .sep = "\n"
    ) |>
      as.character()
  )

usethis::use_data(
  codec_latest_annual,
  internal = FALSE,
  overwrite = TRUE
)
