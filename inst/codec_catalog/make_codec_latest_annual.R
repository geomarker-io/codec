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
        latest_year <- second_latest_year
        x_latest <-
          filter(x_2020_tract, year == latest_year) |>
          summarize(across(-month, mean), .by = c(geoid, year))
      }
    }
    x_latest <-
      summarize(
        x_latest,
        across(c(-year), mean, .names = "{.col}_per_month"),
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
    \(x) codec_harmonize_latest_annual(codec_read(x, codec_board_local_dev())),
    .progress = "harmonizing CoDEC tables to latest annual for 2020 tracts"
  )

sapply(d, attr, "name")


# TODO month_per_month is still coming through??
attributes(d[[2]])


d_out <-
  d |>
  imap(\(df, nm) {
    keep <- setdiff(names(df), "census_tract_id_2020")
    names(df)[names(df) %in% keep] <- paste0(
      nm,
      "_",
      names(df)[names(df) %in% keep]
    )
    df
  }) |>
  reduce(left_join, by = "nces_school_id")

attributes(d[[1]])

md <- saveRDS(dpkgs, "inst/codec_catalog/all_codec_dpkg.rds")
