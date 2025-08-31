devtools::load_all()
codec_name <- "traffic"

library(dplyr, warn.conflicts = FALSE)
library(s2)

hpms_gpkg <-
  pins::board_url(c(
    hpms = "https://github.com/geomarker-io/appc/releases/download/hpms_2020_f12_aadt-2025-07-16/hpms_2020_f12_aadt.gpkg"
  )) |>
  pins::pin_download("hpms")

hpms <-
  sf::st_read(
    hpms_gpkg,
    layer = sf::st_layers(hpms_gpkg)$name[1],
    quiet = TRUE
  ) |>
  mutate(s2_geography = s2::as_s2_geography(geom)) |>
  sf::st_drop_geometry() |>
  tibble::as_tibble()

aoi <- cincy_census_geo("tract", "2020")

withins <-
  aoi$s2_geography |>
  purrr::map(
    \(x) {
      s2_prepared_dwithin(hpms$s2_geography, x, 200) |>
        which()
    },
    .progress = "intersecting HPMS with areas of interest"
  )

get_intersection_aadtm <- function(the_tract, the_s2_withins) {
  hpms_withins <- hpms[the_s2_withins, ]
  lengths <- s2::s2_intersection(hpms_withins$s2_geography, the_tract) |>
    s2::s2_length()
  out <- c(
    aadtm_trucks_buses = sum(hpms_withins$AADT_SINGLE_UNIT * lengths),
    aadtm_tractor_trailer = sum(hpms_withins$AADT_COMBINATION * lengths),
    aadtm_passenger = with(
      hpms_withins,
      sum((AADT - AADT_SINGLE_UNIT - AADT_COMBINATION) * lengths)
    )
  )
  return(out)
}

aadtm <- purrr::pmap_dfr(
  list(the_tract = aoi$s2_geography, the_s2_withins = withins),
  get_intersection_aadtm
)

d <-
  bind_cols(aoi, aadtm) |>
  tibble::as_tibble() |>
  select(-s2_geography) |>
  rename(census_tract_id_2020 = geoid) |>
  mutate(year = 2020L)

d |>
  as_codec_tbl(
    name = codec_name,
    description = paste(
      readLines(fs::path_package(
        "codec",
        "data-raw",
        "codec_tbl",
        codec_name,
        "README.md"
      )),
      collapse = "\n"
    )
  ) |>
  write_codec_pin()
