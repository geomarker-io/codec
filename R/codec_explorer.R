# Internal helpers shared by the CoDEC explorer app and data catalog article.

codec_explorer_base_url <- function() {
  option_url <- getOption("codec.explorer_url", "")
  env_url <- Sys.getenv("CODEC_EXPLORER_URL", unset = "")

  if (nzchar(option_url)) {
    return(option_url)
  }

  if (nzchar(env_url)) {
    return(env_url)
  }

  "https://019defb5-c409-5cd4-dfe9-7930f5ce4fe3.share.connect.posit.cloud/"
}

codec_explorer_defaults <- function() {
  list(
    x = "prop_poverty",
    y = "median_home_value",
    geography = "tract_2020",
    view = "bivariate",
    focus = "map"
  )
}

codec_explorer_url <- function(
  x = NULL,
  y = NULL,
  geography = NULL,
  view = NULL,
  focus = NULL
) {
  defaults <- codec_explorer_defaults()
  params <- list(
    x = if (is.null(x)) defaults$x else x,
    y = if (is.null(y)) defaults$y else y,
    geography = if (is.null(geography)) defaults$geography else geography,
    view = if (is.null(view)) defaults$view else view,
    focus = if (is.null(focus)) defaults$focus else focus
  )

  query <-
    paste0(
      names(params),
      "=",
      vapply(params, utils::URLencode, character(1), reserved = TRUE),
      collapse = "&"
    )

  paste0(codec_explorer_base_url(), "?", query)
}

codec_explorer_partner_field <- function(field) {
  defaults <- codec_explorer_defaults()
  if (identical(field, defaults$y)) {
    return(defaults$x)
  }
  defaults$y
}

codec_explorer_field_label <- function(field) {
  field
}

codec_explorer_field_format <- function(field) {
  if (
    grepl(
      "^(prop_|prcnt_|fraction_)",
      field
    ) ||
      grepl("mean_pct_", field)
  ) {
    return("percent")
  }

  if (grepl("home_value|market_total_value|online_market_total_value", field)) {
    return("currency")
  }

  if (grepl("year_built$|^year$", field)) {
    return("year")
  }

  if (grepl("^(n_|aadtm_)", field)) {
    return("count")
  }

  "number"
}

codec_explorer_field_value <- function(x, field) {
  fmt <- codec_explorer_field_format(field)
  out <- rep("NA", length(x))
  keep <- !is.na(x)

  if (!any(keep)) {
    return(out)
  }

  if (fmt == "percent") {
    out[keep] <- scales::label_percent(accuracy = 0.1)(x[keep])
    return(out)
  }

  if (fmt == "currency") {
    out[keep] <- scales::label_dollar(accuracy = 1)(x[keep])
    return(out)
  }

  if (fmt == "year") {
    out[keep] <- format(round(x[keep]), trim = TRUE)
    return(out)
  }

  if (fmt == "count") {
    out[keep] <- scales::label_number(accuracy = 1, big.mark = ",")(x[keep])
    return(out)
  }

  out[keep] <- scales::label_number(accuracy = 0.1, big.mark = ",")(x[keep])
  out
}

codec_latest_annual_fields <- function() {
  board <- codec_board_local()
  latest_annual <- codec_latest_annual
  fields <- setdiff(names(latest_annual), c("census_tract_id_2020", "year"))

  source_lookup <-
    lapply(codec_list(board), function(name) {
      table <- codec_read(name, board = board)
      tibble::tibble(
        field = setdiff(
          names(table),
          c("census_tract_id_2010", "census_tract_id_2020", "year", "month")
        ),
        source_table = name,
        source_title = attr(table, "title"),
        source_description = attr(table, "description")
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::distinct(field, .keep_all = TRUE)

  tibble::tibble(
    field = fields,
    label = vapply(fields, codec_explorer_field_label, character(1)),
    format = vapply(fields, codec_explorer_field_format, character(1)),
    is_numeric = vapply(latest_annual[fields], is.numeric, logical(1))
  ) |>
    dplyr::left_join(source_lookup, by = "field") |>
    dplyr::mutate(
      source_table = dplyr::case_when(
        !is.na(source_table) ~ source_table,
        grepl("crime|shoot|shots_fired", field) ~ "public_safety",
        TRUE ~ "derived_latest_annual"
      ),
      source_title = dplyr::case_when(
        !is.na(source_title) ~ source_title,
        source_table == "public_safety" ~ "Public safety measures",
        TRUE ~ "Derived latest annual measures"
      ),
      source_description = dplyr::case_when(
        !is.na(source_description) ~ source_description,
        source_table ==
          "public_safety" ~ "Crime and shooting measures included in the harmonized annual explorer.",
        TRUE ~ "Field included in the harmonized annual explorer but not directly traced to a single source table."
      ),
      choice_label = ifelse(
        is.na(source_title),
        label,
        paste0(label, " - ", source_title)
      )
    ) |>
    dplyr::arrange(source_table, field)
}

codec_explorer_dataset_choices <- function() {
  codec_latest_annual_fields() |>
    dplyr::distinct(source_table, source_title, source_description) |>
    dplyr::filter(source_table != "derived_latest_annual") |>
    dplyr::arrange(source_table)
}

codec_explorer_geography_choices <- function() {
  dplyr::bind_rows(
    tibble::tibble(
      geography = "tract_2020",
      family = "tract",
      geography_arg = "tract",
      vintage = "2020",
      neighborhood_type = NA_character_,
      label = "tract 2020"
    ),
    tibble::tibble(
      geography = "zcta_2020",
      family = "zcta",
      geography_arg = "zcta",
      vintage = "2020",
      neighborhood_type = NA_character_,
      label = "zcta 2020"
    ),
    tibble::tibble(
      geography = c(
        "neighborhood_statistical_neighborhood_approximations",
        "neighborhood_community_council"
      ),
      family = "neighborhood",
      geography_arg = "neighborhood",
      vintage = NA_character_,
      neighborhood_type = c(
        "statistical_neighborhood_approximations",
        "community_council"
      ),
      label = c(
        "neighborhood statistical_neighborhood_approximations",
        "neighborhood community_council"
      )
    )
  )
}

codec_explorer_normalize_geography <- function(geography) {
  if (is.null(geography) || !nzchar(geography)) {
    return(codec_explorer_defaults()$geography)
  }

  legacy_lookup <- c(
    tract = "tract_2020",
    zcta = "zcta_2020",
    neighborhood = "neighborhood_statistical_neighborhood_approximations"
  )

  if (geography %in% names(legacy_lookup)) {
    return(legacy_lookup[[geography]])
  }

  geography
}

utils::globalVariables(c(
  "source_table",
  "source_title",
  "source_description",
  "codec_latest_annual",
  "field",
  "source_title",
  "label",
  "source_table"
))
