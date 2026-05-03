app_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(...) {
  ""
})
app_dir <- if (nzchar(app_file)) dirname(app_file) else normalizePath(getwd())
repo_root <- normalizePath(file.path(app_dir, "..", ".."), mustWork = FALSE)

if (
  file.exists(file.path(repo_root, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)
) {
  pkgload::load_all(
    repo_root,
    export_all = FALSE,
    helpers = FALSE,
    quiet = TRUE
  )
}

library(codec)
library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(htmltools)
library(sf)

field_meta <- codec:::codec_latest_annual_fields()
field_lookup <- split(field_meta, field_meta$field)
dataset_meta <- codec:::codec_explorer_dataset_choices()
dataset_lookup <- split(dataset_meta, dataset_meta$source_table)
dataset_values <- dataset_meta$source_table
dataset_choices <- stats::setNames(dataset_values, dataset_values)
geography_meta <- codec:::codec_explorer_geography_choices()
geography_lookup <- split(geography_meta, geography_meta$geography)
numeric_fields <- field_meta$field[
  field_meta$is_numeric & field_meta$field != "year"
]
defaults <- codec:::codec_explorer_defaults()
dataset_cache <- new.env(parent = emptyenv())

coalesce_string <- function(x, y) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(y)
  }
  x
}

field_label <- function(field) {
  field_lookup[[field]]$label[[1]]
}

field_format <- function(field) {
  field_lookup[[field]]$format[[1]]
}

dataset_title <- function(dataset) {
  dataset_lookup[[dataset]]$source_title[[1]]
}

dataset_description <- function(dataset) {
  dataset_lookup[[dataset]]$source_description[[1]]
}

dataset_description_summary <- function(dataset, max_chars = 420L) {
  description <- dataset_description(dataset)

  if (is.null(description) || is.na(description) || !nzchar(description)) {
    return("No dataset description is available.")
  }

  description <- gsub("\r", "", description, fixed = TRUE)
  if (grepl("## About", description, fixed = TRUE)) {
    description <- sub(
      "^.*## About\\s*",
      "",
      description
    )
  }
  description <- sub("\\s*## .*", "", description)
  description <- gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", description)
  description <- gsub("[#*_`>]", "", description)
  description <- gsub("\\s*[-+]\\s+", " ", description)
  description <- trimws(gsub("\\s+", " ", description))

  if (nchar(description) > max_chars) {
    description <- paste0(substr(description, 1, max_chars - 1L), "...")
  }

  description
}

catalog_url <- function() {
  option_url <- getOption("codec.catalog_url", "")
  env_url <- Sys.getenv("CODEC_CATALOG_URL", unset = "")

  if (nzchar(option_url)) {
    return(option_url)
  }

  if (nzchar(env_url)) {
    return(env_url)
  }

  "https://geomarker.io/codec/articles/data.html"
}

dataset_for_field <- function(field) {
  field_lookup[[field]]$source_table[[1]]
}

fields_for_dataset <- function(dataset) {
  field_meta |>
    dplyr::filter(source_table == dataset, is_numeric, field != "year") |>
    dplyr::arrange(field) |>
    dplyr::pull(field)
}

field_choices_for_dataset <- function(dataset) {
  fields <- fields_for_dataset(dataset)
  stats::setNames(fields, fields)
}

geography_values <- geography_meta$geography
geography_choices <- stats::setNames(
  geography_meta$geography,
  geography_meta$label
)

geography_spec <- function(geography) {
  geography <- codec:::codec_explorer_normalize_geography(geography)
  geography_lookup[[geography]][1, , drop = FALSE]
}

geography_label <- function(geography) {
  geography_spec(geography)$label[[1]]
}

format_field_value <- function(value, field) {
  codec:::codec_explorer_field_value(value, field)
}

dataset_source_note_ui <- function(dataset) {
  tags$div(
    class = "codec-dataset-meta",
    tags$div(
      class = "codec-input-note",
      tags$span(dataset_title(dataset))
    ),
    tags$div(
      class = "codec-dataset-tooltip",
      role = "tooltip",
      tags$div(class = "codec-dataset-tooltip__eyebrow", dataset),
      tags$div(
        class = "codec-dataset-tooltip__title",
        dataset_title(dataset)
      ),
      tags$p(dataset_description_summary(dataset)),
      tags$a(
        href = catalog_url(),
        target = "_blank",
        rel = "noopener noreferrer",
        "Open data catalog"
      )
    )
  )
}

geography_name <- function(spec, geoid) {
  if (identical(spec$family[[1]], "neighborhood")) {
    return(geoid)
  }

  paste(spec$family[[1]], geoid)
}

safe_ntile <- function(x, n = 3L) {
  bins <- rep(NA_integer_, length(x))
  keep <- !is.na(x)
  if (!any(keep)) {
    return(bins)
  }
  bins[keep] <- dplyr::ntile(x[keep], n)
  bins
}

bivariate_palette <- matrix(
  c(
    "#f6edde",
    "#e5c7ba",
    "#c28273",
    "#d9e3e2",
    "#a9b9b8",
    "#738d92",
    "#a9c4cf",
    "#6e93a1",
    "#396175"
  ),
  nrow = 3,
  byrow = TRUE
)

univariate_palette <- c(
  "#f6edde",
  "#e8d2c4",
  "#c9a296",
  "#789aa4",
  "#396175"
)

legend_onclick <- function(view, x_field, y_field, bin) {
  js_quote <- function(x) {
    paste0("'", gsub("'", "\\\\'", x, fixed = TRUE), "'")
  }
  paste0(
    "Shiny.setInputValue('legend_selection', ",
    "{view:",
    js_quote(view),
    ",x:",
    js_quote(x_field),
    ",y:",
    js_quote(y_field),
    ",bin:",
    js_quote(bin),
    ",nonce:Date.now()}, {priority: 'event'});"
  )
}

base_geography_sf <- function(spec) {
  family <- spec$family[[1]]
  vintage <- spec$vintage[[1]]
  neighborhood_type <- spec$neighborhood_type[[1]]

  if (identical(family, "tract") && identical(vintage, "2020")) {
    return(get("cincy_tract_geo_2020", asNamespace("codec"), inherits = FALSE))
  }

  if (identical(family, "zcta") && identical(vintage, "2020")) {
    return(
      get("cincy_zcta_geo_2020", asNamespace("codec"), inherits = FALSE) |>
        sf::st_as_sf(sf_column_name = "s2_geography")
    )
  }

  if (
    identical(family, "neighborhood") &&
      identical(neighborhood_type, "statistical_neighborhood_approximations")
  ) {
    return(get(
      "cincy_neighborhood_geo_sna",
      asNamespace("codec"),
      inherits = FALSE
    ))
  }

  if (identical(family, "tract")) {
    return(codec::cincy_census_geo("tract", vintage))
  }

  if (identical(family, "zcta")) {
    return(codec::cincy_zcta_geo(vintage))
  }

  if (identical(family, "neighborhood")) {
    return(codec::cincy_neighborhood_geo(neighborhood_type))
  }

  stop("Unsupported geography: ", spec$geography[[1]], call. = FALSE)
}

attach_latest_annual_values <- function(geo_sf, spec) {
  if (
    identical(spec$family[[1]], "tract") &&
      identical(spec$vintage[[1]], "2020")
  ) {
    values <- codec::codec_latest_annual
    idx <- match(geo_sf$geoid, values$census_tract_id_2020)
    value_names <- setdiff(names(values), "census_tract_id_2020")
  } else {
    values <- codec::codec_interpolate(codec::codec_latest_annual, geo_sf)
    idx <- match(geo_sf$geoid, values$geoid)
    value_names <- setdiff(names(values), "geoid")
  }

  geo_sf$geo_id <- geo_sf$geoid
  geo_sf$geo_name <- geography_name(spec, geo_sf$geoid)

  for (name in value_names) {
    geo_sf[[name]] <- values[[name]][idx]
  }

  geo_sf$geography <- spec$geography[[1]]
  geo_sf$geography_label <- spec$label[[1]]

  sf::st_transform(geo_sf, 4326)
}

build_geography_data <- function(geography) {
  spec <- geography_spec(geography)
  geo_sf <- base_geography_sf(spec)
  attach_latest_annual_values(geo_sf, spec)
}

cached_geography_data <- function(geography) {
  key <- codec:::codec_explorer_normalize_geography(geography)

  if (!exists(key, envir = dataset_cache, inherits = FALSE)) {
    assign(key, build_geography_data(key), envir = dataset_cache)
  }

  get(key, envir = dataset_cache, inherits = FALSE)
}

map_data_for_view <- function(
  data,
  x_field,
  y_field,
  view,
  selection = NULL,
  selected_geo_id = NULL
) {
  data$x_value <- data[[x_field]]
  data$y_value <- data[[y_field]]
  data$x_bin <- safe_ntile(data$x_value)
  data$y_bin <- safe_ntile(data$y_value)

  if (identical(view, "bivariate")) {
    data$fill_color <- "#d7dde3"
    keep <- !is.na(data$x_bin) & !is.na(data$y_bin)
    data$fill_color[keep] <- vapply(
      seq_len(sum(keep)),
      function(i) {
        bivariate_palette[data$y_bin[keep][i], data$x_bin[keep][i]]
      },
      character(1)
    )
    data$legend_bin <- ifelse(
      keep,
      paste(data$x_bin, data$y_bin, sep = "-"),
      NA_character_
    )
    data$legend_title <- paste(field_label(x_field), "vs", field_label(y_field))
  } else {
    active_field <- if (identical(view, "x")) x_field else y_field
    data$value_bin <- safe_ntile(data[[active_field]], 5L)
    keep <- !is.na(data$value_bin)
    data$fill_color <- "#d7dde3"
    data$fill_color[keep] <- univariate_palette[data$value_bin[keep]]
    data$legend_bin <- ifelse(
      keep,
      as.character(data$value_bin),
      NA_character_
    )
    data$legend_title <- field_label(active_field)
  }

  selected_bin <- selection$bin %||% NA_character_
  has_selection <- !is.null(selection) &&
    !is.na(selected_bin) &&
    selected_bin %in% data$legend_bin
  data$legend_selected <- has_selection & data$legend_bin == selected_bin
  data$geo_selected <- if (is.null(selected_geo_id)) {
    rep(FALSE, nrow(data))
  } else {
    !is.na(data$geo_id) & data$geo_id == selected_geo_id
  }
  has_geo_selection <- any(data$geo_selected, na.rm = TRUE)

  data$map_fill_opacity <- if (has_selection) {
    ifelse(data$legend_selected, 0.95, 0.16)
  } else if (has_geo_selection) {
    ifelse(data$geo_selected, 0.95, 0.38)
  } else {
    0.85
  }
  data$map_weight <- if (has_selection) {
    ifelse(data$legend_selected, 1.7, 0.55)
  } else if (has_geo_selection) {
    ifelse(data$geo_selected, 2.4, 0.65)
  } else {
    0.9
  }
  data$map_color <- if (has_selection) {
    ifelse(data$legend_selected, "#1f2e38", "#7b858b")
  } else if (has_geo_selection) {
    ifelse(data$geo_selected, "#1f2e38", "#7b858b")
  } else {
    "#4c5961"
  }

  data$popup_html <- paste0(
    "<div class='codec-popup'>",
    "<div class='codec-popup__title'>",
    htmltools::htmlEscape(data$geo_name),
    "</div>",
    "<div><strong>",
    htmltools::htmlEscape(field_label(x_field)),
    ":</strong> ",
    htmltools::htmlEscape(format_field_value(data$x_value, x_field)),
    "</div>",
    "<div><strong>",
    htmltools::htmlEscape(field_label(y_field)),
    ":</strong> ",
    htmltools::htmlEscape(format_field_value(data$y_value, y_field)),
    "</div>",
    "</div>"
  )
  data
}

bucket_summary <- function(data, field, bin_column, bin) {
  keep <- data[[bin_column]] == as.integer(bin) & !is.na(data[[field]])
  count <- sum(keep, na.rm = TRUE)

  if (!count) {
    return(paste(field_label(field), ": no geographies"))
  }

  rng <- range(data[[field]][keep], na.rm = TRUE)
  paste0(
    field_label(field),
    ": ",
    format_field_value(rng[[1]], field),
    " to ",
    format_field_value(rng[[2]], field),
    " (",
    count,
    " geographies)"
  )
}

legend_detail_ui <- function(lines) {
  tags$div(
    class = "codec-legend__detail",
    lapply(lines, tags$div)
  )
}

bin_range_labels <- function(data, field, bin_column, bins) {
  vapply(
    bins,
    function(bin) {
      values <- data[[field]][data[[bin_column]] == as.integer(bin)]
      values <- values[!is.na(values)]

      if (!length(values)) {
        return("no data")
      }

      rng <- range(values, na.rm = TRUE)
      paste0(
        format_field_value(rng[[1]], field),
        " to ",
        format_field_value(rng[[2]], field)
      )
    },
    character(1)
  )
}

map_legend_ui <- function(view, x_field, y_field, data, selection = NULL) {
  if (identical(view, "bivariate")) {
    cells <- unlist(
      lapply(3:1, function(y_bin) {
        lapply(1:3, function(x_bin) {
          bin <- paste(x_bin, y_bin, sep = "-")
          selected <- !is.null(selection) && identical(selection$bin, bin)
          tags$button(
            type = "button",
            class = paste(
              "codec-legend__cell",
              if (selected) "is-selected" else NULL
            ),
            style = paste0("background:", bivariate_palette[y_bin, x_bin], ";"),
            title = paste(
              field_label(x_field),
              "bin",
              x_bin,
              "and",
              field_label(y_field),
              "bin",
              y_bin
            ),
            onclick = legend_onclick("bivariate", x_field, y_field, bin)
          )
        })
      }),
      recursive = FALSE
    )

    detail <- if (!is.null(selection) && grepl("-", selection$bin)) {
      bins <- strsplit(selection$bin, "-", fixed = TRUE)[[1]]
      legend_detail_ui(c(
        bucket_summary(data, x_field, "x_bin", bins[[1]]),
        bucket_summary(data, y_field, "y_bin", bins[[2]])
      ))
    } else {
      legend_detail_ui("Select a legend cell to highlight matching geographies.")
    }

    return(tags$div(
      class = "codec-legend codec-legend--bivariate",
      tags$div(
        class = "codec-legend__body",
        tags$div(
          class = "codec-legend__axis codec-legend__axis--y",
          "\u2191 ",
          field_label(y_field)
        ),
        tags$div(class = "codec-legend__grid", cells),
        tags$div(
          class = "codec-legend__axis codec-legend__axis--x",
          field_label(x_field),
          " \u2192"
        )
      ),
      detail
    ))
  }

  active_field <- if (identical(view, "x")) x_field else y_field
  bins <- seq_along(univariate_palette)
  labels <- bin_range_labels(data, active_field, "value_bin", bins)
  cells <- lapply(bins, function(bin) {
    selected <- !is.null(selection) && identical(selection$bin, as.character(bin))
    tags$div(
      class = "codec-legend__bin",
      tags$button(
        type = "button",
        class = paste(
          "codec-legend__step",
          if (selected) "is-selected" else NULL
        ),
        style = paste0("background:", univariate_palette[[bin]], ";"),
        title = paste(field_label(active_field), labels[[bin]]),
        onclick = legend_onclick(view, x_field, y_field, as.character(bin))
      ),
      tags$span(class = "codec-legend__bin-label", labels[[bin]])
    )
  })

  detail <- if (!is.null(selection)) {
    legend_detail_ui(bucket_summary(
      data,
      active_field,
      "value_bin",
      selection$bin
    ))
  } else {
    legend_detail_ui("Select a color to highlight matching geographies.")
  }

  tags$div(
    class = "codec-legend codec-legend--univariate",
    tags$div(class = "codec-legend__title", field_label(active_field)),
    tags$div(class = "codec-legend__bar", cells),
    detail
  )
}

query_value <- function(query, name, fallback, choices = NULL) {
  value <- coalesce_string(query[[name]], fallback)
  if (!is.null(choices) && !(value %in% choices)) {
    return(fallback)
  }
  value
}

theme <- bs_theme(
  version = 5,
  bg = "#f3f0eb",
  fg = "#27485c",
  primary = "#c28273",
  secondary = "#8cb4c3",
  border_radius = "0.85rem",
  font_scale = 1
)

ui <- page_fillable(
  theme = theme,
  title = "CoDEC Explorer",
  fillable_mobile = TRUE,
  tags$head(
    tags$script(HTML(
      "
      document.addEventListener('DOMContentLoaded', function() {
        document.documentElement.classList.add('codec-booting');

        document.querySelectorAll('form, input, select, textarea').forEach(function(el) {
          el.setAttribute('autocomplete', 'off');
        });

        const pushStartupQuery = function() {
          if (!window.Shiny || !window.Shiny.setInputValue) {
            return false;
          }

          window.Shiny.setInputValue('startup_query', window.location.search || '', { priority: 'event' });
          return true;
        };

        const armControls = function(event) {
          if (!event.target.closest('.codec-controls, .codec-sidebar')) {
            return;
          }

          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('controls_interacted', Date.now(), { priority: 'event' });
          }
        };

        document.addEventListener('pointerdown', armControls, true);
        document.addEventListener('keydown', armControls, true);

        ['legend_panel']
          .forEach(function(id) {
            const el = document.getElementById(id);
            if (el) {
              el.innerHTML = '';
            }
          });

        const registerSyncHandler = function() {
          if (!window.Shiny || !window.Shiny.addCustomMessageHandler || window.__codecSyncHandlerRegistered) {
            return false;
          }

          window.__codecSyncHandlerRegistered = true;
          window.Shiny.addCustomMessageHandler('codec-sync-controls', function(payload) {
            const values = payload && payload.values ? payload.values : {};

            Object.entries(values).forEach(function(entry) {
              const id = entry[0];
              const value = entry[1];
              const el = document.getElementById(id);

              if (!el) {
                return;
              }

              if (el.tagName === 'SELECT') {
                Array.from(el.options).forEach(function(option) {
                  option.selected = option.value === value;
                });
              }

              if ('value' in el) {
                el.value = value;
              }
            });

            document.documentElement.classList.remove('codec-booting');
          });

          return true;
        };

        if (!registerSyncHandler()) {
          const syncRegistrationTimer = window.setInterval(function() {
            if (registerSyncHandler()) {
              window.clearInterval(syncRegistrationTimer);
            }
          }, 50);

          window.setTimeout(function() {
            window.clearInterval(syncRegistrationTimer);
          }, 5000);
        }

        if (!pushStartupQuery()) {
          const startupQueryTimer = window.setInterval(function() {
            if (pushStartupQuery()) {
              window.clearInterval(startupQueryTimer);
            }
          }, 50);

          window.setTimeout(function() {
            window.clearInterval(startupQueryTimer);
          }, 5000);
        }

        window.addEventListener('pageshow', pushStartupQuery, { once: true });

        window.setTimeout(function() {
          document.documentElement.classList.remove('codec-booting');
        }, 4000);
      });
    "
    )),
    tags$style(HTML(
      "
    :root {
      --codec-bg: #f3f0eb;
      --codec-panel: #fffdfa;
      --codec-ink: #27485c;
      --codec-muted: #71828d;
      --codec-accent: #c28273;
      --codec-border: #d7d3cd;
      --codec-mapline: #48545d;
    }
    html, body {
      background: linear-gradient(180deg, #f7f4ef 0%, var(--codec-bg) 100%);
    }
    .bslib-page-fill {
      gap: 1rem;
      padding: 1rem;
    }
    .codec-booting .codec-controls,
    .codec-booting .codec-sidebar,
    .codec-booting .codec-map {
      visibility: hidden;
    }
    .codec-shell {
      background: var(--codec-panel);
      border: 1px solid var(--codec-border);
      border-radius: 1rem;
      box-shadow: 0 18px 36px rgba(57, 97, 117, 0.08);
      overflow: hidden;
    }
    .codec-header {
      padding: 1.15rem 1.25rem;
      border-bottom: 1px solid var(--codec-border);
    }
    .codec-brand {
      display: grid;
      gap: 0.2rem;
    }
    .codec-kicker {
      color: var(--codec-accent);
      font-size: 0.8rem;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      font-weight: 700;
    }
    .codec-title {
      font-size: 1.3rem;
      font-weight: 700;
      color: var(--codec-ink);
      line-height: 1.1;
    }
    .codec-subtitle {
      color: var(--codec-muted);
      font-size: 0.92rem;
    }
    .codec-controls {
      display: grid;
      gap: 1rem;
      min-height: 0;
    }
    .codec-workspace {
      display: grid;
      grid-template-columns: minmax(280px, 0.34fr) minmax(0, 0.66fr);
      min-height: 0;
    }
    .codec-sidebar {
      padding: 1rem;
      display: grid;
      gap: 1rem;
      min-height: 0;
      border-right: 1px solid var(--codec-border);
      align-content: start;
    }
    .codec-sidebar .shiny-input-container {
      margin-bottom: 0;
    }
    .codec-field-card {
      display: grid;
      gap: 0.6rem;
      padding: 0.85rem 0.9rem;
      border: 1px solid var(--codec-border);
      border-radius: 0.9rem;
      background: #fcfaf7;
    }
    .codec-field-card__title {
      color: var(--codec-muted);
      font-size: 0.78rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      font-weight: 700;
    }
    .codec-input-note {
      display: grid;
      gap: 0.15rem;
      color: var(--codec-muted);
      font-size: 0.84rem;
      line-height: 1.35;
      margin-top: -0.35rem;
    }
    .codec-dataset-control {
      position: relative;
      display: grid;
      gap: 0.6rem;
    }
    .codec-dataset-meta {
      position: relative;
      width: fit-content;
      max-width: 100%;
    }
    .codec-dataset-tooltip {
      position: absolute;
      left: 0;
      top: calc(100% + 0.45rem);
      z-index: 600;
      width: min(22rem, calc(100vw - 3rem));
      padding: 0.85rem 0.9rem;
      color: var(--codec-ink);
      background: #fffdfa;
      border: 1px solid var(--codec-border);
      border-radius: 0.75rem;
      box-shadow: 0 16px 32px rgba(39, 72, 92, 0.18);
      opacity: 0;
      pointer-events: none;
      transform: translateY(-0.25rem);
      transition: opacity 120ms ease, transform 120ms ease;
    }
    .codec-dataset-control:hover .codec-dataset-tooltip,
    .codec-dataset-control:focus-within .codec-dataset-tooltip {
      opacity: 1;
      pointer-events: auto;
      transform: translateY(0);
    }
    .codec-dataset-tooltip__eyebrow {
      color: var(--codec-accent);
      font-size: 0.72rem;
      font-weight: 700;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      overflow-wrap: anywhere;
    }
    .codec-dataset-tooltip__title {
      margin-top: 0.2rem;
      color: var(--codec-ink);
      font-weight: 700;
    }
    .codec-dataset-tooltip p {
      margin: 0.45rem 0 0.65rem;
      color: var(--codec-muted);
      font-size: 0.86rem;
      line-height: 1.35;
    }
    .codec-dataset-tooltip a {
      color: var(--codec-accent);
      font-size: 0.84rem;
      font-weight: 700;
      text-decoration: none;
    }
    .codec-dataset-tooltip a:hover,
    .codec-dataset-tooltip a:focus {
      text-decoration: underline;
    }
    .codec-map {
      padding: 1rem;
      display: grid;
      gap: 0.8rem;
      min-height: 0;
    }
    .codec-map__frame {
      min-height: 520px;
    }
    .codec-note {
      color: var(--codec-muted);
      font-size: 0.92rem;
      line-height: 1.45;
    }
    .codec-link {
      color: var(--codec-accent);
      text-decoration: none;
      font-weight: 700;
    }
    .codec-popup__title {
      font-weight: 700;
      margin-bottom: 0.3rem;
      color: var(--codec-ink);
    }
    .codec-legend {
      display: flex;
      flex-wrap: wrap;
      gap: 0.8rem 1rem;
      align-content: start;
      align-items: end;
      border-top: 1px solid var(--codec-border);
      padding-top: 0.8rem;
    }
    .codec-legend__title {
      font-size: 0.82rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      color: var(--codec-muted);
      font-weight: 700;
    }
    .codec-legend__axis {
      font-size: 0.84rem;
      color: var(--codec-muted);
    }
    .codec-legend__body {
      display: grid;
      gap: 0.3rem;
      width: max-content;
    }
    .codec-legend__grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 0.18rem;
      width: 5.5rem;
    }
    .codec-legend__cell {
      display: block;
      aspect-ratio: 1;
      border-radius: 0.2rem;
      border: 2px solid transparent;
      cursor: pointer;
      padding: 0;
    }
    .codec-legend__bar {
      display: grid;
      grid-template-columns: repeat(5, 1fr);
      gap: 0.32rem;
      min-width: min(100%, 32rem);
      max-width: 42rem;
    }
    .codec-legend__bin {
      display: grid;
      gap: 0.25rem;
      min-width: 0;
    }
    .codec-legend__step {
      display: block;
      height: 0.85rem;
      border-radius: 999px;
      border: 2px solid transparent;
      cursor: pointer;
      padding: 0;
    }
    .codec-legend__bin-label {
      color: var(--codec-muted);
      font-size: 0.68rem;
      line-height: 1.15;
      overflow-wrap: anywhere;
    }
    .codec-legend__cell.is-selected,
    .codec-legend__step.is-selected {
      border-color: #1f2e38;
      box-shadow: 0 0 0 2px rgba(255, 253, 250, 0.95);
    }
    .codec-legend__detail {
      color: var(--codec-muted);
      font-size: 0.86rem;
      line-height: 1.35;
      max-width: 34rem;
    }
    .codec-histogram {
      border-top: 1px solid var(--codec-border);
      padding-top: 0.8rem;
    }
    .codec-histogram__title {
      color: var(--codec-muted);
      font-size: 0.82rem;
      font-weight: 700;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      margin-bottom: 0.35rem;
    }
    .form-label {
      color: var(--codec-ink);
      font-weight: 700;
      margin-bottom: 0.35rem;
    }
    .leaflet-container {
      border-radius: 0.9rem;
      overflow: hidden;
    }
    @media (max-width: 920px) {
      .codec-header > .bslib-grid {
        grid-template-columns: minmax(0, 1fr) !important;
      }
      .codec-workspace {
        grid-template-columns: minmax(0, 1fr) !important;
      }
      .codec-sidebar {
        border-right: 0;
        border-bottom: 1px solid var(--codec-border);
      }
      .codec-map__frame {
        min-height: 460px;
      }
    }
    .plotly .modebar {
      top: 8px;
      right: 8px;
    }
  "
    ))
  ),
  div(
    class = "codec-shell",
    div(
      class = "codec-header",
      layout_columns(
        col_widths = c(5, 7),
        div(
          class = "codec-brand",
          tags$div(
            class = "codec-kicker",
            "Community Data Explorer for Cincinnati"
          ),
          tags$div(class = "codec-title", "CoDEC Latest Annual Explorer"),
          tags$div(
            class = "codec-subtitle",
            paste("CoDEC version", utils::packageVersion("codec"))
          )
        ),
        div(
          class = "codec-controls",
          layout_columns(
            col_widths = c(7, 3, 2),
            selectInput(
              "geography_control",
              "Geography",
              choices = geography_choices,
              selected = defaults$geography
            ),
            selectInput(
              "view_control",
              "Map view",
              choices = c(
                "Bivariate" = "bivariate",
                "X variable" = "x",
                "Y variable" = "y"
              ),
              selected = defaults$view
            ),
            actionButton(
              "reset_controls",
              "Reset",
              class = "btn btn-outline-secondary"
            )
          )
        )
      )
    ),
    div(
      class = "codec-workspace",
      div(
        class = "codec-sidebar",
        div(
          class = "codec-field-card",
          tags$div(class = "codec-field-card__title", "X"),
          div(
            class = "codec-dataset-control",
            selectInput(
              "x_dataset_control",
              "Dataset",
              choices = dataset_choices,
              selected = dataset_for_field(defaults$x)
            ),
            uiOutput("x_source_note")
          ),
          selectInput(
            "x_var_control",
            "Variable",
            choices = field_choices_for_dataset(dataset_for_field(defaults$x)),
            selected = defaults$x
          )
        ),
        div(
          class = "codec-field-card",
          tags$div(class = "codec-field-card__title", "Y"),
          div(
            class = "codec-dataset-control",
            selectInput(
              "y_dataset_control",
              "Dataset",
              choices = dataset_choices,
              selected = dataset_for_field(defaults$y)
            ),
            uiOutput("y_source_note")
          ),
          selectInput(
            "y_var_control",
            "Variable",
            choices = field_choices_for_dataset(dataset_for_field(defaults$y)),
            selected = defaults$y
          )
        )
      ),
      div(
        class = "codec-map",
        div(class = "codec-map__frame", leafletOutput("map", height = "100%")),
        uiOutput("legend_panel"),
        uiOutput("map_detail_panel")
      )
    )
  )
)

server <- function(input, output, session) {
  dataset_values <- dataset_meta$source_table
  dataset_choices <- stats::setNames(dataset_values, dataset_values)
  default_state <- list(
    geography = defaults$geography,
    view = defaults$view,
    x_dataset = dataset_for_field(defaults$x),
    x = defaults$x,
    y_dataset = dataset_for_field(defaults$y),
    y = defaults$y
  )

  app_state <- reactiveVal(default_state)
  app_started <- reactiveVal(FALSE)
  ui_ready <- reactiveVal(FALSE)
  controls_live <- reactiveVal(FALSE)
  controls_interacted <- reactiveVal(FALSE)
  last_query <- reactiveVal(NULL)
  selected_legend <- reactiveVal(NULL)
  selected_geography <- reactiveVal(NULL)
  control_id <- function(name) {
    switch(
      name,
      geography = "geography_control",
      view = "view_control",
      reset_controls = "reset_controls",
      x_dataset = "x_dataset_control",
      x_var = "x_var_control",
      y_dataset = "y_dataset_control",
      y_var = "y_var_control",
      name
    )
  }

  normalize_state <- function(state) {
    state$geography <- codec:::codec_explorer_normalize_geography(
      state$geography
    )
    if (!(state$geography %in% geography_values)) {
      state$geography <- default_state$geography
    }

    if (!(state$view %in% c("bivariate", "x", "y"))) {
      state$view <- default_state$view
    }

    if (is.null(state$x_dataset) || !(state$x_dataset %in% dataset_values)) {
      state$x_dataset <- default_state$x_dataset
    }

    if (is.null(state$y_dataset) || !(state$y_dataset %in% dataset_values)) {
      state$y_dataset <- default_state$y_dataset
    }

    x_fields <- fields_for_dataset(state$x_dataset)
    y_fields <- fields_for_dataset(state$y_dataset)

    if (!(state$x %in% x_fields)) {
      state$x <- if (default_state$x %in% x_fields) {
        default_state$x
      } else {
        x_fields[[1]]
      }
    }

    if (!(state$y %in% y_fields)) {
      state$y <- if (default_state$y %in% y_fields) {
        default_state$y
      } else {
        y_fields[[1]]
      }
    }

    state
  }

  state_query <- function(state) {
    paste0(
      "?x=",
      utils::URLencode(state$x, reserved = TRUE),
      "&y=",
      utils::URLencode(state$y, reserved = TRUE),
      "&geography=",
      utils::URLencode(state$geography, reserved = TRUE),
      "&view=",
      utils::URLencode(state$view, reserved = TRUE)
    )
  }

  set_state <- function(state) {
    state <- normalize_state(state)
    if (!identical(state, app_state())) {
      selected_legend(NULL)
      selected_geography(NULL)
      app_state(state)
    }
  }

  control_values_from_state <- function(state) {
    x_choices <- field_choices_for_dataset(state$x_dataset)
    y_choices <- field_choices_for_dataset(state$y_dataset)

    list(
      geography = state$geography,
      view = state$view,
      x_dataset = state$x_dataset,
      x_var = state$x,
      x_choices = x_choices,
      y_dataset = state$y_dataset,
      y_var = state$y,
      y_choices = y_choices
    )
  }

  sync_controls_from_values <- function(values) {
    shiny::updateSelectInput(
      session,
      control_id("geography"),
      choices = geography_choices,
      selected = values$geography
    )
    shiny::updateSelectInput(
      session,
      control_id("view"),
      selected = values$view
    )

    shiny::updateSelectInput(
      session,
      control_id("x_dataset"),
      choices = dataset_choices,
      selected = values$x_dataset
    )
    shiny::updateSelectInput(
      session,
      control_id("x_var"),
      choices = values$x_choices,
      selected = values$x_var
    )

    shiny::updateSelectInput(
      session,
      control_id("y_dataset"),
      choices = dataset_choices,
      selected = values$y_dataset
    )
    shiny::updateSelectInput(
      session,
      control_id("y_var"),
      choices = values$y_choices,
      selected = values$y_var
    )

    session$sendCustomMessage(
      "codec-sync-controls",
      list(values = stats::setNames(
        list(
          values$geography,
          values$view,
          values$x_dataset,
          values$x_var,
          values$y_dataset,
          values$y_var
        ),
        c(
          control_id("geography"),
          control_id("view"),
          control_id("x_dataset"),
          control_id("x_var"),
          control_id("y_dataset"),
          control_id("y_var")
        )
      ))
    )
  }

  queue_control_sync <- function(values, delays = c(0, 0.35, 1)) {
    for (delay in delays) {
      later::later(
        function() {
          sync_controls_from_values(values)
        },
        delay = delay
      )
    }
  }

  initialize_from_query <- function(query_string = "") {
      query <- shiny::parseQueryString(query_string %||% "")
      x_field <- query_value(query, "x", defaults$x, choices = numeric_fields)
      y_field <- query_value(query, "y", defaults$y, choices = numeric_fields)

      state <- normalize_state(list(
        geography = coalesce_string(
          query[["geography"]],
          default_state$geography
        ),
        view = query_value(
          query,
          "view",
          default_state$view,
          choices = c("bivariate", "x", "y")
        ),
        x_dataset = dataset_for_field(x_field),
        x = x_field,
        y_dataset = dataset_for_field(y_field),
        y = y_field
      ))

      app_state(state)
      canonical_query <- state_query(state)
      last_query(canonical_query)
      app_started(TRUE)
      ui_ready(FALSE)
      controls_live(FALSE)
      controls_interacted(FALSE)
      startup_values <- control_values_from_state(state)

      session$onFlushed(
        function() {
          shiny::updateQueryString(
            canonical_query,
            mode = "replace",
            session = session
          )
          later::later(
            function() {
              ui_ready(TRUE)
              queue_control_sync(startup_values, c(0.15, 0.6, 1.1))
            },
            delay = 1
          )
          later::later(
            function() {
              controls_live(TRUE)
            },
            delay = 1.5
          )
        },
        once = TRUE
      )
  }

  observeEvent(
    input$startup_query,
    {
      initialize_from_query(input$startup_query %||% "")
    },
    once = TRUE,
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(
    input$controls_interacted,
    {
      if (isTRUE(app_started()) && isTRUE(controls_live())) {
        controls_interacted(TRUE)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("geography")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      geography <- codec:::codec_explorer_normalize_geography(input[[control_id(
        "geography"
      )]])
      if (
        !(geography %in% geography_values) ||
          identical(geography, state$geography)
      ) {
        return()
      }
      state$geography <- geography
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("view")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      if (identical(input[[control_id("view")]], state$view)) {
        return()
      }
      state$view <- input[[control_id("view")]]
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("x_dataset")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      if (identical(input[[control_id("x_dataset")]], state$x_dataset)) {
        return()
      }
      state$x_dataset <- input[[control_id("x_dataset")]]
      state$x <- fields_for_dataset(state$x_dataset)[[1]]
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("y_dataset")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      if (identical(input[[control_id("y_dataset")]], state$y_dataset)) {
        return()
      }
      state$y_dataset <- input[[control_id("y_dataset")]]
      state$y <- fields_for_dataset(state$y_dataset)[[1]]
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("x_var")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      if (
        !(input[[control_id("x_var")]] %in% numeric_fields) ||
          identical(input[[control_id("x_var")]], state$x)
      ) {
        return()
      }
      state$x <- input[[control_id("x_var")]]
      state$x_dataset <- dataset_for_field(input[[control_id("x_var")]])
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input[[control_id("y_var")]],
    {
      if (!isTRUE(app_started()) || !isTRUE(controls_live()) || !isTRUE(controls_interacted())) {
        return()
      }
      state <- app_state()
      if (
        !(input[[control_id("y_var")]] %in% numeric_fields) ||
          identical(input[[control_id("y_var")]], state$y)
      ) {
        return()
      }
      state$y <- input[[control_id("y_var")]]
      state$y_dataset <- dataset_for_field(input[[control_id("y_var")]])
      set_state(state)
    },
    ignoreInit = TRUE
  )

  observeEvent(input[[control_id("reset_controls")]], {
    controls_live(FALSE)
    controls_interacted(TRUE)
    selected_legend(NULL)
    selected_geography(NULL)
    app_state(default_state)
    last_query(NULL)
    reset_values <- control_values_from_state(default_state)
    later::later(
      function() {
        queue_control_sync(reset_values, c(0, 0.25, 0.75))
        controls_live(TRUE)
      },
      delay = 0.25
    )
  })

  observeEvent(
    input$legend_selection,
    {
      selection <- input$legend_selection
      if (
        is.null(selection$view) ||
          is.null(selection$x) ||
          is.null(selection$y) ||
          is.null(selection$bin)
      ) {
        return()
      }

      next_selection <- list(
        view = selection$view,
        x = selection$x,
        y = selection$y,
        bin = selection$bin
      )

      if (identical(selected_legend(), next_selection)) {
        selected_legend(NULL)
      } else {
        selected_geography(NULL)
        selected_legend(next_selection)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$map_shape_click,
    {
      click <- input$map_shape_click
      if (is.null(click$id) || !nzchar(click$id)) {
        return()
      }

      if (identical(selected_geography(), click$id)) {
        selected_geography(NULL)
      } else {
        selected_legend(NULL)
        selected_geography(click$id)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    app_state(),
    {
      if (!isTRUE(app_started())) {
        return()
      }

      state <- app_state()
      query <- state_query(state)
      if (!identical(query, last_query())) {
        last_query(query)
        shiny::updateQueryString(
          query,
          mode = "replace",
          session = session
        )
      }
    },
    ignoreInit = TRUE
  )

  observe({
    req(app_started())
    req(ui_ready())
    values <- control_values_from_state(app_state())
    session$onFlushed(
      function() {
        queue_control_sync(values, c(0, 0.2))
      },
      once = TRUE
    )
  })

  current_state <- reactive({
    req(app_started())
    req(ui_ready())
    state <- app_state()
    list(
      geography = state$geography,
      view = state$view,
      x = state$x,
      y = state$y
    )
  })

  base_data <- reactive({
    req(app_started())
    req(ui_ready())
    cached_geography_data(current_state()$geography)
  })

  active_legend_selection <- reactive({
    selection <- selected_legend()
    state <- current_state()

    if (
      is.null(selection) ||
        !identical(selection$view, state$view) ||
        !identical(selection$x, state$x) ||
        !identical(selection$y, state$y)
    ) {
      return(NULL)
    }

    selection
  })

  legend_data <- reactive({
    map_data_for_view(
      base_data(),
      current_state()$x,
      current_state()$y,
      current_state()$view
    )
  })

  map_data <- reactive({
    map_data_for_view(
      base_data(),
      current_state()$x,
      current_state()$y,
      current_state()$view,
      active_legend_selection(),
      selected_geography()
    )
  })

  output$x_source_note <- renderUI({
    req(ui_ready())
    dataset_source_note_ui(app_state()$x_dataset)
  })

  output$y_source_note <- renderUI({
    req(ui_ready())
    dataset_source_note_ui(app_state()$y_dataset)
  })

  output$legend_panel <- renderUI({
    req(ui_ready())
    map_legend_ui(
      current_state()$view,
      current_state()$x,
      current_state()$y,
      legend_data(),
      active_legend_selection()
    )
  })

  output$map_detail_panel <- renderUI({
    req(ui_ready())
    state <- current_state()

    if (identical(state$view, "bivariate")) {
      return(NULL)
    }

    active_field <- if (identical(state$view, "x")) state$x else state$y
    tags$div(
      class = "codec-histogram",
      tags$div(
        class = "codec-histogram__title",
        paste(field_label(active_field), "distribution")
      ),
      plotlyOutput("histogram", height = "190px")
    )
  })

  output$histogram <- renderPlotly({
    req(ui_ready())
    state <- current_state()
    req(!identical(state$view, "bivariate"))

    active_field <- if (identical(state$view, "x")) state$x else state$y
    dat <- legend_data() |>
      sf::st_drop_geometry() |>
      dplyr::mutate(value = .data[[active_field]]) |>
      dplyr::filter(!is.na(.data$value), !is.na(.data$value_bin))

    bins <- seq_along(univariate_palette)
    bin_ranges <- lapply(
      bins,
      function(bin) {
        values <- dat$value[dat$value_bin == bin]
        if (!length(values)) {
          return(c(NA_real_, NA_real_))
        }
        range(values, na.rm = TRUE)
      }
    )

    shapes <- lapply(
      bins,
      function(bin) {
        rng <- bin_ranges[[bin]]
        if (any(is.na(rng))) {
          return(NULL)
        }
        list(
          type = "rect",
          xref = "x",
          yref = "paper",
          x0 = rng[[1]],
          x1 = rng[[2]],
          y0 = 0,
          y1 = 1,
          fillcolor = univariate_palette[[bin]],
          opacity = 0.16,
          line = list(width = 0),
          layer = "below"
        )
      }
    )
    shapes <- Filter(Negate(is.null), shapes)

    selected_geo_id <- selected_geography()
    selected <- active_legend_selection()
    if (!is.null(selected_geo_id)) {
      selected_dat <- dat |>
        dplyr::filter(.data$geo_id == selected_geo_id)
      selected_name <- if (nrow(selected_dat)) {
        selected_dat$geo_name[[1]]
      } else {
        "Selected geography"
      }
    } else if (!is.null(selected)) {
      selected_dat <- dat |>
        dplyr::filter(.data$value_bin == as.integer(selected$bin))
      selected_name <- paste("Selected bin", selected$bin)
    } else {
      selected_dat <- dat[0, , drop = FALSE]
      selected_name <- "Selected"
    }

    plot <- plotly::plot_ly(
      data = dat,
      x = ~value,
      type = "histogram",
      name = "All geographies",
      hoverinfo = "x+y",
      nbinsx = 28,
      marker = list(
        color = "#8cb4c3",
        opacity = 0.72,
        line = list(color = "#fffdfa", width = 0.5)
      )
    )

    if (nrow(selected_dat)) {
      plot <- plot |>
        plotly::add_markers(
          data = selected_dat,
          x = ~value,
          y = rep(0, nrow(selected_dat)),
          inherit = FALSE,
          name = selected_name,
          text = ~paste0(
            geo_name,
            "<br>",
            field_label(active_field),
            ": ",
            format_field_value(value, active_field)
          ),
          hoverinfo = "text",
          marker = list(
            color = "#c28273",
            size = if (nrow(selected_dat) == 1) 12 else 8,
            symbol = "triangle-up",
            line = list(color = "#1f2e38", width = 1)
          )
        )
    }

    plot |>
      plotly::layout(
        margin = list(l = 42, r = 16, t = 8, b = 44),
        paper_bgcolor = "#fffdfa",
        plot_bgcolor = "#fffdfa",
        xaxis = list(title = field_label(active_field), fixedrange = TRUE),
        yaxis = list(title = "geographies", fixedrange = TRUE),
        bargap = 0.04,
        shapes = shapes,
        showlegend = nrow(selected_dat) > 0
      ) |>
      plotly::config(displaylogo = FALSE)
  })

  output$map <- renderLeaflet({
    req(ui_ready())
    dat <- map_data()
    bounds <- sf::st_bbox(dat)
    center_lng <- unname(mean(c(bounds["xmin"], bounds["xmax"])))
    center_lat <- unname(mean(c(bounds["ymin"], bounds["ymax"])))

    leaflet::leaflet(
      dat,
      options = leaflet::leafletOptions(zoomControl = TRUE)
    ) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::setView(lng = center_lng, lat = center_lat, zoom = 10) |>
      leaflet::addPolygons(
        layerId = ~geo_id,
        color = ~map_color,
        weight = ~map_weight,
        opacity = 0.75,
        fillColor = ~fill_color,
        fillOpacity = ~map_fill_opacity,
        smoothFactor = 0.2,
        popup = ~popup_html,
        label = ~geo_name,
        highlightOptions = leaflet::highlightOptions(
          weight = 2.2,
          color = "#1f2e38",
          bringToFront = TRUE
        )
      )
  })
}

shinyApp(ui = ui, server = server)
