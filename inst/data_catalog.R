devtools::load_all()
library(shiny)
library(bslib)

library(dplyr)
library(sf)
library(cincy)

## pak::pak("qfes/rdeck@v0.5.2")
## library(rdeck)
## library(sf)

dpkgs <-
  list(
    environmental_justice_index =
      get_codec_dpkg("environmental_justice_index-v0.1.0") |>
        select(-year),
    hh_acs_measures =
      get_codec_dpkg("hh_acs_measures-v0.0.1") |>
        left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
        st_as_sf() |>
        interpolate(cincy::tract_tigris_2010) |>
        st_drop_geometry() |>
        tibble::as_tibble() |>
        select(-year),
    drivetime =
      get_codec_dpkg("drivetime-v0.2.2") |>
        select(-year),
    landcover =
      get_codec_dpkg("landcover-v0.1.0") |>
        select(-year),
    parcel =
      get_codec_dpkg("parcel-v0.1.0") |>
        select(-year) |>
        rename(n_parcel_violations = "n_violations"),
    traffic =
      get_codec_dpkg("traffic-v0.1.2") |>
        left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
        st_as_sf() |>
        interpolate(cincy::tract_tigris_2010) |>
        st_drop_geometry() |>
        tibble::as_tibble() |>
        select(-year),
    property_code_enforcements =
      get_codec_dpkg("property_code_enforcements-v0.1.0") |>
        slice_max(year, by = census_tract_id_2010) |>
        slice_max(month, by = census_tract_id_2010) |>
        select(-year, -month) |>
        rename(n_property_violations = "n_violations")
  )

snake_title <- function(x) {
  s <- strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
}

dpkgs_fields <-
  dpkgs |>
  purrr::map(names) |>
  purrr::map(\(.) setNames(., vapply(., snake_title, character(1))))

## c_d <-
##   paste(names(codec_dpkgs), codec_dpkgs, sep = "-") |>
##   lapply(get_codec_dpkg) |>
##   stats::setNames(names(codec_dpkgs))
## c_md <- lapply(c_d, dpkg::dpkg_meta)

ui <- page_sidebar(
  title = "CoDEC",
  sidebar = sidebar(
    selectInput(
      inputId = "codec_dpkg",
      label = "CoDEC Data Package",
      choices = names(dpkgs),
      ## selected = "prcnt_poverty",
      selectize = FALSE,
      size = NULL
    ),
    uiOutput("codec_dpkg_desc"),
    width = "33%"
  )
)

server <- function(input, output) {
  ## codec_var <- reactive(input$codec_var)
  codec_dpkg <- reactive(dpkgs[[input$codec_dpkg]])
  ## codec_dpkg <- reactive(purrr::keep(dpkgs, \(.) codec_var() %in% names(.))[[1]])
  output$codec_dpkg_desc <- renderUI({
    attr(codec_dpkg(), "description") |>
      markdown::markdownToHTML(fragment.only = TRUE) |>
      HTML()
  })
  ## d <- reactive(c_d[input$cdp])
  ## output$cdp_table <- renderTable(d())
  ## output$cdp_about <- renderText(c_md[input$cdp]$description, sep = "\n")
}

shinyApp(ui = ui, server = server)

## c_d <- c_d |>
##   purrr::reduce(dplyr::left_join, by = "census_tract_id_2010", .init = cincy::tract_tigris_2010) |>
##   tibble::as_tibble() |>
##   st_as_sf() |>
##   st_transform(st_crs(4326))


## add_codec_map_layer <- function(rdeck, col) {
##   rdeck |>
##     add_polygon_layer(
##       id = deparse(substitute(col)),
##       name = deparse(substitute(col)),
##       data = c_d,
##       get_polygon = geometry,
##       get_fill_color = scale_color_linear({{ col }}),
##       ## get_fill_color = scale_color_quantile({{ col }}, probs = seq(0, 1, 0.125)),
##       opacity = 0.3,
##       get_line_color = "#505050",
##       get_line_width = 10,
##       visible = FALSE,
##       ## get_elevation = n_children_lt18
##       pickable = TRUE,
##       auto_highlight = TRUE,
##       tooltip = c(census_tract_id_2010, {{ col }})
##     )
## }


## rdeck(map_style = mapbox_light(), initial_bounds = st_bbox(c_d)) |>
##   add_polygon_layer(
##     id = "census_tract", name = "census_tract", data = c_d,
##     get_polygon = geometry, get_fill_color = "#ffffff00",
##     get_line_color = "#505050", get_line_width = 10,
##     visible = TRUE, pickable = FALSE
##   ) |>
##   add_codec_map_layer(pct_green_2019) |>
##   add_codec_map_layer(pct_impervious_2019) |>
##   add_codec_map_layer(prcnt_area_within_1mi_high_volume_road)
