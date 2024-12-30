library(codec)
library(shiny)
library(bslib)

## library(dplyr)
## library(sf)
## library(cincy)

## pak::pak("qfes/rdeck@v0.5.2")
## library(rdeck)
## library(sf)

## dpkgs <- readRDS("inst/codec_catalog/all_codec_dpkg.rds")
dpkgs <- readRDS("all_codec_dpkg.rds")

names(dpkgs) <- vapply(dpkgs, \(.) dpkg::dpkg_meta(.)$title, character(1))

ui <- page_sidebar(
  title = "CoDEC",
  sidebar = sidebar(
    selectInput(
      inputId = "codec_dpkg",
      label = "CoDEC Data Package",
      choices = names(dpkgs),
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
