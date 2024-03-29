library(codec)
library(fr)
library(shiny)
library(cincy)
library(bslib)
library(ggiraph)
library(tidyverse)
library(biscale)
library(cowplot)
library(ggExtra)
library(shinyWidgets)
library(leaflet)

{
codec_data_installed <-
  fs::path_package("codec") |>
  fs::path("codec_data") |>
  fs::dir_ls() |>
  fs::path_file()

codec_data_geography <- cincy::tract_tigris_2010
codec_data_geography_name <- names(codec_data_geography)[[1]]

d_prep <-
  purrr::map(
    codec_data_installed,
    \(x) codec::codec_data(name = x, geography = codec_data_geography),
    .progress = "interpolating codec data"
  ) |>
  purrr::set_names(codec_data_installed) 



core_titles <- 
  purrr::map(d_prep, as.list) |>
  purrr::map(\(x) purrr::pluck(x, "name")) |> #just use the core names here
  purrr::map(tibble::as_tibble) |> 
 # purrr::map(\(x) replace_na(x, purrr::pluck(x, "name"))) 
  bind_rows() |> 
  #tibble::add_row(value = "Hamilton County Drivetime", .after = 1) |> #hacky workaround to get drivetime in, give it a title
  rename(name = "value")# |> 
  #mutate(core_name = codec_data_installed)

drop_na_cols <- function(df){
  select(df, \(x) !all(is.na(x)))
}

schema_names <- d_prep |> 
  purrr::map_df(\(x) as.data.frame(x), .id = "schema_name") |> 
  select(-year, -census_tract_id_2010) |> 
  group_by(schema_name) |> 
  group_split() |> 
  purrr::map(tibble::as_tibble) |> 
  purrr::map(slice_head) |> 
  purrr::map(\(x) drop_na_cols(x)) |> 
  map(\(x) pivot_longer(x, cols = 2:last_col())) |> 
  bind_rows() |> 
  select(-value)

md <-
  purrr::map(d_prep, as.list) |>
  purrr::map(\(x) purrr::pluck(x, "schema", "fields")) |> 
  purrr::flatten() |> 
  purrr::map(tibble::as_tibble) |> 
  bind_rows() |> 
  filter(!name == 'year' & !type == 'string') |> 
  left_join(schema_names, by = 'name') |> 
  mutate(title = coalesce(title, name),
         description = replace_na(description, "No description available"))

d_prep$hh_acs_measures <- filter(as.data.frame(d_prep$hh_acs_measures), year == 2019)

d_all <- 
  d_prep |> 
  purrr::map(tibble::as_tibble) |>
  purrr::map(select, -year) |> 
  purrr::reduce(inner_join)

d_all <- 
  d_all |> 
  left_join(cincy::tract_tigris_2010 , by = 'census_tract_id_2010') |> 
  sf::st_as_sf() |> 
  sf::st_transform(4326)

codec_bi_pal <- c(
  "1-1" = "#eddcc1",
  "2-1" = "#d4aa92",
  "3-1" = "#bb7964",
  "1-2" = "#909992",
  "2-2" = "#81766f",
  "3-2" = "#71544c",
  "1-3" = "#375a66",
  "2-3" = "#31464d",
  "3-3" = "#2b3135"
)

codec_bi_pal_2 <- tibble(
  "1-1" = "#eddcc1",
  "2-1" = "#d4aa92",
  "3-1" = "#bb7964",
  "1-2" = "#909992",
  "2-2" = "#81766f",
  "3-2" = "#71544c",
  "1-3" = "#375a66",
  "2-3" = "#31464d",
  "3-3" = "#2b3135"
) |> 
  gather("group", "fill") |> 
  arrange(group)

uni_colors <- c(codec_colors()[1], "#567D91", "#789BAC", "#9FBAC8", "#CCDCE3", "#F6EDDE")

}
  



##----

ex_card <- card(
  card_header("Bivariate Map",
              actionBttn('legend_modal',
                         style = "material-circle",
                         #color = "primary",
                         label = NULL,
                         size = 'xs',
                         block = FALSE,
                         icon = icon("question-circle")) |> 
                tagAppendAttributes(style = "color: #C28273; background-color: #FFFFFF"),
              
              shinyWidgets::prettySwitch("big_plot",
                                         label = "Enlarge scatter plot",
                                         status = "primary") |> 
                tagAppendAttributes(style = "float: right"),
  ),
  layout_sidebar(
    fillable = TRUE,
    sidebar = 
      sidebar(
        div(img(src = "logo.svg", 
                width = "125px", height = "auto", style = "display: block; margin-left: auto; margin-right: auto;")),
        hr(),
        radioButtons(inputId = "sel_geo",
                     label = strong("Select your", a("geographic unit:", href = "https://geomarker.io/cincy/articles/geographies.html", target = "_blank")),
                     choiceNames = c("Census Tract", "Zip Code Tabulation Area", "Neighborhood"),
                     choiceValues = c("tract", 'zcta', 'neighborhood'),
                     selected = "tract"),
        
        checkboxGroupInput(inputId = "core",
                           label = strong("Select the CoDEC cores you would like to include:"),
                           choices = core_titles$name,
                           selected = c("tract_indices", 'hamilton_landcover')),
        layout_column_wrap(
          width = 1/2,
          actionButton('select_all', label = "Select All", style = "fill", color = "primary"),
          actionButton('deselect_all', label = "Deselect All", style = "fill", color = "primary"),
        ),
        hr(),
        uiOutput("x_sel"),
        uiOutput("y_sel"),
        shinyWidgets::prettySwitch("univariate_switch",
                                   label = "Univariate view",
                                   status = "primary") |> 
          tagAppendAttributes(style = "float: right"),
        hr(),
        htmlOutput('x_desc'),
        hr(),
        htmlOutput('y_desc'),
        width = '18%'
      ),
    leafletOutput("map"),
    uiOutput("plot_panel")
  )
)

ui <- page_fillable(
  theme = bs_theme(version = 5,
                   "bg" = "#FFFFFF",
                   "fg" = "#396175",
                   "primary" = "#C28273",
                   "grid-gutter-width" = "0.0rem",
                   "border-radius" = "0.5rem",
                   "btn-border-radius" = "0.25rem" ),
  
  tags$head(
    tags$style(type="text/css", "text {font-family: sans-serif}")),
  
  shinyjs::useShinyjs(),
  
  ex_card
)

server <- function(input, output, session) {
  

  
  d <- reactive({
    
    if (input$sel_geo == 'tract') {
      geo_option <- cincy::tract_tigris_2010
    } else if (input$sel_geo == 'zcta') {
      geo_option <- cincy::zcta_tigris_2010
    } else {
      geo_option <- cincy::neigh_cchmc_2010
    }
    
    
    temp <-
      purrr::map(
        codec_data_installed,
        \(x) codec::codec_data(name = x, geography = geo_option),
        .progress = "interpolating codec data"
      ) |>
      purrr::set_names(codec_data_installed)

    
    if(input$sel_geo == 'tract') {
      temp$hh_acs_measures <- filter(as.data.frame(temp$hh_acs_measures), year == 2019)
    }
    
    d <- 
      temp |> 
      purrr::map(tibble::as_tibble) |>
      purrr::map(select, -year) |> 
      purrr::reduce(inner_join)
    
    if (input$sel_geo == 'tract') {
      d <- d |> left_join(cincy::tract_tigris_2010)
    } else if (input$sel_geo == 'zcta') {
      d <- d |> left_join(cincy::zcta_tigris_2010)
    } else {
      d <- d |> left_join(cincy::neigh_cchmc_2010)
    }
    
    colnames(d)[1] <- 'geo_index'
    
    d <- 
      d |> 
      sf::st_as_sf() |> 
      sf::st_transform(crs = sf::st_crs(d_all))
    
    
  })

  
  observeEvent(input$univariate_switch, {
    
    if (input$univariate_switch == T) {
      shinyjs::disable('y_sel')
      shinyjs::hide(id = 'y_desc')
    } else {
      shinyjs::enable('y_sel')
      shinyjs::show('y_desc')
    }
    
  })
  
  
  d_sel_cores <- reactive({
    
    core_titles |> 
      filter(name %in% input$core) 
    
  })
  
  d_sel_metrics <- reactive({
    
    md |> 
      filter(schema_name %in% d_sel_cores()$name) 
    
  })
  
  
  
  output$x_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'x',
                              label = "X Variable",
                              choices = d_sel_metrics()$title,
                              multiple = FALSE,
                              selected = 'Percent Greenspace 2019',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
    
  })
  
  output$y_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'y',
                              label = "Y Variable", 
                              choices = d_sel_metrics()$title,
                              multiple = FALSE,
                              selected = 'Material Deprivation Index',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
  })
  
  
  
  xvar <- reactive({
    req(input$x)
    
    xvar <- md |> 
      filter(title == input$x) |> 
      pull(name)
    
    xvar
    
  })
  
  yvar <- reactive({
    req(input$y)
    
    yvar <- md |> 
      filter(title == input$y) |> 
      pull(name)
    
    yvar
    
  })
  
  output$x_desc <- renderText({
    req(input$x)
    
    paste0(strong(input$x), ": ", md |> filter(title == input$x) |> pull(description))
  })
  
  output$y_desc <- renderText({
    req(input$y)
    
    paste0(strong(input$y), ": ", md |> filter(title == input$y) |> pull(description))
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(inputId = 'core', selected = core_titles$title)
    
  })
  
  
  observeEvent(input$deselect_all, {
    
    updateCheckboxGroupInput(inputId = 'core', selected = "")
    
  })
  
  output$map <- renderLeaflet({
    
    
    req(input$x)
    
    if (input$univariate_switch == F) {
      
      
      bins_x <- pull(d(), xvar())
      bins_y <- pull(d(), yvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE))
      out <- out |> 
        mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE))
      out <- out|> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               xvar(), ": ", round(get(xvar()),2), "<br>",
                               yvar(), ": ", round(get(yvar()),2)))
      
      pal <- colorFactor(codec_bi_pal, factor(out$bi_class, levels = c("1-1","2-1","3-1",
                                                                       "1-2","2-2","3-2",
                                                                       "1-3","2-3","3-3")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d_all))
      
      map <- 
        leaflet(out) |>
        setView(-84.55, 39.18, zoom = 11.5) |>
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(bi_class), fillOpacity = 0.7, stroke = T,
                    label = ~lapply(out$out_lab, HTML),
                    weight = .5, color = "#333333") |>
        removeLayersControl()
      
      map  
    } else {
      
      bins_x <- pull(d(), xvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               xvar(), ": ", round(get(xvar()),2)))
      
      pal <- colorFactor(uni_colors, factor(out$x_class, levels = c("1", "2", "3",
                                                                    "4", "5", "6")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d_all))
      
      map <- 
        leaflet(out) |> 
        setView(-84.55, 39.18, zoom = 11.5) |> 
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(x_class), fillOpacity = 0.7, stroke = T, 
                    label = ~lapply(out$out_lab, HTML), 
                    weight = .5, color = "#333333") |> 
        removeLayersControl()
      
      map
    }
  })
  
  
  
  output$scatter <- renderGirafe({
    req(input$x)
    
    if (input$univariate_switch == F) {
      
      
      bins_x <- pull(d(), xvar())
      bins_y <- pull(d(), yvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out_scat <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE, labels = c("1", "2", "3")))
      out_scat <- out_scat |> 
        mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE, labels = c("1", "2", "3")))
      out_scat <- out_scat |> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      scatter_panels <- ggplot(out_scat, aes_string(x = xvar(), y = yvar())) +
        annotate("rect", 
                 xmin = -Inf, xmax = bins_x[2],  
                 ymin = -Inf, ymax = bins_y[2],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[1]) + 
        annotate("rect", 
                 xmin = -Inf, xmax = bins_x[2],  
                 ymin = bins_y[2], ymax = bins_y[3],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[2]) +
        annotate("rect", 
                 xmin = -Inf, xmax = bins_x[2], 
                 ymin = bins_y[3], ymax = Inf,
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[3]) + 
        annotate("rect", 
                 xmin = bins_x[2], xmax = bins_x[3], 
                 ymin = -Inf, ymax = bins_y[2],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[4]) + 
        annotate("rect", 
                 xmin = bins_x[2], xmax = bins_x[3],  
                 ymin = bins_y[2], ymax = bins_y[3],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[5]) + 
        annotate("rect", 
                 xmin = bins_x[2], xmax = bins_x[3],  
                 ymin = bins_y[3], ymax = Inf,
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[6]) + 
        annotate("rect", 
                 xmin = bins_x[3], xmax = Inf,  
                 ymin = -Inf, ymax = bins_y[2],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[7]) + 
        annotate("rect", 
                 xmin = bins_x[3], xmax = Inf, 
                 ymin = bins_y[2], ymax = bins_y[3],
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[8]) + 
        annotate("rect", 
                 xmin = bins_x[3], xmax = Inf,  
                 ymin = bins_y[3], ymax = Inf,
                 alpha = 1,
                 fill = codec_bi_pal_2$fill[9])
      
      
      scat <- scatter_panels +
        geom_point_interactive(data = d(), aes_string(x = xvar(), y = yvar(),
                                                      data_id = "geo_index"), 
                               fill = codec_colors()[7], 
                               alpha = .8,
                               shape = 21,
                               color = "grey20", 
                               stroke = .5) +
        theme_light() +
        theme(aspect.ratio = 1, title = element_text(size = 8),
              axis.title = element_text(size = if (input$big_plot == FALSE) {6} else {10}),
              legend.key.size = unit(3,"mm")) +
        labs(x = paste0(input$x), y = paste0(input$y))
      
      hist1 <- ggplot(d()) +
        geom_histogram_interactive(aes_string(x = xvar(), tooltip = "geo_index", 
                                              data_id = "geo_index"), 
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        theme_minimal()
      
      hist2 <- ggplot(d()) +
        geom_histogram_interactive(aes_string(x = yvar(), tooltip = "geo_index", 
                                              data_id = "geo_index"), 
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        coord_flip() + 
        theme_minimal()
      
      scat1 <- insert_xaxis_grob(scat, hist1, position = "bottom")
      scat2 <- insert_yaxis_grob(scat1, hist2, position = "right")
      
      finalScat <- ggdraw() +
        draw_plot(scat2) + #, 0, 0, 1, 1, vjust = -.2) 
        theme(plot.margin = margin(0,0,0,0))#
      
      gir_join <- girafe(ggobj = finalScat, 
                         width_svg = if (input$big_plot == FALSE) {3} else {6}, 
                         height_svg = if (input$big_plot == FALSE) {3} else {6},
                         options = list(opts_sizing(width = 1, rescale = T),
                                        opts_selection(type = "single")))
      
      gir_join
    } else {
      bins_x <- pull(d(), xvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out_scat <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      scatter_panels <- ggplot(out_scat, aes_string(x = xvar())) +
        annotate("rect", 
                 xmin = -Inf, xmax = bins_x[2],  
                 ymin = -Inf, ymax = Inf,
                 alpha = 1,
                 fill = codec_colors()[1]) + 
        annotate("rect", 
                 xmin = bins_x[2], xmax = bins_x[3], 
                 ymin = -Inf, ymax = Inf,  
                 alpha = 1,
                 fill = "#567D91") +
        annotate("rect", 
                 xmin = bins_x[3], xmax = bins_x[4],  
                 ymin = -Inf, ymax = Inf,
                 alpha = 1,
                 fill = "#789BAC") + 
        annotate("rect", 
                 xmin = bins_x[4], xmax = bins_x[5], 
                 ymin = -Inf, ymax = Inf,
                 alpha = 1,
                 fill = "#9FBAC8") + 
        annotate("rect", 
                 xmin = bins_x[5], xmax = bins_x[6], 
                 ymin = -Inf, ymax = Inf,
                 alpha = 1,
                 fill = "#CCDCE3") + 
        annotate("rect", 
                 xmin = bins_x[6], xmax = Inf,   
                 ymin = -Inf, ymax = Inf,
                 alpha = 1,
                 fill = "#F6EDDE") 
      
      scat <- scatter_panels +
        geom_histogram_interactive(d(), mapping = aes_string(x = xvar(), tooltip = "geo_index",
                                                             data_id = "geo_index"),
                                   bins = 20,
                                   alpha = .6,
                                   fill = "grey70", 
                                   color = "grey50") +
        theme_light() +
        theme(aspect.ratio = 1, title = element_text(size = 8),
              axis.title = element_text(size = if (input$big_plot == FALSE) {6} else {10}),
              legend.key.size = unit(3,"mm")) +
        labs(x = paste0(input$x), y = "") 
      
      gir_join <- girafe(ggobj = scat, 
                         width_svg = if (input$big_plot == FALSE) {3} else {6}, 
                         height_svg = if (input$big_plot == FALSE) {3} else {6},
                         options = list(opts_sizing(width = 1, rescale = T),
                                        opts_selection(type = "single")))
      
      gir_join
      
    }
    
  })
  
  d_scat_click <- reactiveVal()
  scat_click <- reactiveVal()
  
  observeEvent(input$scatter_selected, {
    
    if (input$univariate_switch == F) {
      
      
      scat_click <- c(input$scatter_selected)
      
      d_scat_click <- d() |> 
        filter(geo_index == scat_click) 
      
      
      bins_x <- pull(d(), xvar())
      bins_y <- pull(d(), yvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE))
      out <- out |> 
        mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE))
      out <- out|> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               xvar(), ": ", round(get(xvar()),2), "<br>",
                               yvar(), ": ", round(get(yvar()),2)))
      
      pal <- colorFactor(codec_bi_pal, factor(out$bi_class, levels = c("1-1","2-1","3-1",
                                                                       "1-2","2-2","3-2",
                                                                       "1-3","2-3","3-3")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d_all))
      d_scat_click <- sf::st_transform(d_scat_click, crs = sf::st_crs(d_all))
      
      map <- 
        leafletProxy("map", data = out) |> 
        clearShapes() |> 
        setView(-84.55, 39.18, zoom = 11.5) |> 
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(bi_class), fillOpacity = 0.7, stroke = T, 
                    label = ~lapply(out$out_lab, HTML), 
                    weight = .5, color = "#333333") |> 
        addPolygons(data = d_scat_click, color = "#FFF", stroke = T, weight = 5, opacity = 1) |> 
        removeLayersControl()
      
      map
    } else {
      
      scat_click <- c(input$scatter_selected)
      
      d_scat_click <- d() |> 
        filter(geo_index == scat_click) 
      
      
      bins_x <- pull(d(), xvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               xvar(), ": ", round(get(xvar()),2)))
      
      
      
      pal <- colorFactor(uni_colors, factor(out$x_class, levels = c("1", "2", "3",
                                                                    "4", "5", "6")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d()))
      d_scat_click <- sf::st_transform(d_scat_click, crs = sf::st_crs(d()))
      
      map <- 
        leafletProxy("map", data = out) |> 
        clearShapes() |> 
        setView(-84.55, 39.18, zoom = 11.5) |> 
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(x_class), fillOpacity = 0.7, stroke = T, 
                    label = ~lapply(out$out_lab, HTML), 
                    weight = .5, color = "#333333") |> 
        addPolygons(data = d_scat_click, color = "#FFF", stroke = T, weight = 5, opacity = 1) |> 
        removeLayersControl()
      
      map
    }
    
  })
  
  d_selected <- reactiveVal()
  
  observeEvent(input$map_click, {
    
    map_click <- reactiveVal()
    map_click <- input$map_shape_click
    
    
    click <- tibble(lng = map_click$lng, lat = map_click$lat) |> 
      sf::st_as_sf(coords= c('lng', 'lat'), crs = sf::st_crs(d_all))
    
    d_selected <- d() |> 
      sf::st_join(click, left = FALSE)
    
    output$scatter <- renderGirafe({
      req(input$x)
      
      if (input$univariate_switch == F) {
        
        bins_x <- pull(d(), xvar())
        bins_y <- pull(d(), yvar())
        
        bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
        bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
        
        bins_x <- bins_x$brks
        bins_y <- bins_y$brks
        
        # cut into groups defined above
        out_scat <- d() |> 
          mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE, labels = c("1", "2", "3")))
        out_scat <- out_scat |> 
          mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE, labels = c("1", "2", "3")))
        out_scat <- out_scat |> 
          mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
        
        scatter_panels <- ggplot(out_scat, aes_string(x = xvar(), y = yvar())) +
          annotate("rect", 
                   xmin = -Inf, xmax = bins_x[2],  
                   ymin = -Inf, ymax = bins_y[2],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[1]) + 
          annotate("rect", 
                   xmin = -Inf, xmax = bins_x[2],  
                   ymin = bins_y[2], ymax = bins_y[3],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[2]) +
          annotate("rect", 
                   xmin = -Inf, xmax = bins_x[2], 
                   ymin = bins_y[3], ymax = Inf,
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[3]) + 
          annotate("rect", 
                   xmin = bins_x[2], xmax = bins_x[3], 
                   ymin = -Inf, ymax = bins_y[2],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[4]) + 
          annotate("rect", 
                   xmin = bins_x[2], xmax = bins_x[3],  
                   ymin = bins_y[2], ymax = bins_y[3],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[5]) + 
          annotate("rect", 
                   xmin = bins_x[2], xmax = bins_x[3],  
                   ymin = bins_y[3], ymax = Inf,
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[6]) + 
          annotate("rect", 
                   xmin = bins_x[3], xmax = Inf,  
                   ymin = -Inf, ymax = bins_y[2],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[7]) + 
          annotate("rect", 
                   xmin = bins_x[3], xmax = Inf, 
                   ymin = bins_y[2], ymax = bins_y[3],
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[8]) + 
          annotate("rect", 
                   xmin = bins_x[3], xmax = Inf,  
                   ymin = bins_y[3], ymax = Inf,
                   alpha = 1,
                   fill = codec_bi_pal_2$fill[9])
        
        scat <- scatter_panels +
          geom_point_interactive(data = d(), aes_string(x = xvar(), y = yvar(),
                                                        data_id = "geo_index"),
                                 fill = codec_colors()[7], 
                                 alpha = .8,
                                 shape = 21,
                                 color = "grey20", 
                                 stroke = .5) +
          geom_point_interactive(data = d_selected,
                                 aes_string(x = xvar(), y = yvar(),
                                            data_id = "geo_index"),
                                 #  tooltip = paste0(
                                 #   input$x, ": ", xvar(), "\n",
                                 #    input$y, ": ", yvar()
                                 #   )),
                                 color = codec_colors()[1], size = 3, alpha = .6) +
          theme_light() +
          theme(aspect.ratio = 1, title = element_text(size = 8),
                axis.title = element_text(size = if (input$big_plot == FALSE) {6} else {10}),
                legend.key.size = unit(3,"mm")) +
          labs(x = paste0(input$x), y = paste0(input$y))
        
        hist1 <- ggplot(d()) +
          geom_histogram_interactive(aes_string(x = xvar(), tooltip = "geo_index",
                                                data_id = "geo_index"),
                                     fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
          theme_minimal()
        
        hist2 <- ggplot(d()) +
          geom_histogram_interactive(aes_string(x = yvar(), tooltip = "geo_index",
                                                data_id = "geo_index"),
                                     fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
          coord_flip() +
          theme_minimal()
        
        scat1 <- insert_xaxis_grob(scat, hist1, position = "bottom")
        scat2 <- insert_yaxis_grob(scat1, hist2, position = "right")
        
        finalScat <- ggdraw() +
          draw_plot(scat2) + #, 0, 0, 1, 1, vjust = -.2)
          theme(plot.margin = margin(0,0,0,0))#
        
        gir_join <- girafe(ggobj = finalScat, 
                           width_svg = if (input$big_plot == FALSE) {3} else {6}, 
                           height_svg = if (input$big_plot == FALSE) {3} else {6},
                           options = list(opts_sizing(width = 1, rescale = T),
                                          opts_selection(type = "single")))
        gir_join
        
      } else {
        
        bins_x <- pull(d(), xvar())
        
        bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
        
        bins_x <- bins_x$brks
        
        # cut into groups defined above
        out_scat <- d() |> 
          mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE)) |> 
          mutate(x_class = paste0(as.numeric(bi_x)))
        
        scatter_panels <- ggplot(out_scat, aes_string(x = xvar())) +
          annotate("rect", 
                   xmin = -Inf, xmax = bins_x[2],  
                   ymin = -Inf, ymax = Inf,
                   alpha = 1,
                   fill = codec_colors()[1]) + 
          annotate("rect", 
                   xmin = bins_x[2], xmax = bins_x[3], 
                   ymin = -Inf, ymax = Inf,  
                   alpha = 1,
                   fill = "#567D91") +
          annotate("rect", 
                   xmin = bins_x[3], xmax = bins_x[4],  
                   ymin = -Inf, ymax = Inf,
                   alpha = 1,
                   fill = "#789BAC") + 
          annotate("rect", 
                   xmin = bins_x[4], xmax = bins_x[5], 
                   ymin = -Inf, ymax = Inf,
                   alpha = 1,
                   fill = "#9FBAC8") + 
          annotate("rect", 
                   xmin = bins_x[5], xmax = bins_x[6], 
                   ymin = -Inf, ymax = Inf,
                   alpha = 1,
                   fill = "#CCDCE3") + 
          annotate("rect", 
                   xmin = bins_x[6], xmax = Inf,   
                   ymin = -Inf, ymax = Inf,
                   alpha = 1,
                   fill = "#F6EDDE") 
        
        scat <- scatter_panels +
          geom_histogram_interactive(d(), mapping = aes_string(x = xvar(), tooltip = "geo_index",
                                                               data_id = "geo_index"),
                                     bins = 20,
                                     alpha = .6,
                                     fill = "grey70", 
                                     color = "grey50") +
          geom_segment(d_selected, 
                       mapping = aes_string(
                         x = xvar(), 
                         xend = xvar(),
                         y = -1,
                         yend = 0),
                       arrow = arrow(length = unit(1, "mm"), type = "closed"),
                       color = "black") +
          theme_light() +
          theme(aspect.ratio = 1, title = element_text(size = 8),
                axis.title = element_text(size = if (input$big_plot == FALSE) {6} else {10}),
                legend.key.size = unit(3,"mm")) +
          labs(x = paste0(input$x), y = "") 
        
        
        gir_join <- girafe(ggobj = scat, 
                           width_svg = if (input$big_plot == FALSE) {3} else {6}, 
                           height_svg = if (input$big_plot == FALSE) {3} else {6},
                           options = list(opts_sizing(width = 1, rescale = T),
                                          opts_selection(type = "single")))
        
        gir_join
        
      }
    })
    
    
  })
  
  output$legend <- renderPlot({
    
    legend <- bi_legend(pal = codec_bi_pal,
                        dim = 3,
                        xlab = paste0("Higher X Variable"),
                        ylab = paste0("Higher Y Variable"),
                        size = 12)
    
    legend
  })
  
  output$plot_panel <- renderUI({
    absolutePanel(id = "plot_panel",
                  class = "panel panel-default",
                  cursor = "auto",
                  draggable = TRUE,
                  top = 100,
                  height = if (input$big_plot == FALSE) {"400px"} else { "900px"},
                  right = 20,
                  width = if (input$big_plot == FALSE) {"400px"} else { "1000px"},
                  style =
                    "padding: 5px;
                         border: 1px solid #000;
                         background: #FFFFFF;
                         opacity: .9;
                         margin: auto;
                         border-radius: 5pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);",
                  fixedRow(girafeOutput("scatter", 
                                        height = if (input$big_plot == FALSE) {"350px"} else { "800px"}, 
                                        width = if (input$big_plot == FALSE) {"350px"} else { "800px"})))
    
  })
  
  observeEvent(input$legend_modal, {
    showModal(
      modalDialog(
        title = "About Bivariate Maps",
        p("Bivariate maps use a blended color scale to visualize two variables at the same time"),
        plotOutput("legend"),
        easyClose = TRUE
      )
    )
  })
  
  
  
}

shinyApp(ui, server)


