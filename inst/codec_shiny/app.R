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
library(sf)

devtools::load_all()

{
  #----
# codec_data_installed <-
#   fs::path_package("codec") |>
#   fs::path("codec_data") |>
#   fs::dir_ls() |>
#   fs::path_file()
# 
# codec_data_geography <- cincy::tract_tigris_2010
# codec_data_geography_name <- names(codec_data_geography)[[1]]
# 
# d_prep <-
#   purrr::map(
#     codec_data_installed,
#     \(x) codec::codec_data(name = x, geography = codec_data_geography),
#     .progress = "interpolating codec data"
#   ) |>
#   purrr::set_names(codec_data_installed) 
# 
# 
# 
# core_titles <- 
#   purrr::map(d_prep, as.list) |>
#   purrr::map(\(x) purrr::pluck(x, "name")) |> #just use the core names here
#   purrr::map(tibble::as_tibble) |> 
#  # purrr::map(\(x) replace_na(x, purrr::pluck(x, "name"))) 
#   bind_rows() |> 
#   #tibble::add_row(value = "Hamilton County Drivetime", .after = 1) |> #hacky workaround to get drivetime in, give it a title
#   rename(name = "value")# |> 
#   #mutate(core_name = codec_data_installed)
# 
# drop_na_cols <- function(df){
#   select(df, \(x) !all(is.na(x)))
# }
# 
# schema_names <- d_prep |> 
#   purrr::map_df(\(x) as.data.frame(x), .id = "schema_name") |> 
#   select(-year, -census_tract_id_2010) |> 
#   group_by(schema_name) |> 
#   group_split() |> 
#   purrr::map(tibble::as_tibble) |> 
#   purrr::map(slice_head) |> 
#   purrr::map(\(x) drop_na_cols(x)) |> 
#   map(\(x) pivot_longer(x, cols = 2:last_col())) |> 
#   bind_rows() |> 
#   select(-value)
# 
# md <-
#   purrr::map(d_prep, as.list) |>
#   purrr::map(\(x) purrr::pluck(x, "schema", "fields")) |> 
#   purrr::flatten() |> 
#   purrr::map(tibble::as_tibble) |> 
#   bind_rows() |> 
#   filter(!name == 'year' & !type == 'string') |> 
#   left_join(schema_names, by = 'name') |> 
#   mutate(title = coalesce(title, name),
#          description = replace_na(description, "No description available"))
# 
# d_prep$hh_acs_measures <- filter(as.data.frame(d_prep$hh_acs_measures), year == 2019)
# 
# d_all <- 
#   d_prep |> 
#   purrr::map(tibble::as_tibble) |>
#   purrr::map(select, -year) |> 
#   purrr::reduce(inner_join)
# 
# d_all <- 
#   d_all |> 
#   left_join(cincy::tract_tigris_2010 , by = 'census_tract_id_2010') |> 
#   sf::st_as_sf() |> 
#   sf::st_transform(4326)
  #----
  dpkgs <-
    list(
      environmental_justice_index =
        get_codec_dpkg("environmental_justice_index-v0.1.0"),
      hh_acs_measures =
        get_codec_dpkg("hh_acs_measures-v0.0.1") |>
        dplyr::left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
        st_as_sf() |>
        interpolate(cincy::tract_tigris_2010) |>
        st_drop_geometry() |>
        tibble::as_tibble(),
      drivetime =
        get_codec_dpkg("drivetime-v0.2.2"),
      landcover =
        get_codec_dpkg("landcover-v0.1.0"),
      parcel = 
        get_codec_dpkg("parcel-v0.1.0"),
      traffic =
        get_codec_dpkg("traffic-v0.1.2") |>
        dplyr::left_join(cincy::tract_tigris_2020, by = "census_tract_id_2020") |>
        st_as_sf() |>
        interpolate(cincy::tract_tigris_2010) |>
        st_drop_geometry() |>
        tibble::as_tibble()
    )
  
  tracts_sf <- cincy::tract_tigris_2010
  
  d_all <-
    purrr::reduce(dpkgs, dplyr::left_join, by = "census_tract_id_2010", .init = tracts_sf) |>
    tibble::as_tibble() |>
    st_as_sf() |>
    st_transform(st_crs(4326))
  
  badges <-
    tibble::tibble(
      dpkgs = 
        c('drivetime', 'environmental_justice_index', 'hh_acs_measures', 'landcover', 'parcel', 'traffic'),
      badge = 
        c("[![latest github release for drivetime dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=drivetime-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/CODECtools/releases?q=drivetime&expanded=false)",
          "[![latest github release for environmental_justice_index dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=environmental_justice_index-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=environmental_justice_index&expanded=false)",
          "[![latest github release for hh_acs_measures dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=hh_acs_measures-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=hh_acs_measures&expanded=false)",
          "[![latest github release for landcover dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=landcover-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=landcover&expanded=false)",
          "[![latest github release for parcel dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=parcel-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=parcel&expanded=false)",
          "[![latest github release for traffic dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=traffic-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=traffic&expanded=false)"
        )
    )
      
  
  

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
  card_header("CoDEC Explorer",
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
        
        checkboxGroupInput(inputId = "sel_dpkgs",
                           label = strong("Select the CoDEC data packages you would like to include:"),
                           choiceValues = names(dpkgs),
                           choiceNames = 
                             c("Environmental Justice Index",
                               "Harmonized Historical ACS Measures", 
                               "Drivetime", 
                               "Landcover",
                               "Parcel",
                               "Traffic"),
                           selected = c("hh_acs_measures")),
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
        uiOutput('badge_table'),
        hr(),
        width = '18%'
      ),
    leafletOutput("map"),
    uiOutput("plot_panel"),
    uiOutput("clear_button_panel")
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

     if (input$sel_geo == 'zcta') {
       d <- d_all |> 
         cincy::interpolate(to = cincy::zcta_tigris_2010)
     } else if (input$sel_geo == 'neighborhood') {
       d <- d_all |> 
         cincy::interpolate(to = cincy::neigh_cchmc_2010)
     } else {
       d <- d_all
     }
     
     #colnames(d)[1] <- 'geo_index'
     
     d <-
       d |>
       dplyr::rename("geo_index" = 1) |> 
       sf::st_as_sf() |>
       sf::st_transform(crs = sf::st_crs(d_all))
     
   })
     
  # ----
  #   temp <-
  #     purrr::map(
  #       codec_data_installed,
  #       \(x) codec::codec_data(name = x, geography = geo_option),
  #       .progress = "interpolating codec data"
  #     ) |>
  #     purrr::set_names(codec_data_installed)
  # 
  # 
  #   if(input$sel_geo == 'tract') {
  #     temp$hh_acs_measures <- filter(as.data.frame(temp$hh_acs_measures), year == 2019)
  #   }
  # 
  #   d <-
  #     temp |>
  #     purrr::map(tibble::as_tibble) |>
  #     purrr::map(select, -year) |>
  #     purrr::reduce(inner_join)
  # 
  #   if (input$sel_geo == 'tract') {
  #     d <- d |> left_join(cincy::tract_tigris_2010)
  #   } else if (input$sel_geo == 'zcta') {
  #     d <- d |> left_join(cincy::zcta_tigris_2010)
  #   } else {
  #     d <- d |> left_join(cincy::neigh_cchmc_2010)
  #   }
  # 
  #   colnames(d)[1] <- 'geo_index'
  # 
  #   d <-
  #     d |>
  #     sf::st_as_sf() |>
  #     sf::st_transform(crs = sf::st_crs(d))
  # 
  # 
  # })
    #----

  
  observeEvent(input$univariate_switch, {
    
    if (input$univariate_switch == T) {
      shinyjs::disable('y_sel')
    } else {
      shinyjs::enable('y_sel')
    }
    
  })
  
  
  d_sel_dpkgs <- reactive({
    
    dpkgs[input$sel_dpkgs] 
    
  })
  
  d_avail_vars <- reactive({
    
    vars <- unlist(purrr::map(d_sel_dpkgs(), colnames)) 
    
    vars <- vars[!vars %in% c('census_tract_id_2010', 'year')]
    
    named_vars <- set_names(vars, str_to_title(str_replace_all(vars, "_", " ")))
    
    
  })
  
  #tools::toTitleCase(str_replace_all(names(dpkgs), "_", " ")),
  
  
  output$x_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'x',
                              label = "X Variable",
                              choices = d_avail_vars(),
                              multiple = FALSE,
                              selected = 'prcnt_poverty',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
    
  })
  
  output$y_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'y',
                              label = "Y Variable", 
                              choices = d_avail_vars(),
                              multiple = FALSE,
                              selected = 'median_home_value',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
  })
  
  
  # 
  # xvar <- reactive({
  #   req(input$x)
  # 
  #   # xvar <- md |>
  #   #   filter(title == input$x) |>
  #   #   pull(name)
  #   
  #   #go here to insert full name of variable
  # 
  #   xvar <- input$x
  # 
  # })

#   yvar <- reactive({
#     req(input$y)
# # 
# #     yvar <- md |>
# #       filter(title == input$y) |>
# #       pull(name)
# 
#     yvar <- input$y
# 
#   })
  
  output$badge_table <- renderUI({
    req(d_sel_dpkgs)
    
    badges |>
      dplyr::filter(dpkgs %in% names(d_sel_dpkgs())) |>
      gt::gt() |>
      gt::tab_header(
        title = "Links to data package READMEs") |> 
      gt::tab_options(column_labels.hidden = TRUE) |> 
      gt::fmt_markdown()
    
  })

  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(inputId = 'sel_dpkgs', selected = names(dpkgs))
    
  })
  
  
  observeEvent(input$deselect_all, {
    
    updateCheckboxGroupInput(inputId = 'sel_dpkgs', selected = "")
    
  })
  
  output$map <- renderLeaflet({
    
    
    req(input$x)
    
    if (input$univariate_switch == F) {
      
      
      bins_x <- pull(d(), input$x)
      bins_y <- pull(d(), input$y)
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE))
      out <- out |> 
        mutate(bi_y = cut(get(input$y), breaks = bins_y, include.lowest = TRUE))
      out <- out|> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               input$x, ": ", round(get(input$x),2), "<br>",
                               input$y, ": ", round(get(input$y),2)))
      
      pal <- colorFactor(codec_bi_pal, factor(out$bi_class, levels = c("1-1","2-1","3-1",
                                                                       "1-2","2-2","3-2",
                                                                       "1-3","2-3","3-3")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d()))
      
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
      
      bins_x <- pull(d(), input$x)
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               input$x, ": ", round(get(input$x),2)))
      
      pal <- colorFactor(uni_colors, factor(out$x_class, levels = c("1", "2", "3",
                                                                    "4", "5", "6")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d()))
      
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
      
      
      bins_x <- pull(d(), input$x)
      bins_y <- pull(d(), input$y)
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out_scat <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE, labels = c("1", "2", "3")))
      out_scat <- out_scat |> 
        mutate(bi_y = cut(get(input$y), breaks = bins_y, include.lowest = TRUE, labels = c("1", "2", "3")))
      out_scat <- out_scat |> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      scatter_panels <- ggplot(out_scat, aes_string(x = input$x, y = input$y)) +
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
        geom_point_interactive(data = d(), aes_string(x = input$x, y = input$y,
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
        geom_histogram_interactive(aes_string(x = input$x, tooltip = "geo_index", 
                                              data_id = "geo_index"), 
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        theme_minimal()
      
      hist2 <- ggplot(d()) +
        geom_histogram_interactive(aes_string(x = input$y, tooltip = "geo_index", 
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
      bins_x <- pull(d(), input$x)
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out_scat <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      scatter_panels <- ggplot(out_scat, aes_string(x = input$x)) +
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
        geom_histogram_interactive(d(), mapping = aes_string(x = input$x, tooltip = "geo_index",
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
      
      
      bins_x <- pull(d(), input$x)
      bins_y <- pull(d(), input$y)
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE))
      out <- out |> 
        mutate(bi_y = cut(get(input$y), breaks = bins_y, include.lowest = TRUE))
      out <- out|> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               input$x, ": ", round(get(input$x),2), "<br>",
                               input$y, ": ", round(get(input$y),2)))
      
      pal <- colorFactor(codec_bi_pal, factor(out$bi_class, levels = c("1-1","2-1","3-1",
                                                                       "1-2","2-2","3-2",
                                                                       "1-3","2-3","3-3")))
      
      out <- sf::st_transform(out, crs = sf::st_crs(d()))
      d_scat_click <- sf::st_transform(d_scat_click, crs = sf::st_crs(d()))
      
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
      
      
      bins_x <- pull(d(), input$x)
      
      bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
      
      bins_x <- bins_x$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE)) |> 
        mutate(x_class = paste0(as.numeric(bi_x)))
      
      out <- out |> 
        mutate(out_lab = paste(geo_index, "<br>",
                               input$x, ": ", round(get(input$x),2)))
      
      
      
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
      sf::st_as_sf(coords= c('lng', 'lat'), crs = sf::st_crs(d()))
    
    d_selected <- d() |> 
      sf::st_join(click, left = FALSE)
    
    output$scatter <- renderGirafe({
      req(input$x)
      
      if (input$univariate_switch == F) {
        
        bins_x <- pull(d(), input$x)
        bins_y <- pull(d(), input$y)
        
        bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
        bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
        
        bins_x <- bins_x$brks
        bins_y <- bins_y$brks
        
        # cut into groups defined above
        out_scat <- d() |> 
          mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE, labels = c("1", "2", "3")))
        out_scat <- out_scat |> 
          mutate(bi_y = cut(get(input$y), breaks = bins_y, include.lowest = TRUE, labels = c("1", "2", "3")))
        out_scat <- out_scat |> 
          mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
        
        scatter_panels <- ggplot(out_scat, aes_string(x = input$x, y = input$y)) +
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
          geom_point_interactive(data = d(), aes_string(x = input$x, y = input$y,
                                                        data_id = "geo_index"),
                                 fill = codec_colors()[7], 
                                 alpha = .8,
                                 shape = 21,
                                 color = "grey20", 
                                 stroke = .5) +
          geom_point_interactive(data = d_selected,
                                 aes_string(x = input$x, y = input$y,
                                            data_id = "geo_index"),
                                 #  tooltip = paste0(
                                 #   input$x, ": ", input$x, "\n",
                                 #    input$y, ": ", input$y
                                 #   )),
                                 color = "#FFF", size = 3, alpha = .6) +
          theme_light() +
          theme(aspect.ratio = 1, title = element_text(size = 8),
                axis.title = element_text(size = if (input$big_plot == FALSE) {6} else {10}),
                legend.key.size = unit(3,"mm")) +
          labs(x = paste0(input$x), y = paste0(input$y))
        
        hist1 <- ggplot(d()) +
          geom_histogram_interactive(aes_string(x = input$x, tooltip = "geo_index",
                                                data_id = "geo_index"),
                                     fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
          theme_minimal()
        
        hist2 <- ggplot(d()) +
          geom_histogram_interactive(aes_string(x = input$y, tooltip = "geo_index",
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
        
        bins_x <- pull(d(), input$x)
        
        bins_x <- classInt::classIntervals(bins_x, n = 6, style = "quantile")
        
        bins_x <- bins_x$brks
        
        # cut into groups defined above
        out_scat <- d() |> 
          mutate(bi_x = cut(get(input$x), breaks = bins_x, include.lowest = TRUE)) |> 
          mutate(x_class = paste0(as.numeric(bi_x)))
        
        scatter_panels <- ggplot(out_scat, aes_string(x = input$x)) +
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
          geom_histogram_interactive(d(), 
                                     mapping = aes_string(x = input$x, tooltip = "geo_index",
                                                          data_id = "geo_index"),
                                     bins = 20,
                                     alpha = .6,
                                     fill = "grey70", 
                                     color = "grey50") +
          geom_segment(d_selected, 
                       mapping = aes_string(
                         x = input$x, 
                         xend = input$x,
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
                    "z-index: 10;
                     padding: 5px;
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
  
  output$clear_button_panel <- renderUI({
    absolutePanel(id = "clear_button_panel",
                  class = "panel panel-default",
                  cursor = "auto",
                  draggable = TRUE,
                  top = 50,
                  right = 20,
                  style =
                    "z-index: 10;
                     padding: 5px;
                         border: 1px solid #000;
                         background: #FFFFFF;
                         opacity: .9;
                         margin: auto;
                         border-radius: 5pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);",
                  fixedRow(
                    shinyWidgets::actionBttn("clear_map_selection",
                                             label = "Reset map",
                                             size = 'xs',
                                             style = 'simple',
                                             status = "primary") |> 
                      tagAppendAttributes(style = "color: #FFF; background-color: #396175;"),)
    )
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
  
  observeEvent(input$clear_map_selection, {
    
    map <- 
      leafletProxy("map", data = d()) |> 
      clearShapes() |> 
      setView(-84.55, 39.18, zoom = 11.5) |> 
      addProviderTiles(provider = providers$CartoDB.Positron) |> 
      addPolygons(fillColor = "#fff", 
                  opacity = .4, 
                  color = "#333333",
                  weight = .5
                  )
    
    map
    
  })
  
  
  
}

shinyApp(ui, server)


