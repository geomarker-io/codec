library(codec)
library(shiny)
library(bslib)
library(leaflet)

#' dpkgs <- readRDS("inst/codec_catalog/all_codec_dpkg.rds")
dpkgs <- readRDS("all_codec_dpkg.rds")

names(dpkgs) <- vapply(dpkgs, \(.) dpkg::dpkg_meta(.)$title, character(1))

ui <- page_sidebar(
  title = "CoDEC",
  sidebar = sidebar(
    selectInput(
      inputId = "codec_dpkg",
      label = "CoDEC Data Package",
      selected = "American Community Survey Measures",
      choices = names(dpkgs),
      selectize = FALSE,
      size = NULL
    ),
    uiOutput("codec_dpkg_desc"),
    downloadButton("download_excel", "Download Excel"),
    width = "30%"
  ),
  tableOutput("data_table")
)

server <- function(input, output) {
  codec_dpkg <- reactive(dpkgs[[input$codec_dpkg]])

  output$download_excel <-
    downloadHandler(
      filename = function() {
        paste0(
          "CoDEC-",
          dpkg::dpkg_meta(codec_dpkg())$name,
          "-v", dpkg::dpkg_meta(codec_dpkg())$version, ".xlsx"
        )
      },
      content = function(file) readxl::write_xlsx(codec_dpkg(), path = file)
    )

  output$data_table <- renderTable(head(codec_dpkg(), n = 25))

  output$codec_dpkg_desc <- renderUI({
    attr(codec_dpkg(), "description") |>
      markdown::markdownToHTML(fragment.only = TRUE) |>
      HTML()
  })
}

shinyApp(ui = ui, server = server)
