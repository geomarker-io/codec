library(codec)
library(shiny)
library(bslib)

dpkgs <- readRDS("codec_latest_annual.rds")

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
    downloadButton(
      "download_excel",
      "Download Excel",
      style = "width: 200px; padding: 3px 3px; font-size: 12px; text-align: center; background-color: #396175; color: #ffffff; border: none;"
    ),
    uiOutput("field_names"),
    width = "425px"
  ),
  uiOutput("codec_dpkg_desc")
)

server <- function(input, output) {
  codec_dpkg <- reactive(dpkgs[[input$codec_dpkg]])

  output$download_excel <-
    downloadHandler(
      filename = function() {
        paste0(
          "CoDEC-",
          dpkg::dpkg_meta(codec_dpkg())$name,
          "-v",
          dpkg::dpkg_meta(codec_dpkg())$version,
          ".xlsx"
        )
      },
      content = function(file) writexl::write_xlsx(codec_dpkg(), path = file)
    )

  output$field_names <-
    renderUI({
      tibble::tibble(`Field Names` = names(codec_dpkg())) |>
        knitr::kable() |>
        markdown::markdownToHTML(fragment.only = TRUE) |>
        HTML()
    })

  output$codec_dpkg_desc <- renderUI({
    attr(codec_dpkg(), "description") |>
      markdown::markdownToHTML(fragment.only = TRUE) |>
      HTML()
  })
}

shinyApp(ui = ui, server = server)
