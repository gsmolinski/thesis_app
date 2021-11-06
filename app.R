library(shiny)
library(shinyalert)
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Code OE"),
  tabsetPanel(
    tabPanel(title = "Import",
             br(),
             import_file_ui("import")
    )
  )
)

server <- function(input, output, session) {
  imported_file <- import_file_server("import")
}

shinyApp(ui, server)