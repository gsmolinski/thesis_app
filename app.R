library(shiny)
library(shinyalert)
library(DBI)

ui <- fluidPage(
  useShinyalert(),
  titlePanel("Code OE"),
  tabsetPanel(
    tabPanel(title = "Import",
             br(),
             fluidRow(
               column(3, import_file_ui("import")),
               column(3, offset = 2, choose_existing_project_ui("existing_project"))
             )
    )
  )
)

server <- function(input, output, session) {
  db_con <- dbConnect(RSQLite::SQLite(), "data/oe_database.db")
  imported_file <- import_file_server("import")
  choose_existing_project_server("existing_project", db_con)
}

shinyApp(ui, server)