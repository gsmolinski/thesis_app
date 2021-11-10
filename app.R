library(shiny)
library(shinyalert)
library(DBI)
library(stringi)
library(dplyr)

ui <- fluidPage(
  useShinyalert(),
  titlePanel("Code OE"),
  actionButton("debug", "Debug"), # remove later
  tabsetPanel(
    tabPanel(title = "Import",
             br(),
             fluidRow(
               column(3, import_file_ui("import")),
               column(3, offset = 1, choose_existing_project_ui("existing_project"))
             ),
             br(),
             fluidRow(
               column(2, load_project_ui("load"))
             )
    ),
    tabPanel(title = "Code",
             br()
             ),
    tabPanel(title = "Export",
             br(),
             export_remove_ui("export")
             )
  )
)

server <- function(input, output, session) {
  db_con <- dbConnect(RSQLite::SQLite(), "data/oe_database.db")
  
  imported <- import_file_server("import")
  chosen_project <- choose_existing_project_server("existing_project", db_con)
  loaded_project <- load_project_server("load", db_con,
                      imported = imported,
                      chosen_project = chosen_project)
  export_remove_server("export", db_con,
                       setup_project = loaded_project$setup_project,
                       chosen_project = chosen_project)
  
  observeEvent(input$debug, {
    browser()
  }) # remove later
  
  onStop(function() {
    dbDisconnect(db_con)
  })
}

runApp(shinyApp(ui, server))