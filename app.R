library(shiny)
library(shinyalert)
library(DBI)
library(stringi)
library(dplyr)
library(tibble)
library(DT)
library(purrr)
library(wordcloud2)
library(tidyr)
library(bslib)

ui <- fluidPage(
  useShinyalert(),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  #actionButton("debug", "Debug"), # remove later
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
             br(),
             choose_variable_ui("code_variable"),
             tabsetPanel(
               tabPanel(title = "Verbatim",
                        br(),
                        fluidRow(
                          column(6, code_oe_ui("code_oe")),
                          column(6, wordcloud_freq_ui("wordcloud")),
                          br()
                        )
               ),
               tabPanel(title = "Code frame",
                        br(),
                        code_frame_tab_ui("code_frame")
               )
             )
             ),
    tabPanel(title = "Export",
             br(),
             export_remove_ui("export")
             )
  )
)

server <- function(input, output, session) {
  db_con <- dbConnect(RSQLite::SQLite(), "data/oe_database.db")
  dbSendStatement(db_con, "PRAGMA foreign_keys = ON")
  
  imported <- import_file_server("import")
  chosen_project <- choose_existing_project_server("existing_project", db_con)
  loaded_project <- load_project_server("load", db_con,
                      imported = imported,
                      chosen_project = chosen_project)
  chosen_variable <- choose_variable_server("code_variable", db_con,
                                            setup_project = loaded_project$setup_project,
                                            input_load = loaded_project$input_load)
  code_frame <- code_frame_tab_server("code_frame", db_con,
                        setup_project = loaded_project$setup_project,
                        chosen_variable = chosen_variable$chosen,
                        load = chosen_variable$load)
  export_remove_server("export", db_con,
                       setup_project = loaded_project$setup_project)
  
  clicked_word <- wordcloud_freq_server("wordcloud", db_con,
                        setup_project = loaded_project$setup_project,
                        chosen_variable = chosen_variable$chosen,
                        load = chosen_variable$load)
  
  code_oe_server("code_oe", db_con,
                 setup_project = loaded_project$setup_project,
                 chosen_variable = chosen_variable$chosen,
                 load = chosen_variable$load,
                 clicked_word = clicked_word,
                 code_frame = code_frame)
  
  # observeEvent(input$debug, {
  #   browser()
  # }) # remove later
  
  onStop(function() {
    dbDisconnect(db_con)
  })
}

runApp(shinyApp(ui, server))