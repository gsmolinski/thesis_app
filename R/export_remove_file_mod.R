export_remove_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(1, downloadButton(ns("export_button"), "Export")),
      column(1, offset = 1, actionButton(ns("remove_button"), "Remove"))
    )
  )
}

export_remove_server <- function(id, db_con, setup_project, chosen_project) {
  moduleServer(id, function(input, output, session) {
    
    output$export_button <- downloadHandler(
      filename = function() {
        paste0("Coded ", chosen_project(), ".xlsx")
      },
      content = function(file) {
        # write function to download data when code tab will be ready
      }
    )
    
    observeEvent(input$remove_button, {
      query <- glue::glue_sql("DELETE FROM projects WHERE id = {project_id}",
                              project_id = setup_project(), .con = db_con)
      dbSendStatement(db_con, query)
    })
  })
}