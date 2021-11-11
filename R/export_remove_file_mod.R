export_remove_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1, downloadButton(ns("export_button"), "Export")),
      column(1, offset = 1, actionButton(ns("remove_button"), "Delete"))
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
        code_frame_query <- glue::glue_sql("SELECT variable, code, label FROM code_frame
                                           WHERE project_id = {chosen_project}",
                                           chosen_project = setup_project(),
                                           .con = db_con)
        code_frame <- dbGetQuery(db_con, code_frame_query)
        
        codes_query <- glue::glue_sql("SELECT verbatim_id, code FROM codes WHERE project_id = {chosen_project}",
                                      chosen_project = setup_project(),
                                      .con = db_con)
        codes <- dbGetQuery(db_con, codes_query)
        codes <- codes %>% 
          group_by(verbatim_id) %>% 
          summarise(code = stri_c(code, collapse = " "))
        
        verbatims_query <- glue::glue_sql("SELECT participant_id, verbatim_id, variable, value
                                          FROM verbatims WHERE project_id = {chosen_project}",
                                          chosen_project = setup_project(),
                                          .con = db_con)
        verbatims <- dbGetQuery(db_con, verbatims_query)
        verbatims <- left_join(verbatims, codes, by = "verbatim_id")
        verbatims <- verbatims %>% 
          select(id = participant_id, variable, value, code)
        writexl::write_xlsx(list(verbatims = verbatims, code_frame = code_frame), file, format_headers = FALSE)
      }
    )
    
    observeEvent(input$remove_button, {
      query <- glue::glue_sql("DELETE FROM projects WHERE id = {project_id}",
                              project_id = setup_project(), .con = db_con)
      dbSendStatement(db_con, query)
    })
  })
}