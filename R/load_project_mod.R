load_project_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    actionButton(ns("load"), "Load project")
  )
}

load_project_server <- function(id, db_con, imported, chosen_project) {
  moduleServer(id, function(input, output, session) {
    
    setup_project <- eventReactive(input$load, {
      if (isTruthy(imported$load_file()) || isTruthy(chosen_project())) {
        if (isTruthy(chosen_project())) {
          query_take_id <- glue::glue_sql("SELECT id FROM projects WHERE name = {project_name}",
                                  project_name = chosen_project(), .con = db_con)
          dbGetQuery(db_con, query_take_id) %>% 
            pull(id)
        } else {
          query_write_project_name <- glue::glue_sql("INSERT INTO projects (name) VALUES ({file_name})",
                                                     file_name = imported$load_file()$file_name, .con = db_con)
          dbSendStatement(db_con, query_write_project_name)
          # write data to table as well!
          
          query_take_id <- glue::glue_sql("SELECT id FROM projects WHERE name = {file_name}",
                                          file_name = imported$load_file()$file_name, .con = db_con)
          dbGetQuery(db_con, query_take_id) %>% 
            pull(id)
        }
      }
    })
    
    observeEvent(input$load, {
      req(imported$file_input())
      if (is.null(imported$load_file()) && !isTruthy(chosen_project())) {
        shinyalert(text = "File must contain only three columns: id, variable, value.",
                   type = "error")
      }
    })
    
    observeEvent(setup_project(), {
      shinyalert(text = "Go to the 'Code' tab",
                 type = "success")
    })
    
    observe(setup_project())
    
    return(setup_project)
  })
}