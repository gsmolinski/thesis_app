choose_existing_project_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("choose_project"), "Choose existing project", choices = NULL,
                   options = list(onInitialize = I('function() { this.setValue(""); }')))
  )
}

choose_existing_project_server <- function(id, db_con) {
  moduleServer(id, function(input, output, session) {
    # update only when refreshing the whole app
    updateSelectizeInput(session = session, "choose_project",
                         choices = pull(dbGetQuery(db_con, "SELECT name FROM projects ORDER BY name"), name))
    
    return(reactive(input$choose_project))
  })
}