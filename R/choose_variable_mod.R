choose_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectizeInput(ns("choose_variable"), label = NULL, choices = NULL,
                               multiple = TRUE,
                               options = list(onInitialize = I('function() { this.setValue(""); }'),
                                              placeholder = "Choose variable(s)..."))),
      column(1, actionButton("load_variable", "Load"))
    )
  )
}

choose_variable_server <- function(id, db_con, setup_project, input_load) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input_load(), {
      query <- glue::glue_sql("SELECT variable FROM verbatims WHERE project_id = {project}",
                              project = setup_project(), .con = db_con)
      variables <- pull(dbGetQuery(db_con, query), variable)
      updateSelectizeInput(session = session, "choose_variable", choices = variables,
                           server = TRUE)
    })
    
    return(reactive(input$load_variable))
  })
}