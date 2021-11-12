code_frame_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, DTOutput(ns("code_frame_table")))
    ),
    br(),
    fluidRow(
      column(1, actionButton(ns("add"), "Add")),
      column(1, actionButton(ns("delete"), "Delete")),
      column(1, actionButton(ns("save"), "Save"))
    ),
    br()
  )
}


code_frame_tab_server <- function(id, db_con, setup_project, chosen_variable, load) {
  moduleServer(id, function(input, output, session) {
    
    code_frame <- reactiveVal()
    
    proxy <- dataTableProxy("code_frame_table")
    
    pull_data <- reactive({
      query <- glue::glue_sql("SELECT code, label FROM code_frame
                              WHERE project_id = {chosen_project}
                              AND variable IN ({chosen_variable*})",
                              chosen_project = setup_project(),
                              chosen_variable = chosen_variable(),
                              .con = db_con)
      code_frame <- dbGetQuery(db_con, query)
      code_frame
    })
    
    observeEvent(load(), {
      code_frame(pull_data())
    })
    
    output$code_frame_table <- renderDT(
      code_frame(), editable = list(target = "cell", area = 2), rownames = FALSE
    )
    
    observeEvent(input$code_frame_table_cell_edit, {
      code_frame(editData(code_frame(), input$code_frame_table_cell_edit, proxy, rownames = FALSE))
    })
    
    observeEvent(input$add, {
      frame <- code_frame()
      frame <- frame %>% 
        add_row(code = numeric(1), label = character(1))
      code_frame(frame)
    })
    
    observeEvent(input$delete, {
      row <- req(input$code_frame_table_rows_selected)
      frame <- code_frame()
      frame <- frame[-as.numeric(row), ]
      code_frame(frame)
    })
    
    observeEvent(input$save, {
      if (any(duplicated(code_frame()))) {
        shinyalert(text = "Codes with labels cannot be duplicated", type = "error")
      } else {
        # write data, perhaps we should pull out all data, hide in datatable and now just append it back?
        # but before this, remove all data for chosen variables and project?
      }
    })
  })
}