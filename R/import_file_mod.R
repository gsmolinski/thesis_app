import_file_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fileInput(ns("imported_file"), "Choose new file (project)",
              accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    
  )
}

import_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    load_file <- reactive({
      file <- input$imported_file
      if (isTruthy(file)) {
        file_data <- readxl::read_xlsx(file$datapath, guess_max = 1000000)
        
        if (!all(names(file_data) == c("id", "variable", "value")) || ncol(file_data) != 3) {
          FALSE
        } else {
          list(file = file_data, file_name = stri_sub(file$name, 1, -6))
        }
      } else {
        FALSE
      }
    })
    
    return(list(load_file = load_file, file_input = reactive(input$imported_file)))
    
  })
}
