import_file_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fileInput(ns("imported_file"), "Choose new file (project)",
              accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  )
}

import_file_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive({
      file <- input$imported_file
      req(file)
     file <- readxl::read_xlsx(file$datapath, guess_max = 1000000)
     
     if (!all(names(file) == c("id", "variable", "value")) || ncol(file) != 3) {
       shinyalert(text = "File must contain only three columns: id, variable, value.")
     } else {
       file
     }
    })
    
  })
}