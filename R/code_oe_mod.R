code_oe_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("table_code_oe"))
  )
  
}

code_oe_server <- function(id, db_con, setup_project, chosen_variable, load, clicked_word, show_all,
                           code_frame) {
  moduleServer(id, function(input, output, session) {
    
    verbatims <- reactiveVal()
    
    proxy <- dataTableProxy("table_code_oe")
    
    pull_data <- eventReactive(load(), {
      req(chosen_variable())
      codes_query <- glue::glue_sql("SELECT verbatim_id, code FROM codes WHERE project_id = {chosen_project} AND
                                    variable IN ({variables*})",
                                    chosen_project = setup_project(),
                                    variables = chosen_variable(),
                                    .con = db_con)
      codes <- dbGetQuery(db_con, codes_query)
      codes <- codes %>% 
        group_by(verbatim_id) %>% 
        summarise(code = stri_c(unique(code), collapse = " "))
      
      verbatims_query <- glue::glue_sql("SELECT verbatim_id, value
                                          FROM verbatims WHERE project_id = {chosen_project} AND
                                        variable IN ({variables*})",
                                        chosen_project = setup_project(),
                                        variables = chosen_variable(),
                                        .con = db_con)
      verbatims <- dbGetQuery(db_con, verbatims_query)
      verbatims <- left_join(verbatims, codes, by = "verbatim_id")
      verbatims <- verbatims %>% 
        select(id = verbatim_id, value, code)
      verbatims
    })
    
    observe({
      verbatims(pull_data())
    })
    
    output$table_code_oe <- renderDT({
      if (isTruthy(clicked_word())) {
        ver <- verbatims() %>% 
          arrange(desc(stri_detect_fixed(value, clicked_word(), case_insensitive = TRUE)))
      } else {
        ver <- verbatims()
      }
      verbatims(ver)
      datatable(ver, editable = list(target = "cell", disable = list(columns = c(0, 1))), rownames = FALSE)
    })
    
    observeEvent(input$table_code_oe_cell_edit, {
      row <- input$table_code_oe_cell_edit$row
      col <- input$table_code_oe_cell_edit$col
      val <- input$table_code_oe_cell_edit$value
      val <- unique(stri_trim_both(unlist(stri_split_boundaries(stri_replace_all_regex(val, "[^0-9\\s]", replacement = "")))))
      if (all(val %in% code_frame()$code)) {
        val_pasted <- paste0(val, collapse = " ")
        info <- data.frame(row = row,
                           col = col,
                           value = val_pasted)
        verbatims(editData(verbatims(), info, proxy, rownames = FALSE))
        query <- glue::glue_sql("SELECT * FROM codes WHERE project_id = {project} AND
                                verbatim_id = {id}",
                                project = setup_project(),
                                id = verbatims()$id[row],
                                .con = db_con)
        edited_id_db <- dbGetQuery(db_con, query)
        query <- glue::glue_sql("SELECT variable FROM verbatims WHERE project_id = {project} AND
                                verbatim_id = {id}",
                                project = setup_project(),
                                id = verbatims()$id[row],
                                .con = db_con)
        variable <- dbGetQuery(db_con, query)$variable
        code_not_saved_yet <- val[!val %in% edited_id_db$code]
        if (length(code_not_saved_yet) > 0) {
          table <- data.frame(project_id = rep(setup_project(), length(code_not_saved_yet)),
                              variable = rep(variable, length(code_not_saved_yet)),
                              verbatim_id = rep(verbatims()$id[row], length(code_not_saved_yet)),
                              code = code_not_saved_yet)
          dbAppendTable(db_con, "codes", table)
        }
        code_removed <- edited_id_db$code[!edited_id_db$code %in% val]
        
        if (length(code_removed) > 0) {
          query <- glue::glue_sql("DELETE FROM codes WHERE project_id = {project}
                                  AND verbatim_id = {id} AND code IN ({code_removed*})",
                                  project = setup_project(),
                                  id = verbatims()$id[row],
                                  code_removed = code_removed,
                                  .con = db_con)
          dbSendStatement(db_con, query)
        }
        
      } else {
        shinyalert(text = "At least one code not present in code frame.", type = "error")
      }
    })
  })
}