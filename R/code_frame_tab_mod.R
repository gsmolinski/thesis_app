code_frame_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, DTOutput(ns("code_frame_table")))
    ),
    br(),
    fluidRow(
      column(1, actionButton(ns("add"), "Add")),
      column(1, actionButton(ns("delete"), "Delete"))
    ),
    br()
  )
}


code_frame_tab_server <- function(id, db_con, setup_project, chosen_variable, load) {
  moduleServer(id, function(input, output, session) {
    
    code_frame <- reactiveVal()
    
    proxy <- dataTableProxy("code_frame_table")
    
    pull_data <- eventReactive(load(), {
      req(chosen_variable())
      query_pull <- glue::glue_sql("SELECT variable, code, label FROM code_frame
                              WHERE project_id = {chosen_project}
                              AND variable IN ({chosen_variable*})",
                              chosen_project = setup_project(),
                              chosen_variable = chosen_variable(),
                              .con = db_con)
      code_frame <- dbGetQuery(db_con, query_pull)
      if (nrow(code_frame) > 0) {
        if (!all(chosen_variable() %in% code_frame$variable)) {
          code_frame <- rbind(code_frame, data.frame(variable = chosen_variable()[!chosen_variable() %in% code_frame$variable],
                                       code = NA,
                                       label = NA))
        } # when additional chosen variables don't have any code, then it is necessary
        # otherwise they would be missed below
        code_frame_split <- split(code_frame[, -1], code_frame$variable)
        code_frame_split <- map(code_frame_split, filter, !is.na(code)) # we just want empty data.frame for missed variable
        code_frame <- reduce(code_frame_split, union)
        codes_duplicated <- unique(code_frame$code[duplicated(code_frame$code)])
        if (length(codes_duplicated) > 0) {
          code_frame <- code_frame %>% 
            group_by(code) %>% 
            summarise(label = stri_c(label, collapse = " | "))
          for (i in codes_duplicated) {
            new_label <- code_frame %>% 
              filter(code == i) %>% 
              pull(label)
            query_update <- glue::glue_sql("UPDATE code_frame
                                         SET label = {new_label}
                                         WHERE code = {code_duplicated}",
                                         new_label = new_label,
                                         code_duplicated = i,
                                         .con = db_con)
            dbSendStatement(db_con, query_update)
          }
        }
        
        code_frame_split <- map(code_frame_split, ~ anti_join(code_frame, .x, by = c("code", "label")))
        code_frame_binded <- data.table::rbindlist(code_frame_split, idcol = TRUE) %>% 
          mutate(project_id = setup_project()) %>% 
          rename(variable = .id) %>% 
          relocate(project_id)
        if (code_frame_binded[, .N] > 0) {
          dbAppendTable(db_con, "code_frame", code_frame_binded)
        }
      }
      code_frame <- code_frame %>% 
        select(code, label) # in case we had empty code_frame, but pulled variable column
      list(code_frame = code_frame, chosen_variable = chosen_variable()) # chosen variables when
      # load button was pushed (from the time when pushed!)
    })
    
    observe({
      code_frame(pull_data()$code_frame)
    })
    
    output$code_frame_table <- renderDT(
      code_frame(), editable = list(target = "cell", area = 2), rownames = FALSE
    )
    
    observeEvent(input$code_frame_table_cell_edit, {
      row <- input$code_frame_table_cell_edit$row
      col <- input$code_frame_table_cell_edit$col + 1 # because we don't have row names
      val <- input$code_frame_table_cell_edit$value
      column_name <- names(code_frame())[[col]]
      code_changed <- code_frame()[row, ] %>% 
        pull(code)
      if (column_name == "code" && code_changed == 0 && any(val == code_frame()$code)) {
        shinyalert(text = "This code already exists.", type = "error") # only for new codes, i.e. when changing 0
      } else {
        code_frame(editData(code_frame(), input$code_frame_table_cell_edit, proxy, rownames = FALSE))
        # idea is that on database side changes are based on code, not row, different than on datatable.
        # It means that user can see 2x code 5 with different labels - and which label really exists
        # in the database? This one which was change as last - and that means user needs to remember
        # what is a true label or immediately remove the old label (row).
        query <- glue::glue_sql("UPDATE code_frame
                              SET {column} = {value}
                              WHERE code = {code_changed}",
                              column = column_name,
                              value = val,
                              code_changed = code_changed,
                              .con = db_con)
        dbSendStatement(db_con, query)
      }
    })
    
    observeEvent(input$add, {
      req(code_frame())
      frame <- code_frame()
      if (!any(frame$code == 0)) {
        frame <- frame %>% 
          add_row(code = numeric(1), label = character(1))
        
        code_frame(frame)
        variables <- pull_data()$chosen_variable
        table <- data.frame(variable = variables) %>% 
          mutate(project_id = setup_project(),
                 code = 0,
                 label = "") %>% 
          select(project_id, variable, code, label)
        dbAppendTable(db_con, "code_frame", table)
      } else {
        shinyalert(text = "Newly added code already exists. Change the code number first.",
                   type = "error")
      }
    })
    
    observeEvent(input$delete, {
      req(code_frame())
      row <- req(input$code_frame_table_rows_selected)
      frame <- code_frame()
      code <- pull(frame[as.numeric(row), ], code)
      code_is_duplicated <- any(frame$code[-row] == code) # means user only recoded, not removed really
      frame <- frame[-as.numeric(row), ]
      code_frame(frame)
      if (!code_is_duplicated) { # so do not touch database because of ON DELETE CASCADE!
        query <- glue::glue_sql("DELETE FROM code_frame WHERE project_id = {project}
                              AND variable IN ({variables*}) AND code = {chosen_code}",
                              project = setup_project(),
                              variables = pull_data()$chosen_variable,
                              chosen_code = code,
                              .con = db_con)
        dbSendStatement(db_con, query)
      }
    })
    
    return(code_frame)
    
  })
}