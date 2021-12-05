wordcloud_freq_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      tabPanel("Wordcloud",
               wordcloud2Output(ns("wordcloud"))
               ),
      tabPanel("Table",
               DTOutput(ns("freq_table")),
               br(),
               actionButton(ns("stopword"), "Mark as stopword"),
               )
    )
  )
}

wordcloud_freq_server <- function(id, db_con, setup_project, chosen_variable, load) {
  moduleServer(id, function(input, output, session) {
    
    words <- reactiveVal()
    
    observeEvent(load(), {
      query <- glue::glue_sql("SELECT value FROM verbatims WHERE project_id = {setup_project}
                              AND variable IN ({chosen_variable*});",
                              setup_project = setup_project(),
                              chosen_variable = chosen_variable(),
                              .con = db_con)
      table <- dbGetQuery(db_con, query)
      table <- table %>% 
        mutate(word = stri_extract_all_words(value)) %>% 
        select(word) %>% 
        unnest(word) %>% 
        mutate(word = stri_trans_tolower(word)) %>% 
        count(word, sort = TRUE, name = "freq")
      words(table)
    })
    
    output$wordcloud <- renderWordcloud2({
      wordcloud2(head(req(words()), 100), size = 0.5)
    })
    
    output$freq_table <- renderDT({
      datatable(req(words()), rownames = FALSE)
    })
    
    observeEvent(input$stopword, {
      req(words())
      row <- req(input$freq_table_rows_selected)
      words(words()[-as.numeric(row), ])
    })
    
    return(reactive(gsub(":.*", "", input$wordcloud_clicked_word)))
    
  })
}