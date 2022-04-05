##########################
#### Reactive Events ####
##########################

#'@description observe the file upload event
#'@param session variable from server.R
#'@param rv reactive values from server.R
#'@return observeEvent
observe_upload <- function(session, rv){
  # handles input data_file
  
  observeEvent(session$input$data_file, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Loading data...", value = 0)
    
    cat(file=stderr(), "loading file: ", session$input$data_file$datapath, "\n")
    
    # referring to function to service data_processing.R
    # concatenate if rv$data exists
    if(is.null(rv$data)){
      rv$data <- read_data_file(session$input$data_file$datapath) %>%
        drop_na(year)
    } else{
      rv$data <- read_data_file(session$input$data_file$datapath, df = rv$data) %>%
        drop_na(year)
    }
  })
}


#'@description observe the file upload event
#'@param session variable from server.R
#'@param rv reactive values from server.R
#'@return observeEvent
observe_filter_update <- function(session, rv){
  # update filtered data frame
  observeEvent(c(session$input$publication_types,
                 session$input$research_areas,
                 session$input$publications,
                 session$input$publishers,
                 session$input$year,
                 session$input$search_keypressed)
               ,{
                 if(isTruthy(rv$data)) {
                   df <- rv$data
                   
                   if(isTruthy(session$input$publication_types)){
                     df <- df %>% filter(publication_type %in% session$input$publication_types)
                   }
                   
                   if(isTruthy(session$input$research_areas)){
                     df <- df %>% filter(research_area %in% session$input$research_areas)
                   }
                   
                   
                   if(isTruthy(session$input$publications)){
                     df <- df %>% filter(publication %in% session$input$publications)
                   }
                   
                   if(isTruthy(session$input$publishers)){
                     df <- df %>% filter(publisher %in% session$input$publishers)
                   }
                   
                   if(isTruthy(session$input$year)){
                     df <- df %>% filter(year>=session$input$year[1] & year<=session$input$year[2])
                   }
                   
                   if(isTruthy(session$input$search)){
                     if(isTruthy(session$input$chk_search_areas)){
                        df_search <- df %>% simple_search(session$input$search, c("publication"), 
                                                          search_columns = session$input$chk_search_areas)
                     } else {
                        df_search <- df %>% simple_search(session$input$search, c("publication"))
                     }
                     if(!isTruthy(df_search)){
                       showNotification("Advanced search did not return any result, using filter results only.")
                     } else {
                       df <- df_search
                     }
                   }
                   
                   if(isTruthy(df))
                    rv$filtered_data <- df
                 }
               }, 
               ignoreNULL = F)
}

