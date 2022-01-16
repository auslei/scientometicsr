#this file supports server.R
get_display_data <- function(df, rv){
  if(isTruthy(rv$data)) {
    df <- rv$data
    
    if(isTruthy(input$publication_types)){
      df <- df %>% filter(publication_type %in% input$publication_types)
    }
    
    if(isTruthy(input$research_areas)){
      df <- df %>% filter(research_area %in% input$research_areas)
    }
    
    
    if(isTruthy(input$publications)){
      df <- df %>% filter(publication %in% input$publications)
    }
    
    if(isTruthy(input$publishers)){
      df <- df %>% filter(publisher %in% input$publishers)
    }
    
    if(isTruthy(input$year)){
      df <- df %>% filter(year>=input$year[1] & year<=input$year[2])
    }
    
    df
  }
}