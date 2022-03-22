library(shiny)
library(highcharter)
library(treemap)
library(viridisLite)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  rv = reactiveValues()
  
  raw_data <-mod_upload_file_server("upload_file")
  filtered_data <- mod_data_filter_server("data_filter", data = raw_data)
  mod_data_summary_tab_server('summary', data = filtered_data)
  #word_cloud <- module_wordcloud_server("wordcloud", data = filtered_data)
  mod_visnetwork_server("network", data = filtered_data)
  
  output$infoboxes <- renderUI({
    req(filtered_data())
    gen_information_boxes(filtered_data())
  })
  
  output$data_table <- renderDataTable({
    req(filtered_data())
    cat(file = stderr(), "server.R/data_table: render data_table output for raw data.", "\n")
    filtered_data() %>% 
      mutate(title = paste0("<a href = 'https://www.doi.org/", doi, "'>", title, "</a>")) %>%
      select(year, publication_type, title, citations, raw_text, clean_text, formated_keywords) %>% arrange(year)
    
  }, escape = FALSE,  options = list(dom = 't', pageLength = 10, info = FALSE))
  
}
